module Network where

import qualified Activation as A (Activation(..), derivate, forward)
import qualified Data.Vector as DV (empty, fromList, Vector(..), zipWith)
import qualified Data.List as DL (zip4)
import qualified Loss as L (derivate, forward, Loss(..))
import qualified Matrix as M (empty, fromLayersList, fromVectors, Matrix(..), multiplyVectorL, multiplyVectorR, transpose)
import qualified System.Random as SR (StdGen(..))

data ForwardResult a = ForwardResult {
    layerInputs :: [DV.Vector a],
    layerOutputs :: [DV.Vector a]
} deriving (Eq, Show)

data Network a = Network {
    activations :: [A.Activation],
    weights :: [M.Matrix a]
} deriving (Eq, Show)

fromList :: [Int] -> [A.Activation] -> SR.StdGen -> Either String (Network Double)
fromList layers activations generator
    | length layers - 1 /= length activations = Left "Mismatching dimensions between layers and activations."
    | otherwise = Right $ Network activations (M.fromLayersList layers generator)

{-- Forward propagation --}

forward :: (RealFloat a) => DV.Vector a -> Network a -> Either String (ForwardResult a)
forward input network  = do
    (inputs, outputs) <- unzip <$> forwardStep input network
    return ForwardResult { layerInputs = inputs, layerOutputs = input:outputs } -- the original input is the "zero" output

forwardStep :: (RealFloat a) => DV.Vector a -> Network a -> Either String [(DV.Vector a, DV.Vector a)]
forwardStep input (Network [] []) = Right []
forwardStep input (Network (activation:activations) (weight:weights)) = do
    activationInput <- M.multiplyVectorR weight input
    let output = A.forward activation activationInput
    ((activationInput, output) :) <$> forwardStep output (Network activations weights)

{-- Backward propagation --}

backward ::
    (RealFloat a) =>
    Network a ->                -- ^ Current Network
    ForwardResult a ->          -- ^ Forward pass result
    DV.Vector a ->               -- ^ Target vector
    L.Loss ->                   -- ^ Loss function
    Either String [M.Matrix a]  -- ^ List of W_i updates with the propagation vector
backward (Network activations weights) (ForwardResult layerInputs layerOutputs) target loss =
    fmap (fst <$>) eitherResult
    where
        eitherResult = sequence $ init $ scanr computeBackwardStep propagation (DL.zip4 layerInputs activations weights (init layerOutputs))
        propagation = Right (M.empty, L.derivate loss (last layerOutputs) target)
        computeBackwardStep (input, activation, weight, output) previous = previous >>= \(_, x) -> backwardStep input activation weight output x

backwardStep ::
    RealFloat a =>
    DV.Vector a                                  -- ^ Input of the layer
    -> A.Activation                             -- ^ Activation function
    -> M.Matrix a                               -- ^ Weights of current layer
    -> DV.Vector a                               -- ^ Ouput of the above layer
    -> DV.Vector a                               -- ^ Current propagation
    -> Either String (M.Matrix a, DV.Vector a)   -- ^ W_i updates with the propagation vector
backwardStep input activation weights output propagation =
    (,) <$> Right nextGradient <*> nextPropagation
    where
        jacobianDiagonal = A.derivate activation input
        nextPropagation = (DV.zipWith (*) propagation jacobianDiagonal) `M.multiplyVectorL` weights
        nextGradient = M.transpose $ output `M.fromVectors` DV.zipWith (*) propagation jacobianDiagonal