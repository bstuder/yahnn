module Network where

import qualified Activation as A (Activation(..), derivate, forward)
import qualified Data.Either as E (Either(..))
import qualified Data.Vector as V (empty, fromList, Vector(..), zipWith)
import qualified Data.List as LI
import qualified Loss as L (derivate, forward, Loss(..))
import qualified Matrix as M (empty, fromLayersList, fromVectors, Matrix(..), multiplyVectorL, multiplyVectorR, transpose)
import qualified System.Random as R (StdGen(..))

data ForwardResult a = ForwardResult {
    layerInputs :: [V.Vector a],
    layerOutputs :: [V.Vector a]
} deriving (Eq, Show)

data Network a = Network {
    activations :: [A.Activation],
    weights :: [M.Matrix a]
} deriving (Eq, Show)

fromList :: [Int] -> [A.Activation] -> R.StdGen -> E.Either String (Network Double)
fromList layers activations generator
    | length layers - 1 /= length activations = E.Left "Mismatching dimensions between layers and activations."
    | otherwise = E.Right $ Network activations (M.fromLayersList layers generator)

{-- Forward propagation --}

forward :: (RealFloat a) => V.Vector a -> Network a -> E.Either String (ForwardResult a)
forward input network  = do
    (inputs, outputs) <- unzip <$> forwardStep input network
    return ForwardResult { layerInputs = inputs, layerOutputs = input:outputs } -- the original input is the "zero" output

forwardStep :: (RealFloat a) => V.Vector a -> Network a -> E.Either String [(V.Vector a, V.Vector a)]
forwardStep input (Network [] []) = E.Right []
forwardStep input (Network (activation:activations) (weight:weights)) = do
    activationInput <- M.multiplyVectorR weight input
    let output = A.forward activation activationInput
    ((activationInput, output) :) <$> forwardStep output (Network activations weights)

{-- Backward propagation --}

backward ::
    (RealFloat a) =>
    Network a ->                -- ^ Current Network
    ForwardResult a ->          -- ^ Forward pass result
    V.Vector a ->               -- ^ Target vector
    L.Loss ->                   -- ^ Loss function
    Either String [M.Matrix a]  -- ^ List of W_i updates with the propagation vector
backward (Network activations weights) (ForwardResult layerInputs layerOutputs) target loss =
    fmap (fst <$>) eitherResult
    where
        eitherResult = sequence $ init $ scanr computeBackwardStep propagation (LI.zip4 layerInputs activations weights (init layerOutputs))
        propagation = E.Right (M.empty, L.derivate loss (last layerOutputs) target)
        computeBackwardStep (input, activation, weight, output) previous = previous >>= \(_, x) -> backwardStep input activation weight output x

backwardStep ::
    RealFloat a =>
    V.Vector a                                  -- ^ Input of the layer
    -> A.Activation                             -- ^ Activation function
    -> M.Matrix a                               -- ^ Weights of current layer
    -> V.Vector a                               -- ^ Ouput of the above layer
    -> V.Vector a                               -- ^ Current propagation
    -> Either String (M.Matrix a, V.Vector a)   -- ^ W_i updates with the propagation vector
backwardStep input activation weights output propagation =
    (,) <$> E.Right nextGradient <*> nextPropagation
    where
        jacobianDiagonal = A.derivate activation input
        nextPropagation = (V.zipWith (*) propagation jacobianDiagonal) `M.multiplyVectorL` weights
        nextGradient = M.transpose $ output `M.fromVectors` V.zipWith (*) propagation jacobianDiagonal