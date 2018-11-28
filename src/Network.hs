module Network
(
    backward,
    forward,
    ForwardResult(..),
    fromLists,
    Network,
    random,
    train,
    unsafeFromLists
) where

import qualified Activation as A (Activation(..), derivate, forward)
import qualified Data.Vector as DV (Vector(..), zipWith)
import qualified Data.List as DL (zip4)
import qualified Dataset as D (Dataset(..))
import qualified Loss as L (derivate, forward, Loss(..))
import qualified Matrix as M (empty, fromLayers, fromVectors, Matrix, multiplyVectorL, multiplyVectorR, transpose)
import qualified Optimizer as O (optimize, Optimizer(..))
import qualified System.Random as SR (StdGen(..))

data ForwardResult a = ForwardResult {
    layerInputs :: [DV.Vector a],   -- ^ Values of the input of each activation fonction per layer
    layerOutputs :: [DV.Vector a]   -- ^ Values of the output of each activation fonction per layer, including the input layer
} deriving (Eq, Show)

data Network a = Network {
    activations :: [A.Activation],
    weights :: [M.Matrix a]
} deriving (Eq, Show)

backward :: (RealFloat a) =>
         Network a                              -- ^ Current Network
         -> ForwardResult a                     -- ^ Forward pass result
         -> DV.Vector a                         -- ^ Target vector
         -> L.Loss                              -- ^ Loss function
         -> Either String [M.Matrix a]          -- ^ List of gradient matrices
backward (Network activations weights) (ForwardResult layerInputs layerOutputs) target loss =
    fmap (fst <$>) eitherResult
    where
        eitherResult = sequence $ init $ scanr computeBackwardStep propagation (DL.zip4 layerInputs activations weights (init layerOutputs))
        propagation = Right (M.empty, L.derivate loss (last layerOutputs) target)
        computeBackwardStep (input, activation, weight, output) previous = previous >>= \(_, x) -> backwardStep input activation weight output x

backwardStep :: RealFloat a =>
                DV.Vector a                                -- ^ Input of the layer
                -> A.Activation                            -- ^ Activation function
                -> M.Matrix a                              -- ^ Weights of current layer
                -> DV.Vector a                             -- ^ Ouput of the above layer
                -> DV.Vector a                             -- ^ Current propagation
                -> Either String (M.Matrix a, DV.Vector a) -- ^ Gradients of the current layer, with the propagation vector
backwardStep input activation weights output propagation =
    (,) <$> Right nextGradient <*> nextPropagation
    where
        jacobianDiagonal = A.derivate activation input
        nextPropagation = (DV.zipWith (*) propagation jacobianDiagonal) `M.multiplyVectorL` weights
        nextGradient = M.transpose $ output `M.fromVectors` DV.zipWith (*) propagation jacobianDiagonal

forward :: (RealFloat a) =>
           DV.Vector a                          -- ^ Input of the network
           -> Network a                         -- ^ Current network
           -> Either String (ForwardResult a)   -- ^ Result of the forward pass
forward input network  = do
    (inputs, outputs) <- unzip <$> forwardStep input network
    return ForwardResult { layerInputs = inputs, layerOutputs = input:outputs }

forwardStep :: (RealFloat a) =>
               DV.Vector a
               -> Network a
               -> Either String [(DV.Vector a, DV.Vector a)]    -- ^ Inputs and outputs of the current layer.
forwardStep input (Network [] []) = Right []
forwardStep input (Network (activation:activations) (weight:weights)) = do
    activationInput <- M.multiplyVectorR weight input
    let output = A.forward activation activationInput
    ((activationInput, output) :) <$> forwardStep output (Network activations weights)

fromLists :: [A.Activation] -> [M.Matrix a] -> Either String (Network a)
fromLists activations weights
    | length activations /= length weights = Left "Mismatching dimensions between layers and weights."
    -- #TODO: test matrices's dimensions consistency
    | otherwise = Right $ unsafeFromLists activations weights

random :: [Int] -> [A.Activation] -> SR.StdGen -> Either String (Network Double)
random layers activations generator
    | length layers - 1 /= length activations = Left "Mismatching dimensions between layers and activations."
    | otherwise = Right $ Network activations (M.fromLayers layers generator)

train :: (RealFloat a) =>
         O.Optimizer a                      -- ^ Optimizer
         -> L.Loss                          -- ^ Loss function
         -> D.Dataset a                     -- ^ Dataset to train on
         -> Network a                       -- ^ Current Network
         -> Either String (Network a, [a])  -- ^ Trained network and list of loss values
train _ _ (D.Dataset [] _) network = Right (network, [])
train _ _ (D.Dataset _ []) network = Right (network, [])
train optimizer loss (D.Dataset (datapoint:datapoints) (target:targets)) network = do
    forwardResult <- forward datapoint network
    gradients <- backward network forwardResult target loss
    newNetwork <- Network (activations network) <$> O.optimize (weights network) gradients optimizer
    (lastNetwork, losses) <- train optimizer loss (D.Dataset datapoints targets) newNetwork
    let lossValue = L.forward loss (last $ layerOutputs forwardResult) target
    return (lastNetwork, lossValue:losses)

unsafeFromLists :: [A.Activation] -> [M.Matrix a] -> Network a
unsafeFromLists activations weights = Network activations weights
