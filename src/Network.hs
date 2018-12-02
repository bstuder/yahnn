{-# LANGUAGE Strict #-}

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

import qualified Activation as A (Activation(..), backward, forward)
import qualified Data.List as DL (foldl', zip4)
import qualified Data.Vector as DV (Vector(..), zipWith)
import qualified Dataset as D (Dataset(..))
import qualified Loss as L (backward, forward, Loss)
import qualified Matrix as M (empty, fromLayers, Matrix, multiplyMatrices, transpose)
import qualified Optimizer as O (optimize, Optimizer)
import qualified System.Random as SR (StdGen)


{----- TYPES -----}

data ForwardResult a = ForwardResult {
    layerInputs :: [M.Matrix a],   -- ^ Values of the input of each activation fonction per layer
    layerOutputs :: [M.Matrix a]   -- ^ Values of the output of each activation fonction per layer, including the input layer
} deriving (Eq, Show)

data Network a = Network {
    activations :: [A.Activation],
    weights :: [M.Matrix a]
} deriving (Eq, Show)


{----- HIDDEN METHODS -----}

backwardStep :: RealFloat a =>
                M.Matrix a                                  -- ^ Input of the layer
                -> A.Activation                             -- ^ Activation function
                -> M.Matrix a                               -- ^ Weights of current layer
                -> M.Matrix a                               -- ^ Ouput of the previous layer
                -> M.Matrix a                               -- ^ Current propagation
                -> Either String (M.Matrix a, M.Matrix a)   -- ^ Gradients of the current layer, with the propagation vector
backwardStep input activation weights output propagation = do
    jacobian <- A.backward activation input
    nextPropagation <- M.multiplyMatrices propagation jacobian >>= flip M.multiplyMatrices weights
    nextGradient <- M.transpose <$> (M.multiplyMatrices propagation jacobian >>= M.multiplyMatrices output)
    return (nextGradient, nextPropagation)

forwardStep :: RealFloat a =>
    M.Matrix a                                   -- ^ Input of the network
    -> Network a                                 -- ^ Current network
    -> Either String [(M.Matrix a, M.Matrix a)]  -- ^ Inputs and outputs of the current layer.
forwardStep input (Network [] []) = Right []
forwardStep input (Network (activation:activations) (weight:weights)) = do
    activationInput <- M.multiplyMatrices weight input
    output <- A.forward activation activationInput
    ((activationInput, output) :) <$> forwardStep output (Network activations weights)

trainStep :: RealFloat a =>
             O.Optimizer a                      -- ^ Optimizer
             -> L.Loss                          -- ^ Loss function
             -> Either String (Network a, [a])  -- ^ Previous trained network and losses
             -> (M.Matrix a, M.Matrix a)        -- ^ Datapoint and target to train on
             -> Either String (Network a, [a])  -- ^ Trained network and list of loss values
trainStep optimizer loss accumulator (datapoint, target) = do
    (network, losses) <- accumulator
    forwardResult <- forward datapoint network
    gradients <- backward network forwardResult target loss
    newNetwork <- Network (activations network) <$> O.optimize (weights network) gradients optimizer
    lossValue <- L.forward loss (last $ layerOutputs forwardResult) target
    Right (newNetwork, losses ++ [lossValue])


{----- EXPORTED METHODS -----}

backward :: RealFloat a =>
         Network a                          -- ^ Current Network
         -> ForwardResult a                 -- ^ Forward pass result
         -> M.Matrix a                      -- ^ Target vector
         -> L.Loss                          -- ^ Loss function
         -> Either String [M.Matrix a]      -- ^ List of gradient matrices
backward (Network activations weights) (ForwardResult layerInputs layerOutputs) target loss = do
    let propagation = (,) M.empty <$> L.backward loss (last layerOutputs) target
    backwardResults <- sequence $ init $ scanr computeBackwardStep propagation (DL.zip4 layerInputs activations weights (init layerOutputs))
    return $ fst <$> backwardResults
  where
    computeBackwardStep (input, activation, weight, output) previous = previous >>= \(_, x) -> backwardStep input activation weight output x

forward :: RealFloat a =>
           M.Matrix a                           -- ^ Input of the network
           -> Network a                         -- ^ Current network
           -> Either String (ForwardResult a)   -- ^ Result of the forward pass
forward input network  = do
    (inputs, outputs) <- unzip <$> forwardStep input network
    return ForwardResult { layerInputs = inputs, layerOutputs = input:outputs }

fromLists :: [A.Activation] -> [M.Matrix a] -> Either String (Network a)
fromLists activations weights
    | length activations /= length weights = Left "Mismatching dimensions between layers and weights."
    -- #TODO: test matrices's dimensions consistency
    | otherwise = Right $ unsafeFromLists activations weights

random :: [Int] -> [A.Activation] -> SR.StdGen -> Either String (Network Double)
random layers activations generator
    | length layers - 1 /= length activations = Left "Mismatching dimensions between layers and activations."
    | otherwise = Right $ Network activations (M.fromLayers layers generator)

train :: RealFloat a =>
         O.Optimizer a                      -- ^ Optimizer
         -> L.Loss                          -- ^ Loss function
         -> Network a                       -- ^ Current Network
         -> D.Dataset a                     -- ^ Dataset to train on
         -> Either String (Network a, [a])  -- ^ Trained network and list of loss values
train optimizer loss network (D.Dataset datapoints targets) =
    DL.foldl' (trainStep optimizer loss) (Right (network, [])) $ zip datapoints targets

unsafeFromLists :: [A.Activation] -> [M.Matrix a] -> Network a
unsafeFromLists = Network
