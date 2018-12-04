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
import qualified Data.Vector.Unboxed as DV (Vector(..), Unbox, zipWith)
import qualified Dataset as D (Dataset(..))
import qualified Loss as L (backward, forward, Loss)
import qualified Matrix as M (empty, fromLayers, Matrix, multiplyMatrices, transpose)
import qualified Optimizer as O (optimize, Optimizer)
import qualified System.Random as SR (StdGen)


{----- TYPES -----}

data ForwardResult = ForwardResult {
    layerInputs  :: [M.Matrix],  -- ^ Values of the input of each activation fonction per layer
    layerOutputs :: [M.Matrix]   -- ^ Values of the output of each activation fonction per layer, including the input layer
} deriving (Eq, Show)

data Network = Network {
    activations :: [A.Activation],
    weights     :: [M.Matrix]
} deriving (Eq, Show)


{----- HIDDEN METHODS -----}

backwardStep :: M.Matrix                              -- ^ Input of the layer
                -> A.Activation                       -- ^ Activation function
                -> M.Matrix                           -- ^ Weights of current layer
                -> M.Matrix                           -- ^ Ouput of the previous layer
                -> M.Matrix                           -- ^ Current propagation
                -> Either String (M.Matrix, M.Matrix) -- ^ Gradients of the current layer, with the propagation vector
backwardStep input activation weights output propagation = do
    jacobian <- A.backward activation input
    nextPropagation <- M.multiplyMatrices propagation jacobian >>= flip M.multiplyMatrices weights
    nextGradient <- M.transpose <$> (M.multiplyMatrices propagation jacobian >>= M.multiplyMatrices output)
    return (nextGradient, nextPropagation)

forwardStep :: M.Matrix                             -- ^ Input of the network
            -> Network                              -- ^ Current network
            -> Either String [(M.Matrix, M.Matrix)] -- ^ Inputs and outputs of the current layer.
forwardStep input (Network [] []) = Right []
forwardStep input (Network (activation:activations) (weight:weights)) = do
    activationInput <- M.multiplyMatrices weight input
    output <- A.forward activation activationInput
    ((activationInput, output) :) <$> forwardStep output (Network activations weights)

trainStep :: O.Optimizer                          -- ^ Optimizer
             -> L.Loss                            -- ^ Loss function
             -> Either String (Network, [Double]) -- ^ Previous trained network and losses
             -> (M.Matrix, M.Matrix)              -- ^ Datapoint and target to train on
             -> Either String (Network, [Double]) -- ^ Trained network and list of loss values
trainStep optimizer loss accumulator (datapoint, target) = do
    (network, losses) <- accumulator
    forwardResult <- forward datapoint network
    gradients <- backward network forwardResult target loss
    newNetwork <- Network (activations network) <$> O.optimize (weights network) gradients optimizer
    lossValue <- L.forward loss (last $ layerOutputs forwardResult) target
    Right (newNetwork, losses ++ [lossValue])


{----- EXPORTED METHODS -----}

backward :: Network                  -- ^ Current Network
         -> ForwardResult            -- ^ Forward pass result
         -> M.Matrix                 -- ^ Target vector
         -> L.Loss                   -- ^ Loss function
         -> Either String [M.Matrix] -- ^ List of gradient matrices
backward (Network activations weights) (ForwardResult layerInputs layerOutputs) target loss = do
    let propagation = (,) M.empty <$> L.backward loss (last layerOutputs) target
    backwardResults <- sequence $ init $ scanr computeBackwardStep propagation (DL.zip4 layerInputs activations weights (init layerOutputs))
    return $ fst <$> backwardResults
  where
    computeBackwardStep (input, activation, weight, output) previous = previous >>= \(_, x) -> backwardStep input activation weight output x

forward ::
           M.Matrix                       -- ^ Input of the network
           -> Network                     -- ^ Current network
           -> Either String ForwardResult -- ^ Result of the forward pass
forward input network  = do
    (inputs, outputs) <- unzip <$> forwardStep input network
    return ForwardResult { layerInputs = inputs, layerOutputs = input:outputs }

fromLists :: [A.Activation] -> [M.Matrix] -> Either String Network
fromLists activations weights
    | length activations /= length weights = Left "Mismatching dimensions between layers and weights."
    -- #TODO: test matrices's dimensions consistency
    | otherwise = Right $ unsafeFromLists activations weights

random :: [Int] -> [A.Activation] -> SR.StdGen -> Either String Network
random layers activations generator
    | length layers - 1 /= length activations = Left "Mismatching dimensions between layers and activations."
    | otherwise = Right $ Network activations (M.fromLayers layers generator)

train :: O.Optimizer                          -- ^ Optimizer
         -> L.Loss                            -- ^ Loss function
         -> Network                           -- ^ Current Network
         -> D.Dataset                         -- ^ Dataset to train on
         -> Either String (Network, [Double]) -- ^ Trained network and list of loss values
train optimizer loss network (D.Dataset datapoints targets) =
    DL.foldl' (trainStep optimizer loss) (Right (network, [])) $ zip datapoints targets

unsafeFromLists :: [A.Activation] -> [M.Matrix] -> Network
unsafeFromLists = Network
