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
import qualified Matrix as M (addMatrices, empty, fromList, Matrix, multiplyMatrices, transpose)
import qualified Optimizer as O (optimize, Optimizer)
import qualified System.Random as SR (randomRs, split, StdGen)


{----- TYPES -----}

data ForwardResult = ForwardResult {
    layerInputs  :: [M.Matrix],  -- ^ Values of the input of each activation fonction per layer
    layerOutputs :: [M.Matrix]   -- ^ Values of the output of each activation fonction per layer, including the input layer
} deriving (Eq, Show)

data Network = Network {
    activations :: [A.Activation],
    biases      :: [M.Matrix],
    weights     :: [M.Matrix]
} deriving (Eq, Show)


{----- HIDDEN METHODS -----}

backwardStep :: M.Matrix                                          -- ^ Input of the layer
                -> A.Activation                                   -- ^ Activation function
                -> M.Matrix                                       -- ^ Weights of current layer
                -> M.Matrix                                       -- ^ Ouput of the previous layer
                -> M.Matrix                                       -- ^ Current propagation
                -> Either String ((M.Matrix, M.Matrix), M.Matrix) -- ^ Gradients of the current layer, with the propagation vector
backwardStep input activation weights output propagation = do
    jacobian <- A.backward activation input
    let buffer = M.multiplyMatrices propagation jacobian
    nextPropagation <- buffer >>= flip M.multiplyMatrices weights
    nextWeightsGradient <- M.transpose <$> (buffer >>= M.multiplyMatrices output)
    nextBiasesGradient <- M.transpose <$> buffer
    return ((nextBiasesGradient, nextWeightsGradient), nextPropagation)

forwardStep :: M.Matrix                                -- ^ Input of the network
               -> Network                              -- ^ Current network
               -> Either String [(M.Matrix, M.Matrix)] -- ^ Inputs and outputs of the current layer.
forwardStep input (Network [] [] []) = Right []
forwardStep input (Network (activation:activations) (bias:biases) (weight:weights)) = do
    activationInput <- M.multiplyMatrices weight input >>= M.addMatrices bias
    activationOutput <- A.forward activation activationInput
    ((activationInput, activationOutput) :) <$> forwardStep activationOutput (Network activations biases weights)

randomStep :: [Int] -> SR.StdGen -> Either String ([M.Matrix], [M.Matrix])
randomStep [layer] _ = Right ([], [])
randomStep (firstLayer : secondLayer : nextLayers) generator = do
    let (firstGenerator, (secondGenerator, thirdGenerator)) = SR.split <$> SR.split generator
    biases <- M.fromList secondLayer 1 $ take secondLayer $ SR.randomRs (-1.0, 1.0) firstGenerator
    weights <- M.fromList secondLayer firstLayer $ take (firstLayer * secondLayer) $ SR.randomRs (-1.0, 1.0) secondGenerator
    (nextBiases, nextWeights) <- randomStep (secondLayer : nextLayers) thirdGenerator
    return (biases : nextBiases, weights : nextWeights)

trainStep :: O.Optimizer                          -- ^ Optimizer
             -> L.Loss                            -- ^ Loss function
             -> Either String (Network, [Double]) -- ^ Previous trained network and losses
             -> (M.Matrix, M.Matrix)              -- ^ Datapoint and target to train on
             -> Either String (Network, [Double]) -- ^ Trained network and list of loss values
trainStep optimizer loss accumulator (datapoint, target) = do
    (network, losses) <- accumulator
    forwardResult <- forward datapoint network
    (biasesGradients, weightsGradients) <- backward loss network target forwardResult
    newBiases <- O.optimize (biases network) biasesGradients optimizer
    newWeights <- O.optimize (weights network) weightsGradients optimizer
    lossValue <- L.forward loss (last $ layerOutputs forwardResult) target
    return (Network (activations network) newBiases newWeights, losses ++ [lossValue])


{----- EXPORTED METHODS -----}

backward :: L.Loss                                    -- ^ Loss function
            -> Network                                -- ^ Current Network
            -> M.Matrix                               -- ^ Target vector
            -> ForwardResult                          -- ^ Forward pass result
            -> Either String ([M.Matrix], [M.Matrix]) -- ^ Typle of list of gradient of biases and weights
backward loss (Network activations biases weights) target (ForwardResult layerInputs layerOutputs) = do
    let propagation = (,) (M.empty, M.empty) <$> L.backward loss (last layerOutputs) target
    backwardResults <- sequence $ init $ scanr computeBackwardStep propagation (DL.zip4 layerInputs activations weights (init layerOutputs))
    return $ unzip $ fst <$> backwardResults
  where
    computeBackwardStep (input, activation, weight, output) previous = previous >>= \(_, x) -> backwardStep input activation weight output x

forward :: M.Matrix                       -- ^ Input of the network
           -> Network                     -- ^ Current network
           -> Either String ForwardResult -- ^ Result of the forward pass
forward input network = do
    (inputs, outputs) <- unzip <$> forwardStep input network
    return ForwardResult { layerInputs = inputs, layerOutputs = input:outputs }

fromLists :: [A.Activation] -> [M.Matrix] -> [M.Matrix] -> Either String Network
fromLists activations biases weights
    | length activations /= length biases || length biases /= length weights = Left "Mismatching dimensions between layers and weights."
    -- #TODO: test matrices's dimensions consistency
    | otherwise = Right $ unsafeFromLists activations biases weights

random :: [Int] -> [A.Activation] -> SR.StdGen -> Either String Network
random layers activations generator
    | length layers - 1 /= length activations = Left "Mismatching dimensions between layers and activations."
    | otherwise = do
        (biases, weights) <- randomStep layers generator
        return $ Network activations biases weights

train :: O.Optimizer                          -- ^ Optimizer
         -> L.Loss                            -- ^ Loss function
         -> Network                           -- ^ Current Network
         -> D.Dataset                         -- ^ Dataset to train on
         -> Either String (Network, [Double]) -- ^ Trained network and list of loss values
train optimizer loss network (D.Dataset datapoints targets) =
    DL.foldl' (trainStep optimizer loss) (Right (network, [])) $ zip datapoints targets

unsafeFromLists :: [A.Activation] -> [M.Matrix] -> [M.Matrix] -> Network
unsafeFromLists = Network
