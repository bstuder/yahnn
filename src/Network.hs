{-# LANGUAGE Strict #-}

module Network
(
    backward,
    evaluateClassification,
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
import qualified Evaluator as E (ClassificationMatrix, empty, updateClassificationMatrix)
import qualified Loss as L (backward, forward, Loss)
import qualified Matrix as M (addMatrices, empty, fromList, Matrix, multiplyMatrices, transpose)
import qualified Optimizer as O (optimize, Optimizer)
import qualified System.Random as SR (randomRs, split, StdGen)


{----- TYPES -----}

data ForwardResult a = ForwardResult {
    layerInputs :: [M.Matrix a],   -- ^ Values of the input of each activation fonction per layer
    layerOutputs :: [M.Matrix a]   -- ^ Values of the output of each activation fonction per layer, including the input layer
} deriving (Eq, Show)

data Network a = Network {
    activations :: [A.Activation],
    biases :: [M.Matrix a],
    weights :: [M.Matrix a]
} deriving (Eq, Show)


{----- HIDDEN METHODS -----}

backwardStep :: RealFloat a =>
                M.Matrix a                                                  -- ^ Input of the layer
                -> A.Activation                                             -- ^ Activation function
                -> M.Matrix a                                               -- ^ Weights of current layer
                -> M.Matrix a                                               -- ^ Ouput of the previous layer
                -> M.Matrix a                                               -- ^ Current propagation
                -> Either String ((M.Matrix a, M.Matrix a), M.Matrix a)     -- ^ Gradients of the current layer, with the propagation vector
backwardStep input activation weights output propagation = do
    jacobian <- A.backward activation input
    let buffer = M.multiplyMatrices propagation jacobian
    nextPropagation <- buffer >>= flip M.multiplyMatrices weights
    nextWeightsGradient <- M.transpose <$> (buffer >>= M.multiplyMatrices output)
    nextBiasesGradient <- M.transpose <$> buffer
    return ((nextBiasesGradient, nextWeightsGradient), nextPropagation)

evaluateClassificationStep :: RealFloat a =>
                              Network a                                 -- ^ Current network
                              -> Either String E.ClassificationMatrix   -- ^ Result of the previous evaluation
                              -> (M.Matrix a, M.Matrix a)               -- ^ Datapoint and target to train on
                              -> Either String E.ClassificationMatrix   -- ^ Result of the evaluation
evaluateClassificationStep network classificationMatrix (datapoint, target) = do
    (ForwardResult _ layerOutputs) <- forward datapoint network
    E.updateClassificationMatrix (last layerOutputs) target <$> classificationMatrix

forwardStep :: RealFloat a =>
               M.Matrix a                                   -- ^ Input of the network
               -> Network a                                 -- ^ Current network
               -> Either String [(M.Matrix a, M.Matrix a)]  -- ^ Inputs and outputs of the current layer.
forwardStep input (Network [] [] []) = Right []
forwardStep input (Network (activation:activations) (bias:biases) (weight:weights)) = do
    activationInput <- M.multiplyMatrices weight input >>= M.addMatrices bias
    activationOutput <- A.forward activation activationInput
    ((activationInput, activationOutput) :) <$> forwardStep activationOutput (Network activations biases weights)

randomStep :: [Int] -> SR.StdGen -> Either String ([M.Matrix Double], [M.Matrix Double])
randomStep [layer] _ = Right ([], [])
randomStep (firstLayer : secondLayer : nextLayers) generator = do
    let (firstGenerator, (secondGenerator, thirdGenerator)) = SR.split <$> SR.split generator
    biases <- M.fromList secondLayer 1 $ take secondLayer $ SR.randomRs (-1.0, 1.0) firstGenerator
    weights <- M.fromList secondLayer firstLayer $ take (firstLayer * secondLayer) $ SR.randomRs (-1.0, 1.0) secondGenerator
    (nextBiases, nextWeights) <- randomStep (secondLayer : nextLayers) thirdGenerator
    return (biases : nextBiases, weights : nextWeights)

trainStep :: RealFloat a =>
             O.Optimizer a                      -- ^ Optimizer
             -> L.Loss                          -- ^ Loss function
             -> Either String (Network a, [a])  -- ^ Previous trained network and losses
             -> (M.Matrix a, M.Matrix a)        -- ^ Datapoint and target to train on
             -> Either String (Network a, [a])  -- ^ Trained network and list of loss values
trainStep optimizer loss accumulator (datapoint, target) = do
    (network, losses) <- accumulator
    forwardResult <- forward datapoint network
    (biasesGradients, weightsGradients) <- backward loss network target forwardResult
    newBiases <- O.optimize (biases network) biasesGradients optimizer
    newWeights <- O.optimize (weights network) weightsGradients optimizer
    lossValue <- L.forward loss (last $ layerOutputs forwardResult) target
    return (Network (activations network) newBiases newWeights, losses ++ [lossValue])


{----- EXPORTED METHODS -----}

backward :: RealFloat a =>
            L.Loss                                          -- ^ Loss function
            -> Network a                                    -- ^ Current Network
            -> M.Matrix a                                   -- ^ Target vector
            -> ForwardResult a                              -- ^ Forward pass result
            -> Either String ([M.Matrix a], [M.Matrix a])   -- ^ Typle of list of gradient of biases and weights
backward loss (Network activations biases weights) target (ForwardResult layerInputs layerOutputs) = do
    let propagation = (,) (M.empty, M.empty) <$> L.backward loss (last layerOutputs) target
    backwardResults <- sequence $ init $ scanr computeBackwardStep propagation (DL.zip4 layerInputs activations weights (init layerOutputs))
    return $ unzip $ fst <$> backwardResults
  where
    computeBackwardStep (input, activation, weight, output) previous = previous >>= \(_, x) -> backwardStep input activation weight output x

evaluateClassification :: RealFloat a =>
                          Network a                                 -- ^ Current Network
                          -> D.Dataset a                            -- ^ Dataset to evaluate on
                          -> Either String E.ClassificationMatrix   -- ^ Result of the evaluation
evaluateClassification network (D.Dataset datapoints targets) = do
    DL.foldl' (evaluateClassificationStep network) (Right $ E.empty) $ zip datapoints targets

forward :: RealFloat a =>
           M.Matrix a                           -- ^ Input of the network
           -> Network a                         -- ^ Current network
           -> Either String (ForwardResult a)   -- ^ Result of the forward pass
forward input network = do
    (inputs, outputs) <- unzip <$> forwardStep input network
    return ForwardResult { layerInputs = inputs, layerOutputs = input:outputs }

fromLists :: [A.Activation] -> [M.Matrix a] -> [M.Matrix a] -> Either String (Network a)
fromLists activations biases weights
    | length activations /= length biases || length biases /= length weights = Left "Mismatching dimensions between layers and weights."
    -- #TODO: test matrices's dimensions consistency
    | otherwise = Right $ unsafeFromLists activations biases weights

random :: [Int] -> [A.Activation] -> SR.StdGen -> Either String (Network Double)
random layers activations generator
    | length layers - 1 /= length activations = Left "Mismatching dimensions between layers and activations."
    | otherwise = do
        (biases, weights) <- randomStep layers generator
        return $ Network activations biases weights

train :: RealFloat a =>
         O.Optimizer a                      -- ^ Optimizer
         -> L.Loss                          -- ^ Loss function
         -> Network a                       -- ^ Current Network
         -> D.Dataset a                     -- ^ Dataset to train on
         -> Either String (Network a, [a])  -- ^ Trained network and list of loss values
train optimizer loss network (D.Dataset datapoints targets) =
    DL.foldl' (trainStep optimizer loss) (Right (network, [])) $ zip datapoints targets

unsafeFromLists :: [A.Activation] -> [M.Matrix a] -> [M.Matrix a] -> Network a
unsafeFromLists = Network
