module Network where

import qualified Activation as A (Activation(..), forward, derivate)
import qualified Data.Either as E (Either(..))
import qualified Data.Vector as V (empty, fromList, Vector(..), zipWith)
import qualified Matrix as M (fromLayersList, Matrix(..), multiplyVector, multiplyMatrix)
import qualified System.Random as R (StdGen(..))

data Network a = Network {
    activations :: [A.Activation],
    weights :: [M.Matrix a]
} deriving (Eq, Show)

data ForwardResult a = ForwardResult {
    layerInputs :: [V.Vector a],
    layerOutputs :: [V.Vector a]
} deriving (Eq, Show)

forward :: (RealFloat a) => V.Vector a -> Network a -> E.Either String (ForwardResult a)
forward input network  = uncurry ForwardResult . unzip <$> forward' input network

forward' :: (RealFloat a) => V.Vector a -> Network a -> E.Either String [(V.Vector a, V.Vector a)]
forward' input (Network [] []) = E.Right []
forward' input (Network (activation:activations) (weight:weights)) = do
    activationInput <- M.multiplyVector weight input
    let output = A.forward activation activationInput
    ((activationInput, output) :) <$> forward' output (Network activations weights)

fromList :: [Int] -> [A.Activation] -> R.StdGen -> E.Either String (Network Double)
fromList layers activations generator
    | length layers - 1 /= length activations = E.Left "Mismatching dimensions between layers and activations."
    | otherwise = E.Right $ Network activations (M.fromLayersList layers generator)


{-- Backward Propagation --}

backwardStep :: RealFloat a =>
                V.Vector a    -- ^ Activation Input of the layer
             -> A.Activation  -- ^ Activation function
             -> M.Matrix a    -- ^ Weights of current layer
             -> V.Vector a    -- ^ Ouput of the above layer
             -> V.Vector a    -- ^ Current gradient
             -> V.Vector a    -- ^ Current derivative
             -> Either String (M.Matrix a, V.Vector a)
backwardStep activationInput activation weight output gradient derivative = (,) <$> nextGradient <*> nextDerivative
  where jacobian = A.derivate activation activationInput
        nextDerivative = V.zipWith (*) derivative <$> jacobian `M.multiplyMatrix` weight
        nextGradient = E.Right $ output `crossVector` V.zipWith (*) derivative jacobian
        crossVector v1 v2 = M.Matrix (length v1) (length v2) (v1 >>= \e -> fmap (*e) v2)
