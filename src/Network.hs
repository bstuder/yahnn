module Network where

import qualified Activation as A (Activation(..), forward)
import qualified Data.Either as E (Either(..))
import qualified Data.Vector as V (empty, fromList, Vector(..))
import qualified Matrix as M (fromLayersList, Matrix(..), multiplyVector)
import qualified System.Random as R (StdGen(..))

data Network a = Network {
    activations :: [A.Activation],
    weights :: [M.Matrix a]
} deriving (Eq, Show)

forward :: (RealFloat a) => V.Vector a -> Network a -> E.Either String [V.Vector a]
forward input (Network [] []) = E.Right []
forward input (Network (activation:activations) (weight:weights)) = do
    ouput <- A.forward activation <$> M.multiplyVector weight input 
    (ouput :) <$> forward ouput (Network activations weights)

fromList :: [Int] -> [A.Activation] -> R.StdGen -> E.Either String (Network Double)
fromList layers activations generator
    | length layers - 1 /= length activations = E.Left "Mismatching dimensions between layers and activations."
    | otherwise = E.Right $ Network activations (M.fromLayersList layers generator)
