module Network where

import qualified Activation as A (Activation(..), activationForward)
import qualified Data.Vector as V (empty, fromList, Vector(..))
import qualified Matrix as M (fromLayersList, Matrix(..), multiplyVector)
import qualified System.Random as S (StdGen(..))

data Network a = Network {
    activations :: [A.Activation],
    weights :: [M.Matrix a]
} deriving (Eq, Show)

forward :: (Real a) => V.Vector a -> Network a -> [V.Vector a]
forward input (Network [] []) = []
forward input (Network (a:ax) (w:wx)) = output : forward output (Network ax wx)
    where output = A.activationForward a $ M.multiplyVector w input

fromList :: [Int] -> [A.Activation] -> S.StdGen -> Network Double
fromList layers activations generator
--    | length layers /= length activations - 1 = Void
    | otherwise = Network activations (M.fromLayersList layers generator)
