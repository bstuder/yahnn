module Network where

import qualified Activation as A
import qualified Data.Vector as V (empty, fromList, Vector(..))
import qualified Matrix as M (fromLayersList, Matrix(..))
import qualified System.Random as S (StdGen(..))

data Network a = Network {
    activations :: V.Vector A.Activation,
    weights :: V.Vector (M.Matrix a)
} deriving (Show)

fromList :: [Int] -> [A.Activation] -> S.StdGen -> Network Double
fromList layers activations generator
--    | length layers /= length activations - 1 = Void
    | otherwise = Network (V.fromList activations) (M.fromLayersList layers generator)
