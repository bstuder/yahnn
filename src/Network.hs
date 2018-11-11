module Network where

import qualified Activation as A
import qualified Data.Vector as V (empty, fromList, Vector(..))
import qualified Matrix as M (fromLayersList, Matrix(..))
import qualified System.Random as S (StdGen(..))

data Network a = Network {
    activations :: [A.Activation],
    weights :: [M.Matrix a]
} deriving (Show)

fromList :: [Int] -> [A.Activation] -> S.StdGen -> Network Double
fromList layers activations generator
--    | length layers /= length activations - 1 = Void
    | otherwise = Network activations (M.fromLayersList layers generator)
