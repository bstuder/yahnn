module Utils where

import qualified Data.Vector as V (cons, drop, empty, fromListN, null, take, Vector(..))
import qualified System.Random as R (randomRs, StdGen(..))

chunksOf :: Int -> V.Vector a -> V.Vector (V.Vector a)
chunksOf length vector
    | length <= 0 = V.empty
    | V.null vector = V.empty
    | otherwise = V.take length vector `V.cons` chunksOf length (V.drop length vector)

generateVector :: Int -> R.StdGen -> V.Vector Double
generateVector n = V.fromListN n . R.randomRs (-1.0, 1.0)
