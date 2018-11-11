module Utils where

import qualified Data.Vector as V (cons, drop, empty, null, take, fromListN, Vector(..))
import qualified System.Random as S (randomRs, StdGen(..))

chunksOf :: Int -> V.Vector a -> V.Vector (V.Vector a)
chunksOf length vector
    | length <= 0 = V.empty
    | V.null vector = V.empty
    | otherwise = V.take length vector `V.cons` chunksOf length (V.drop length vector)

generateVector :: Int -> S.StdGen -> V.Vector Double
generateVector n = V.fromListN n . S.randomRs (-1.0, 1.0)
