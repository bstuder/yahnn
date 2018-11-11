module Utils where

import qualified Data.Vector as V (cons, drop, empty, null, take, Vector(..))
import qualified System.Random as S (randomR, StdGen(..))

chunksOf :: Int -> V.Vector a -> V.Vector (V.Vector a)
chunksOf length vector
    | length <= 0 = V.empty
    | V.null vector = V.empty
    | otherwise = V.take length vector `V.cons` chunksOf length (V.drop length vector)

generateVector :: Int -> S.StdGen -> (V.Vector Double, S.StdGen)
generateVector length generator
    | length <= 0 = (V.empty, generator)
    | otherwise =
        let (first, last) = S.randomR (-1.0, 1.0) generator
            (vector, next_generator) = generateVector (length - 1) last
        in (first `V.cons` vector, next_generator)
