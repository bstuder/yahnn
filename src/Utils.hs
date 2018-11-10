module Utils where

import Data.Vector as V (cons, drop, empty, null, take, Vector(..))

chunksOf :: Int -> V.Vector a -> V.Vector (V.Vector a)
chunksOf length vector
    | length <= 0 = V.empty
    | V.null vector = V.empty
    | otherwise = V.take length vector `V.cons` chunksOf length (V.drop length vector)
