module Utils where

import Data.Vector as V (cons, drop, empty, null, take, Vector(..))

chunksOf :: Int -> V.Vector a -> V.Vector (V.Vector a)
chunksOf length vector
    | V.null vector = V.empty
    | otherwise = V.cons (V.take length vector) (chunksOf length (V.drop length vector))
