module Matrix where

import qualified Data.Vector as V


data Matrix a = Matrix {
    nrow :: !Int,
    ncol :: !Int,
    vec :: V.Vector a
}


chunksOf :: Int -> V.Vector a -> V.Vector (V.Vector a)
chunksOf n v
  | n <= 0 = V.empty
  | V.null v = V.empty
chunksOf n xs = V.take n xs `V.cons` chunksOf n (V.drop n xs)


toRows :: Matrix a -> V.Vector (V.Vector a)
toRows (Matrix _ col vec) = chunksOf col vec


applyRow :: (V.Vector a -> b) -> Matrix a -> V.Vector b
applyRow f = fmap f  . toRows
