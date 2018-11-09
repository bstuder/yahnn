module Matrix where

import Utils (chunksOf)
import qualified Data.Vector as V

data Matrix a = Matrix {
    nrow :: !Int,
    ncol :: !Int,
    vec :: V.Vector a
}

toRows :: Matrix a -> V.Vector (V.Vector a)
toRows (Matrix _ col vec) = chunksOf col vec

applyRow :: (V.Vector a -> b) -> Matrix a -> V.Vector b
applyRow f = fmap f . toRows
