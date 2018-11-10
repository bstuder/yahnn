module Matrix where

import Utils (chunksOf)
import qualified Data.Vector as V (empty, Vector(..), zipWith)

data Matrix a = Matrix {
    rows :: !Int,
    columns :: !Int,
    vector :: V.Vector a
}

applyRow :: (V.Vector a -> b) -> Matrix a -> V.Vector b
applyRow function = fmap function . toRows

multiplyVector :: (Real a) => Matrix a -> V.Vector a -> V.Vector a
multiplyVector matrix vector
    | columns matrix /= length vector = V.empty
    | otherwise = fmap sum $ fmap (\x -> V.zipWith (*) x vector) (toRows matrix)

toRows :: Matrix a -> V.Vector (V.Vector a)
toRows (Matrix _ columns vector) = chunksOf columns vector