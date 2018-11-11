module Matrix where

import qualified Utils as U (chunksOf, generateVector)
import qualified Data.Vector as V (cons, empty, generate, replicate, Vector(..), zipWith)
import qualified System.Random as S (StdGen(..), split)

data Matrix a = Matrix {
    rows :: !Int,
    columns :: !Int,
    vector :: V.Vector a
} deriving (Eq, Show)

applyRow :: (V.Vector a -> b) -> Matrix a -> V.Vector b
applyRow function = fmap function . toRows

fromLayersList :: [Int] -> S.StdGen -> [Matrix Double]
fromLayersList [] _ = []
fromLayersList [x] _ = []
fromLayersList (x:y:xs) generator = Matrix x y randomVector : fromLayersList (y:xs) subGenerator2
  where
    (subGenerator1, subGenerator2) = S.split generator
    randomVector = U.generateVector (x * y) subGenerator1

generate :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
generate rows columns function =
    Matrix rows columns $ V.generate (rows * columns) (\indice -> function (indice `div` columns, indice `mod` columns))

multiplyVector :: (Real a) => Matrix a -> V.Vector a -> V.Vector a
multiplyVector matrix vector
    | columns matrix /= length vector = V.empty
    | otherwise = sum . V.zipWith (*) vector <$> toRows matrix

toRows :: Matrix a -> V.Vector (V.Vector a)
toRows (Matrix _ columns vector) = U.chunksOf columns vector
