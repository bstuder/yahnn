module Matrix where

import qualified Utils as U (chunksOf, generateVector)
import qualified Data.Vector as V (cons, empty, generate, replicate, Vector(..), zipWith)
import qualified System.Random as S (randomR, StdGen(..))

data Matrix a = Matrix {
    rows :: !Int,
    columns :: !Int,
    vector :: V.Vector a
} deriving (Show)

applyRow :: (V.Vector a -> b) -> Matrix a -> V.Vector b
applyRow function = fmap function . toRows

fromLayersList :: [Int] -> S.StdGen -> V.Vector (Matrix Double)
fromLayersList [] _ = V.empty
fromLayersList [x] _ = V.empty
fromLayersList (x:y:xs) generator = Matrix x y randomVector `V.cons` fromLayersList (y:xs) generator
  where
    randomVector = U.generateVector (x * y) generator

generate :: Int -> Int -> (Int -> a) -> Matrix a
generate rows columns function = Matrix rows columns (V.generate (rows * columns) function)

multiplyVector :: (Real a) => Matrix a -> V.Vector a -> V.Vector a
multiplyVector matrix vector
    | columns matrix /= length vector = V.empty
    | otherwise = sum . V.zipWith (*) vector <$> toRows matrix

toRows :: Matrix a -> V.Vector (V.Vector a)
toRows (Matrix _ columns vector) = U.chunksOf columns vector
