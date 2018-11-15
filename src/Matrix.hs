module Matrix where

import qualified Data.Either as E (Either(..))
import qualified Data.List as L (transpose)
import qualified Data.Vector as V (backpermute, cons, empty, fromList, generate, Vector(..), zipWith)
import qualified System.Random as R (StdGen(..), split)
import qualified Utils as U (chunksOf, generateVector)

data Matrix a = Matrix {
    rows :: !Int,
    columns :: !Int,
    vector :: V.Vector a
} deriving (Eq, Show)

applyRow :: (V.Vector a -> b) -> Matrix a -> V.Vector b
applyRow function = fmap function . toRows

fromLayersList :: [Int] -> R.StdGen -> [Matrix Double]
fromLayersList [] _ = []
fromLayersList [x] _ = []
fromLayersList (x:y:xs) generator = Matrix y x randomVector : fromLayersList (y:xs) secondGenerator
  where
    (firstGenerator, secondGenerator) = R.split generator
    randomVector = U.generateVector (x * y) firstGenerator

generate :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
generate rows columns function =
    Matrix rows columns $ V.generate (rows * columns) (\indice -> function (indice `div` columns, indice `mod` columns))

multiplyVector :: (RealFloat a) => Matrix a -> V.Vector a -> E.Either String (V.Vector a)
multiplyVector matrix vector
    | columns matrix /= length vector = E.Left "Mismatching dimensions between the matrix and the vector."
    | otherwise = E.Right $ sum . V.zipWith (*) vector <$> toRows matrix

toRows :: Matrix a -> V.Vector (V.Vector a)
toRows (Matrix _ columns vector) = U.chunksOf columns vector

transpose :: Matrix a -> Matrix a
transpose (Matrix rows columns vector) = Matrix columns rows transposedVector
  where transposedVector = V.backpermute vector (V.fromList newIndexes)
        newIndexes = concat . L.transpose . take rows . iterate (fmap (+columns)) $ [0 .. columns - 1]
