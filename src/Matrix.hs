module Matrix where

import qualified Data.Either as E (Either(..))
import qualified Data.List as L (transpose)
import qualified Data.Vector as V (backpermute, cons, empty, fromList, generate, Vector(..), zipWith, length)
import qualified System.Random as R (StdGen(..), split)
import qualified Utils as U (chunksOf, generateVector)

data Matrix a = Matrix {
    rows :: !Int,
    columns :: !Int,
    vector :: V.Vector a
} deriving (Eq, Show)

empty :: Matrix a
empty = Matrix 0 0 mempty

showDim :: Matrix a -> String
showDim (Matrix rows columns _) = "(" <> show rows <> "," <> show columns <> ")"

fromRowVector :: V.Vector a -> Matrix a
fromRowVector v = Matrix 1 (V.length v) v

fromColumnVector :: V.Vector a -> Matrix a
fromColumnVector v = Matrix (V.length v) 1 v

dotProduct :: RealFloat a => V.Vector a -> V.Vector a -> a
dotProduct v w = sum $ V.zipWith (*) v w

matmul :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
matmul matrixLeft matrixRight
  | columns matrixLeft /= rows matrixRight = E.Left "Mismatching dimensions between two matrix"
  | otherwise = E.Right $ Matrix (rows matrixLeft) (columns matrixRight) newVector
  where newVector = toRows matrixLeft >>= \row -> dotProduct row <$> rightColumns
        rightColumns = toColumns matrixRight

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
  | columns matrix /= V.length vector = E.Left $ "Mismatching dimensions between the matrix " <> showDim matrix <> " and the vector: " <> show (V.length vector)
    | otherwise = E.Right $ dotProduct vector <$> toRows matrix

multiplyMatrix :: (RealFloat a) => V.Vector a -> Matrix a -> E.Either String (V.Vector a)
multiplyMatrix vector matrix = transpose matrix `multiplyVector` vector

toRows :: Matrix a -> V.Vector (V.Vector a)
toRows (Matrix _ columns vector) = U.chunksOf columns vector

toColumns :: Matrix a -> V.Vector (V.Vector a)
toColumns = toRows . transpose

transpose :: Matrix a -> Matrix a
transpose (Matrix rows columns vector) = Matrix columns rows transposedVector
  where transposedVector = V.backpermute vector (V.fromList newIndexes)
        newIndexes = concat . L.transpose . take rows . iterate (fmap (+columns)) $ [0 .. columns - 1]
