module Matrix where

import qualified Data.List as DL (transpose)
import qualified Data.Vector as DV (and, backpermute, fromList, generate, length, map, Vector(..), zipWith)
import qualified System.Random as R (StdGen(..), split)
import qualified Utils as U (chunksOf, dotProduct, generateVector)

data Matrix a = Matrix {
    rows :: !Int,
    columns :: !Int,
    vector :: DV.Vector a
} deriving (Eq, Show)

instance Functor Matrix where
    fmap function (Matrix rows columns vector) = Matrix rows columns $ DV.map function vector

addMatrices :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
addMatrices (Matrix firstRows firstColumns firstVector) (Matrix secondRows secondColumns secondVector)
    | (firstRows /= secondRows) || (firstColumns /= secondColumns) = Left "Mismatching dimensions between both matrices"
    | otherwise = Right $ Matrix firstRows firstColumns $ DV.zipWith (+) firstVector secondVector

applyRow :: (DV.Vector a -> b) -> Matrix a -> DV.Vector b
applyRow function = fmap function . toRows

empty :: Matrix a
empty = Matrix 0 0 mempty

equal :: RealFloat a => a -> Matrix a -> Matrix a -> Bool
equal precision (Matrix firstRows firstColumns firstVector) (Matrix secondRows secondColumns secondVector)
    | (firstRows, firstColumns) /= (secondRows, secondColumns) = False
    | otherwise = DV.and $ DV.zipWith (\x y -> abs (x - y) <= precision) firstVector secondVector

fromColumnVector :: DV.Vector a -> Matrix a
fromColumnVector vector = Matrix (DV.length vector) 1 vector

fromLayersList :: [Int] -> R.StdGen -> [Matrix Double]
fromLayersList [] _ = []
fromLayersList [x] _ = []
fromLayersList (x:y:xs) generator = Matrix y x randomVector : fromLayersList (y:xs) secondGenerator
  where
    (firstGenerator, secondGenerator) = R.split generator
    randomVector = U.generateVector (x * y) firstGenerator

fromRowVector :: DV.Vector a -> Matrix a
fromRowVector vector = Matrix 1 (DV.length vector) vector

fromVectors :: RealFloat a => DV.Vector a -> DV.Vector a -> Matrix a
fromVectors firstVector secondVector = Matrix (length firstVector) (length secondVector) (firstVector >>= \x -> fmap (*x) secondVector)

generate :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
generate rows columns function =
    Matrix rows columns $ DV.generate (rows * columns) (\indice -> function (indice `div` columns, indice `mod` columns))

multiplyMatrices :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
multiplyMatrices leftMatrix rightMatrix
    | columns leftMatrix /= rows rightMatrix = Left "Mismatching dimensions between both matrices"
    | otherwise = Right $ Matrix (rows leftMatrix) (columns rightMatrix) newVector
  where
    newVector = toRows leftMatrix >>= \row -> U.dotProduct row <$> rightColumns
    rightColumns = toColumns rightMatrix

multiplyVectorL :: (RealFloat a) => DV.Vector a -> Matrix a -> Either String (DV.Vector a)
multiplyVectorL vector matrix
    | rows matrix /= DV.length vector = Left $ "Mismatching dimensions between vector " <> show (DV.length vector) <> " and matrix " <> showSize matrix
    | otherwise = transpose matrix `multiplyVectorR` vector

multiplyVectorR :: (RealFloat a) => Matrix a -> DV.Vector a -> Either String (DV.Vector a)
multiplyVectorR matrix vector
    | columns matrix /= DV.length vector = Left $ "Mismatching dimensions between matrix " <> showSize matrix <> " and vector " <> show (DV.length vector)
    | otherwise = Right $ U.dotProduct vector <$> toRows matrix

showSize :: Matrix a -> String
showSize (Matrix rows columns _) = "(" <> show rows <> "," <> show columns <> ")"

toRows :: Matrix a -> DV.Vector (DV.Vector a)
toRows (Matrix _ columns vector) = U.chunksOf columns vector

toColumns :: Matrix a -> DV.Vector (DV.Vector a)
toColumns = toRows . transpose

transpose :: Matrix a -> Matrix a
transpose (Matrix rows columns vector) = Matrix columns rows transposedVector
  where
    transposedVector = DV.backpermute vector (DV.fromList newIndexes)
    newIndexes = concat . DL.transpose . take rows . iterate (fmap (+columns)) $ [0 .. columns - 1]
