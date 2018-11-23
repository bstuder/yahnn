module Matrix where

import qualified Data.Either as E (Either(..))
import qualified Data.List as DL (transpose)
import qualified Data.Vector as V (and, backpermute, cons, empty, fromList, generate, length, map, Vector(..), zipWith)
import qualified System.Random as R (StdGen(..), split)
import qualified Utils as U (chunksOf, dotProduct, generateVector)

data Matrix a = Matrix
    { rows :: !Int
    , columns :: !Int
    , vector :: V.Vector a
    } deriving (Eq, Show)

instance Functor Matrix where
    fmap function (Matrix rows columns vector) = Matrix rows columns $ V.map function vector

addMatrices :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
addMatrices (Matrix firstRows firstColumns firstVector) (Matrix secondRows secondColumns secondVector)
    | (firstRows /= secondRows) || (firstColumns /= secondColumns) = E.Left "Mismatching dimensions between both matrices"
    | otherwise = E.Right $ Matrix firstRows firstColumns $ V.zipWith (+) firstVector secondVector

applyRow :: (V.Vector a -> b) -> Matrix a -> V.Vector b
applyRow function = fmap function . toRows

empty :: Matrix a
empty = Matrix 0 0 mempty

equal :: RealFloat a => Matrix a -> Matrix a -> a -> Bool
equal (Matrix firstRows firstColumns firstVector) (Matrix secondRows secondColumns secondVector) precision
    | (firstRows /= secondRows) || (firstColumns /= secondColumns) = False
    | otherwise = V.and $ V.zipWith (\x y -> abs (x - y) <= precision) firstVector secondVector

fromColumnVector :: V.Vector a -> Matrix a
fromColumnVector vector = Matrix (V.length vector) 1 vector

fromLayersList :: [Int] -> R.StdGen -> [Matrix Double]
fromLayersList [] _ = []
fromLayersList [x] _ = []
fromLayersList (x:y:xs) generator = Matrix y x randomVector : fromLayersList (y:xs) secondGenerator
  where
    (firstGenerator, secondGenerator) = R.split generator
    randomVector = U.generateVector (x * y) firstGenerator

fromRowVector :: V.Vector a -> Matrix a
fromRowVector vector = Matrix 1 (V.length vector) vector

fromVectors :: RealFloat a => V.Vector a -> V.Vector a -> Matrix a
fromVectors firstVector secondVector = Matrix (length firstVector) (length secondVector) (firstVector >>= \x -> fmap (*x) secondVector)

generate :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
generate rows columns function =
    Matrix rows columns $ V.generate (rows * columns) (\indice -> function (indice `div` columns, indice `mod` columns))

multiplyMatrices :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
multiplyMatrices leftMatrix rightMatrix
    | columns leftMatrix /= rows rightMatrix = E.Left "Mismatching dimensions between both matrices"
    | otherwise = E.Right $ Matrix (rows leftMatrix) (columns rightMatrix) newVector
  where
    newVector = toRows leftMatrix >>= \row -> U.dotProduct row <$> rightColumns
    rightColumns = toColumns rightMatrix

multiplyVectorL :: (RealFloat a) => V.Vector a -> Matrix a -> E.Either String (V.Vector a)
multiplyVectorL vector matrix
    | rows matrix /= V.length vector = E.Left $ "Mismatching dimensions between vector " <> show (V.length vector) <> " and matrix " <> showSize matrix
    | otherwise = transpose matrix `multiplyVectorR` vector

multiplyVectorR :: (RealFloat a) => Matrix a -> V.Vector a -> E.Either String (V.Vector a)
multiplyVectorR matrix vector
    | columns matrix /= V.length vector = E.Left $ "Mismatching dimensions between matrix " <> showSize matrix <> " and vector " <> show (V.length vector)
    | otherwise = E.Right $ U.dotProduct vector <$> toRows matrix

showSize :: Matrix a -> String
showSize (Matrix rows columns _) = "(" <> show rows <> "," <> show columns <> ")"

toRows :: Matrix a -> V.Vector (V.Vector a)
toRows (Matrix _ columns vector) = U.chunksOf columns vector

toColumns :: Matrix a -> V.Vector (V.Vector a)
toColumns = toRows . transpose

transpose :: Matrix a -> Matrix a
transpose (Matrix rows columns vector) = Matrix columns rows transposedVector
  where
    transposedVector = V.backpermute vector (V.fromList newIndexes)
    newIndexes = concat . DL.transpose . take rows . iterate (fmap (+columns)) $ [0 .. columns - 1]
