{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Matrix 
(
    addMatrices,
    applyRow,
    empty,
    equal,
    fromLayers,
    fromList,
    fromRowVector,
    fromVectors,
    generate,
    Matrix,
    multiplyMatrices,
    multiplyVectorL,
    multiplyVectorR,
    showSize,
    toRows,
    toColumns,
    transpose,
    unsafeFromList
) where

import qualified Data.List as DL (transpose)
import qualified Data.Vector as DV ((!), and, backpermute, fromList, generate, length, map, Vector(..), zipWith, zip)
import qualified System.Random as R (StdGen(..), split)
import qualified Utils as U (chunksOf, dotProduct, generateVector)

data Matrix a = Matrix {
    rows       :: !Int,
    columns    :: !Int,
    vector     :: DV.Vector a
} deriving (Eq, Show)

pattern FullMatrix rows columns vector <- Matrix rows columns (validateSize (rows * columns) -> Just vector) where
    FullMatrix rows columns vector = Matrix rows columns vector
pattern DiagonalMatrix size vector <- Matrix size ((== size) -> True) (validateSize size -> Just vector) where
    DiagonalMatrix size vector = Matrix size size vector
pattern ColumnVector size vector <- Matrix size 1 (validateSize size -> Just vector) where
    ColumnVector size vector = Matrix size 1 vector
pattern RowVector size vector <- Matrix 1 size (validateSize size -> Just vector) where
    RowVector size vector = Matrix 1 size vector

instance Functor Matrix where
    fmap function (Matrix rows columns vector) = Matrix rows columns $ DV.map function vector

addMatrices :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
addMatrices full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= addMatrices full
addMatrices diagonal@DiagonalMatrix{} full@FullMatrix{} = addMatrices full diagonal
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

fromLayers :: [Int]             -- ^ List of number or neurons per layer
            -> R.StdGen         -- ^ Random generator
            -> [Matrix Double]  -- ^ Matrix with the specified layers and random weights
fromLayers [] _ = []
fromLayers [x] _ = []
fromLayers (x:y:xs) generator = Matrix y x randomVector : fromLayers (y:xs) secondGenerator
  where
    (firstGenerator, secondGenerator) = R.split generator
    randomVector = U.generateVector (x * y) firstGenerator

fromList :: RealFloat a => Int -> Int -> [a] -> Either String (Matrix a)
fromList rows columns list
    | rows * columns /= length list = Left "Mismatch between dimensions and list length"
    | otherwise = Right $ unsafeFromList rows columns list

fromRowVector :: DV.Vector a -> Matrix a
fromRowVector vector = Matrix 1 (DV.length vector) vector

fromVectors :: RealFloat a =>
               DV.Vector a      -- ^ Column vector
               -> DV.Vector a   -- ^ Row vector
               -> Matrix a      -- ^ Matrix resulting from the multiplication of both vectors.
fromVectors firstVector secondVector = Matrix (length firstVector) (length secondVector) (firstVector >>= \x -> fmap (*x) secondVector)

generate :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
generate rows columns function =
    Matrix rows columns $ DV.generate (rows * columns) (\indice -> function (indice `div` columns, indice `mod` columns))

multiplyMatrices :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
multiplyMatrices full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= multiplyMatrices full
multiplyMatrices diagonal@DiagonalMatrix{} full@FullMatrix{} = toFull diagonal >>= flip multiplyMatrices full
multiplyMatrices (DiagonalMatrix firstSize firstVector) (DiagonalMatrix secondSize secondVector)
    | firstSize /= secondSize = Left "Mismatching dimensions between both matrices"
    | otherwise = Right $ DiagonalMatrix firstSize $ DV.zipWith (*) firstVector secondVector
multiplyMatrices firstMatrix@(Matrix firstRows firstColumns _) secondMatrix@(Matrix secondRows secondColumns _)
    | firstColumns /= secondRows = Left "Mismatching dimensions between both matrices"
    | otherwise = Right $ Matrix firstRows secondColumns $ toRows firstMatrix >>= \row -> U.dotProduct row <$> toColumns secondMatrix

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

toFull :: RealFloat a => Matrix a -> Either String (Matrix a)
toFull (DiagonalMatrix size vector) = Right $ generate size size (\(row, column) -> if row == column then vector DV.! row else 0)
toFull ColumnVector{} = Left "Cannot transform a column vector into a matrix"
toFull RowVector{} = Left "Cannot transform a row vector into a matrix"
toFull matrix@Matrix{} = Right matrix

toRows :: Matrix a -> DV.Vector (DV.Vector a)
toRows (Matrix _ columns vector) = U.chunksOf columns vector

toColumns :: Matrix a -> DV.Vector (DV.Vector a)
toColumns = toRows . transpose

transpose :: Matrix a -> Matrix a
transpose diagonal@DiagonalMatrix{} = diagonal
transpose (ColumnVector size vector) = RowVector size vector
transpose (RowVector size vector) = ColumnVector size vector
transpose (Matrix rows columns vector) = Matrix columns rows transposedVector
  where
    transposedVector = DV.backpermute vector (DV.fromList newIndexes)
    newIndexes = concat . DL.transpose . take rows . iterate (fmap (+columns)) $ [0 .. columns - 1]

unsafeFromList :: RealFloat a => Int -> Int -> [a] -> Matrix a
unsafeFromList rows columns list = Matrix rows columns $ DV.fromList list

validateSize :: Int ->  DV.Vector a ->  Maybe (DV.Vector a)                                                                
validateSize size vector = if DV.length vector == size then Just vector else Nothing