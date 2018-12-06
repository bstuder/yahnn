{-# LANGUAGE Strict, DeriveGeneric, PatternSynonyms, ViewPatterns #-}

module Matrix
(
    Axis(..),
    pattern ColumnVector,
    pattern DiagonalMatrix,
    pattern FullMatrix,
    Matrix,
    pattern RowVector,

    addMatrices,
    concatenate,
    empty,
    equal,
    fromList,
    fromVector,
    generate,
    init,
    map,
    maximum,
    minimum,
    multiplyMatrices,
    normalize,
    singleton,
    showSize,
    Matrix.take,
    toRows,
    toColumns,
    transpose,
    unsafeFromList
) where

import qualified Data.List as DL (transpose)
import qualified Data.Maybe as DM (fromMaybe)
import qualified Data.Serialize as DS (decodeLazy, encodeLazy, Serialize)
import qualified Data.Vector as DV (Vector, toList)
import qualified Data.Vector.Unboxed as DVU ((++), (!), and, backpermute, concat, empty, fromList, generate, length, map, maximum, minimum, take, toList, Vector, zipWith)
import qualified Data.Vector.Serialize as DVS (genericGetVector, genericPutVector)
import qualified GHC.Generics as GG (Generic)
import Prelude as P hiding (init, map, maximum, minimum)
import qualified Utils as U (chunksOf, dotProduct)


{----- TYPES -----}

data Matrix = Matrix {
    rows    :: Int,
    columns :: Int,
    vector  :: DVU.Vector Double
} deriving (Eq, GG.Generic, Show)

data Axis = Columns | Rows deriving (Eq, Show)

{----- PATTERNS -----}

pattern ColumnVector size vector <- Matrix (validateSize -> Just size) 1 (validateVector size -> Just vector) where
    ColumnVector size vector = Matrix size 1 vector

pattern DiagonalMatrix size vector <- Matrix (validateSize -> Just size) ((== size) -> True) (validateVector size -> Just vector) where
    DiagonalMatrix size vector = Matrix size size vector

pattern FullMatrix rows columns vector <- Matrix rows columns (validateVector (rows * columns) -> Just vector) where
    FullMatrix rows columns vector = Matrix rows columns vector

pattern RowVector size vector <- Matrix 1 (validateSize -> Just size) (validateVector size -> Just vector) where
    RowVector size vector = Matrix 1 size vector

pattern SingletonMatrix vector <- Matrix 1 1 (validateVector 1 -> Just vector) where
    SingletonMatrix vector = Matrix 1 1 vector

validateSize :: Int -> Maybe Int
validateSize size = if size > 1 then Just size else Nothing

validateVector :: Int -> DVU.Vector Double -> Maybe (DVU.Vector Double)
validateVector size vector = if DVU.length vector == size then Just vector else Nothing


{----- INSTANCES -----}

instance DS.Serialize Matrix


{----- EXPORTED METHODS -----}

infixr 8 !

(!) :: Matrix -> (Int, Int) -> Double
(!) (Matrix nrow ncol v) (row, col) = v DVU.! (row * ncol  + col)

addMatrices ::  Matrix -> Matrix -> Either String Matrix
addMatrices full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= addMatrices full
addMatrices diagonal@DiagonalMatrix{} full@FullMatrix{} = addMatrices full diagonal
addMatrices firstMatrix@(Matrix firstRows firstColumns firstVector) secondMatrix@(Matrix secondRows secondColumns secondVector)
    | (firstRows /= secondRows) || (firstColumns /= secondColumns) = Left $ "Cannot add matrix " ++ showSize firstMatrix ++ " to matrix " ++ showSize secondMatrix
    | otherwise = Right $ Matrix firstRows firstColumns $ DVU.zipWith (+) firstVector secondVector

concatenate :: Axis -> Matrix -> Matrix -> Either String Matrix
concatenate axis full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= concatenate axis full
concatenate axis diagonal@DiagonalMatrix{} full@FullMatrix{} = toFull diagonal >>= \matrix -> concatenate axis matrix full
concatenate Columns firstMatrix@(Matrix firstRows firstColumns firstVector) secondMatrix@(Matrix secondRows secondColumns secondVector)
    | firstRows /= secondRows = Left $ "Cannot append columns of matrix " ++ showSize secondMatrix ++ " to matrix " ++ showSize firstMatrix
    | otherwise = Right $ Matrix firstRows (firstColumns + secondColumns) $ DVU.concat $ zipWith (DVU.++) (toRows firstMatrix) (toRows secondMatrix)
concatenate Rows firstMatrix@(Matrix firstRows firstColumns firstVector) secondMatrix@(Matrix secondRows secondColumns secondVector)
    | firstColumns /= secondColumns = Left $ "Cannot append rows of matrix " ++ showSize secondMatrix ++ " to matrix " ++ showSize firstMatrix
    | otherwise = Right $ Matrix (firstRows + secondRows) firstColumns $ firstVector DVU.++ secondVector

empty :: Matrix
empty = Matrix 0 0 mempty

equal ::  Double -> Matrix -> Matrix -> Bool
equal precision (Matrix firstRows firstColumns firstVector) (Matrix secondRows secondColumns secondVector)
    | (firstRows, firstColumns) /= (secondRows, secondColumns) = False
    | otherwise = DVU.and $ DVU.zipWith (\x y -> abs (x - y) <= precision) firstVector secondVector

fromList :: Int -> Int -> [Double] -> Either String Matrix
fromList rows columns list
    | (rows == columns && rows == length list) || (rows * columns == length list) = Right $ unsafeFromList rows columns list
    | otherwise = Left "Mismatch between dimensions and list length"

fromVector ::  Int -> Int -> DVU.Vector Double -> Either String Matrix
fromVector rows columns vector = fromList rows columns $ DVU.toList vector

generate :: Int -> Int -> ((Int, Int) -> Double) -> Matrix
generate rows columns function =
    Matrix rows columns $ DVU.generate (rows * columns) (\indice -> function (indice `div` columns, indice `mod` columns))

init :: Axis -> Matrix -> Either String Matrix
init Columns matrix@(Matrix rows columns vector) = Matrix.take Columns (columns - 1) matrix
init Rows matrix@(Matrix rows columns vector) = Matrix.take Rows (rows - 1) matrix

map :: (Double -> Double) -> Matrix -> Matrix
map function (Matrix rows columns vector) = Matrix rows columns $ DVU.map function vector

maximum ::  Matrix -> Double
maximum (Matrix _ _ vector) = DVU.maximum vector

minimum ::  Matrix -> Double
minimum (Matrix _ _ vector) = DVU.minimum vector

normalize ::  Maybe Double -> Maybe Double -> Matrix -> Matrix
normalize maybeUpperBound maybeLowerBound matrix = map transform matrix
  where transform  = if upperBound == lowerBound
                       then const 0
                       else \x -> (2 * x - upperBound - lowerBound) / (upperBound - lowerBound)
        upperBound = DM.fromMaybe (maximum matrix) maybeUpperBound
        lowerBound = DM.fromMaybe (minimum matrix) maybeLowerBound

multiplyMatrices ::  Matrix -> Matrix -> Either String Matrix
multiplyMatrices full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= multiplyMatrices full
multiplyMatrices diagonal@DiagonalMatrix{} full@FullMatrix{} = toFull diagonal >>= flip multiplyMatrices full
multiplyMatrices firstMatrix@(DiagonalMatrix firstSize firstVector) secondMatrix@(DiagonalMatrix secondSize secondVector)
    | firstSize /= secondSize = Left $ "Cannot multiply matrix " ++ showSize firstMatrix ++ " with matrix " ++ showSize secondMatrix
    | otherwise = Right $ DiagonalMatrix firstSize $ DVU.zipWith (*) firstVector secondVector
multiplyMatrices firstMatrix@(FullMatrix firstRows firstColumns _) secondMatrix@(FullMatrix secondRows secondColumns _)
    | firstColumns /= secondRows = Left $ "Cannot multiply matrix " ++ showSize firstMatrix ++ " with matrix " ++ showSize secondMatrix
    | otherwise = Right $ generate firstRows secondColumns (\(i, j) -> sum [ firstMatrix ! (i,k) * secondMatrix ! (k,j) | k <- [0..firstColumns-1]])

singleton :: Double -> Matrix
singleton value = unsafeFromList 1 1 [value]

showSize :: Matrix -> String
showSize (Matrix rows columns _) = "[" <> show rows <> " x " <> show columns <> "]"

take :: Axis -> Int -> Matrix -> Either String Matrix
take axis value diagonal@DiagonalMatrix{} = toFull diagonal >>= Matrix.take axis value
take Columns value matrix@(Matrix rows columns vector)
    | value >= columns = Right empty
    | otherwise = Right $ Matrix rows value $ DVU.concat (DVU.take value <$> toRows matrix)
take Rows value matrix@(Matrix rows columns vector)
    | value >= rows = Right empty
    | otherwise = Right $ Matrix value columns $ DVU.take (value * columns) vector

toFull :: Matrix -> Either String Matrix
toFull (DiagonalMatrix size vector) = Right $ generate size size (\(row, column) -> if row == column then vector DVU.! row else 0)
toFull ColumnVector{} = Left "Cannot transform a column vector into a matrix"
toFull RowVector{} = Left "Cannot transform a row vector into a matrix"
toFull matrix@Matrix{} = Right matrix

toRows :: Matrix -> [DVU.Vector Double]
toRows (Matrix _ columns vector) = DV.toList $ U.chunksOf columns vector

toColumns :: Matrix -> [DVU.Vector Double]
toColumns = toRows . transpose

transpose :: Matrix -> Matrix
transpose diagonal@DiagonalMatrix{} = diagonal
transpose (ColumnVector size vector) = RowVector size vector
transpose (RowVector size vector) = ColumnVector size vector
transpose (FullMatrix rows columns vector) = Matrix columns rows transposedVector
  where
    transposedVector = DVU.backpermute vector (DVU.fromList newIndexes)
    newIndexes = concat . DL.transpose . P.take rows . iterate (fmap (+columns)) $ [0 .. columns - 1]

unsafeFromList :: Int -> Int -> [Double] -> Matrix
unsafeFromList rows columns list = Matrix rows columns $ DVU.fromList list
