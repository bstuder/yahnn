{-# LANGUAGE Strict, DeriveGeneric, PatternSynonyms, ViewPatterns #-}

module Matrix
(
    Matrix,
    pattern FullMatrix,
    pattern DiagonalMatrix,
    pattern RowVector,
    pattern ColumnVector,
    Matrix.map,
    empty,
    equal,
    applyRow,
    addMatrices,
    fromLayers,
    fromList,
    fromVector,
    generate,
    maximum,
    minimum,
    multiplyMatrices,
    normalize,
    showSize,
    toRows,
    toColumns,
    transpose,
    unsafeFromList
) where

import Data.Maybe (fromMaybe)
import qualified Data.List as DL (transpose)
import qualified Data.Vector as DVB (Vector(..))
import qualified Data.Vector.Unboxed as DV (Vector(..), Unbox, (!), and, backpermute, fromList, generate, length, map, maximum, minimum, toList, zipWith, empty)
import qualified Data.Vector.Serialize as DVS (genericGetVector, genericPutVector)
import Prelude hiding (minimum, maximum)
import qualified System.Random as R (StdGen(..), split)
import qualified Utils as U (chunksOf, dotProduct, generateVector)

import qualified Data.Serialize as DS (decodeLazy, encodeLazy, Serialize)
import qualified GHC.Generics as GG (Generic(..))


{----- TYPES -----}

data Matrix a = Matrix {
    rows    :: Int,
    columns :: Int,
    vector  :: DV.Vector a
} deriving (Eq, GG.Generic, Show)


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

validateVector :: DV.Unbox a => Int -> DV.Vector a -> Maybe (DV.Vector a)
validateVector size vector = if DV.length vector == size then Just vector else Nothing

{----- INSTANCES -----}

instance (DS.Serialize a, DV.Unbox a) => DS.Serialize (Matrix a)

{----- EXPORTED METHODS -----}

addMatrices :: (DV.Unbox a, RealFloat a) => Matrix a -> Matrix a -> Either String (Matrix a)
addMatrices full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= addMatrices full
addMatrices diagonal@DiagonalMatrix{} full@FullMatrix{} = addMatrices full diagonal
addMatrices firstMatrix@(Matrix firstRows firstColumns firstVector) secondMatrix@(Matrix secondRows secondColumns secondVector)
    | (firstRows /= secondRows) || (firstColumns /= secondColumns) = Left $ "Cannot add matrix " ++ showSize firstMatrix ++ " to matrix " ++ showSize secondMatrix
    | otherwise = Right $ Matrix firstRows firstColumns $ DV.zipWith (+) firstVector secondVector

map :: (DV.Unbox a, DV.Unbox b) => (a -> b) -> Matrix a -> Matrix b
map function (Matrix rows columns vector) = Matrix rows columns $ DV.map function vector

applyRow :: DV.Unbox a => (DV.Vector a -> b) -> Matrix a -> DVB.Vector b
applyRow function = fmap function . toRows

empty :: DV.Unbox a => Matrix a
empty = Matrix 0 0 DV.empty

equal :: (DV.Unbox a, RealFloat a) => a -> Matrix a -> Matrix a -> Bool
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

fromList :: DV.Unbox a => Int -> Int -> [a] -> Either String (Matrix a)
fromList rows columns list
    | (rows == columns && rows == length list) || (rows * columns == length list) = Right $ unsafeFromList rows columns list
    | otherwise = Left "Mismatch between dimensions and list length"

fromVector :: (DV.Unbox a, RealFloat a) => Int -> Int -> DV.Vector a -> Either String (Matrix a)
fromVector rows columns vector = fromList rows columns $ DV.toList vector

generate :: DV.Unbox a => Int -> Int -> ((Int, Int) -> a) -> Matrix a
generate rows columns function =
    Matrix rows columns $ DV.generate (rows * columns) (\indice -> function (indice `div` columns, indice `mod` columns))

maximum :: (DV.Unbox a, RealFloat a) => Matrix a -> a
maximum (Matrix _ _ vector) = DV.maximum vector

minimum :: (DV.Unbox a, RealFloat a) => Matrix a -> a
minimum (Matrix _ _ vector) = DV.minimum vector

normalize :: (DV.Unbox a, RealFloat a) => Maybe a -> Maybe a -> Matrix a -> Matrix a
normalize maybeUpperBound maybeLowerBound matrix = Matrix.map transform matrix
  where transform  = if upperBound == lowerBound
                       then const 0
                       else \x -> (2 * x - upperBound - lowerBound) / (upperBound - lowerBound)
        upperBound = fromMaybe (maximum matrix) maybeUpperBound
        lowerBound = fromMaybe (minimum matrix) maybeLowerBound

infixr 8 !

(!) :: DV.Unbox a => DV.Unbox a => Matrix a -> (Int, Int) -> a
(!) (Matrix nrow ncol v) (row, col) = v DV.! (row * ncol  + col)

multiplyMatrices :: (DV.Unbox a, RealFloat a) => Matrix a -> Matrix a -> Either String (Matrix a)
multiplyMatrices full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= multiplyMatrices full
multiplyMatrices diagonal@DiagonalMatrix{} full@FullMatrix{} = toFull diagonal >>= flip multiplyMatrices full
multiplyMatrices firstMatrix@(DiagonalMatrix firstSize firstVector) secondMatrix@(DiagonalMatrix secondSize secondVector)
    | firstSize /= secondSize = Left $ "Cannot multiply matrix " ++ showSize firstMatrix ++ " with matrix " ++ showSize secondMatrix
    | otherwise = Right $ DiagonalMatrix firstSize $ DV.zipWith (*) firstVector secondVector
multiplyMatrices firstMatrix@(Matrix firstRows firstColumns v1) secondMatrix@(Matrix secondRows secondColumns v2)
    | firstColumns /= secondRows = Left $ "Cannot multiply matrix " ++ showSize firstMatrix ++ " with matrix " ++ showSize secondMatrix
    | otherwise = Right $ generate firstRows secondColumns (\(i, j) -> sum [ firstMatrix ! (i,k) * secondMatrix ! (k,j) | k <- [0..firstColumns-1]])

showSize :: Matrix a -> String
showSize (Matrix rows columns _) = "[" <> show rows <> " x " <> show columns <> "]"

toFull :: (DV.Unbox a, RealFloat a) => Matrix a -> Either String (Matrix a)
toFull (DiagonalMatrix size vector) = Right $ generate size size (\(row, column) -> if row == column then vector DV.! row else 0)
toFull ColumnVector{} = Left "Cannot transform a column vector into a matrix"
toFull RowVector{} = Left "Cannot transform a row vector into a matrix"
toFull matrix@Matrix{} = Right matrix

toRows :: DV.Unbox a => Matrix a -> DVB.Vector (DV.Vector a)
toRows (Matrix _ columns vector) = U.chunksOf columns vector

toColumns :: DV.Unbox a => Matrix a -> DVB.Vector (DV.Vector a)
toColumns = toRows . transpose

transpose :: DV.Unbox a => Matrix a -> Matrix a
transpose diagonal@DiagonalMatrix{} = diagonal
transpose (ColumnVector size vector) = RowVector size vector
transpose (RowVector size vector) = ColumnVector size vector
transpose (Matrix rows columns vector) = Matrix columns rows transposedVector
  where
    transposedVector = DV.backpermute vector (DV.fromList newIndexes)
    newIndexes = concat . DL.transpose . take rows . iterate (fmap (+columns)) $ [0 .. columns - 1]

unsafeFromList :: DV.Unbox a => Int -> Int -> [a] -> Matrix a
unsafeFromList rows columns list = Matrix rows columns $ DV.fromList list
