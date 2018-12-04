{-# LANGUAGE Strict, DeriveGeneric, PatternSynonyms, ViewPatterns #-}

module Matrix
(
    addMatrices,
    applyRow,
    Axis(..),
    pattern ColumnVector,
    concatenate,
    pattern DiagonalMatrix,
    empty,
    equal,
    fromList,
    fromVector,
    pattern FullMatrix,
    generate,
    init,
    Matrix,
    maximum,
    minimum,
    multiplyMatrices,
    normalize,
    pattern RowVector,
    singleton,
    showSize,
    Matrix.take,
    toRows,
    toColumns,
    transpose,
    unsafeFromList
) where

import qualified Data.List as DL (transpose)
import qualified Data.Vector as DV ((!), (++), and, backpermute, empty, fromList, generate, length, map, maximum, minimum, take, toList, Vector(..), zipWith)
import qualified Data.Vector.Serialize as DVS (genericGetVector, genericPutVector)
import Prelude as P hiding (init, minimum, maximum)
import qualified Utils as U (chunksOf, dotProduct)

import qualified Data.Serialize as DS (decodeLazy, encodeLazy, Serialize)
import qualified GHC.Generics as GG (Generic(..))


{----- TYPES -----}

data Matrix a = Matrix {
    rows    :: Int,
    columns :: Int,
    vector  :: DV.Vector a
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

validateVector :: Int -> DV.Vector a -> Maybe (DV.Vector a)
validateVector size vector = if DV.length vector == size then Just vector else Nothing

{----- INSTANCES -----}

instance (DS.Serialize a) => DS.Serialize (Matrix a)
instance Functor Matrix where
    fmap function (Matrix rows columns vector) = Matrix rows columns $ DV.map function vector


{----- EXPORTED METHODS -----}

addMatrices :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
addMatrices full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= addMatrices full
addMatrices diagonal@DiagonalMatrix{} full@FullMatrix{} = addMatrices full diagonal
addMatrices firstMatrix@(Matrix firstRows firstColumns firstVector) secondMatrix@(Matrix secondRows secondColumns secondVector)
    | (firstRows /= secondRows) || (firstColumns /= secondColumns) = Left $ "Cannot add matrix " ++ showSize firstMatrix ++ " to matrix " ++ showSize secondMatrix
    | otherwise = Right $ Matrix firstRows firstColumns $ DV.zipWith (+) firstVector secondVector

applyRow :: (DV.Vector a -> b) -> Matrix a -> DV.Vector b
applyRow function = fmap function . toRows

concatenate :: RealFloat a => Axis -> Matrix a -> Matrix a -> Either String (Matrix a)
concatenate axis full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= concatenate axis full
concatenate axis diagonal@DiagonalMatrix{} full@FullMatrix{} = toFull diagonal >>= \matrix -> concatenate axis matrix full
concatenate Columns firstMatrix@(Matrix firstRows firstColumns firstVector) secondMatrix@(Matrix secondRows secondColumns secondVector)
    | firstRows /= secondRows = Left $ "Cannot append columns of matrix " ++ showSize secondMatrix ++ " to matrix " ++ showSize firstMatrix
    | otherwise = Right $ Matrix firstRows (firstColumns + secondColumns) $ foldl (DV.++) DV.empty $ DV.zipWith (DV.++) (toRows firstMatrix) (toRows secondMatrix)
concatenate Rows firstMatrix@(Matrix firstRows firstColumns firstVector) secondMatrix@(Matrix secondRows secondColumns secondVector)
    | firstColumns /= secondColumns = Left $ "Cannot append rows of matrix " ++ showSize secondMatrix ++ " to matrix " ++ showSize firstMatrix
    | otherwise = Right $ Matrix (firstRows + secondRows) firstColumns $ firstVector DV.++ secondVector

empty :: Matrix a
empty = Matrix 0 0 mempty

equal :: RealFloat a => a -> Matrix a -> Matrix a -> Bool
equal precision (Matrix firstRows firstColumns firstVector) (Matrix secondRows secondColumns secondVector)
    | (firstRows, firstColumns) /= (secondRows, secondColumns) = False
    | otherwise = DV.and $ DV.zipWith (\x y -> abs (x - y) <= precision) firstVector secondVector

fromList :: Int -> Int -> [a] -> Either String (Matrix a)
fromList rows columns list
    | (rows == columns && rows == length list) || (rows * columns == length list) = Right $ unsafeFromList rows columns list
    | otherwise = Left "Mismatch between dimensions and list length"

fromVector :: RealFloat a => Int -> Int -> DV.Vector a -> Either String (Matrix a)
fromVector rows columns vector = fromList rows columns $ DV.toList vector

generate :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
generate rows columns function =
    Matrix rows columns $ DV.generate (rows * columns) (\indice -> function (indice `div` columns, indice `mod` columns))

init :: RealFloat a => Axis -> Matrix a -> Either String (Matrix a)
init Columns matrix@(Matrix rows columns vector) = Matrix.take Columns (columns - 1) matrix
init Rows matrix@(Matrix rows columns vector) = Matrix.take Rows (rows - 1) matrix

maximum :: RealFloat a => Matrix a -> a
maximum (FullMatrix _ _ vector) = DV.maximum vector

minimum :: RealFloat a => Matrix a -> a
minimum (FullMatrix _ _ vector) = DV.minimum vector

normalize :: RealFloat a => Maybe a -> Maybe a -> Matrix a -> Matrix a
normalize maybeUpperBound maybeLowerBound matrix =
    if upperBound == lowerBound
        then const 0 <$> matrix
        else (\x -> (2 * x - upperBound - lowerBound) / (upperBound - lowerBound)) <$> matrix
  where
    upperBound = maybe (maximum matrix) id maybeUpperBound
    lowerBound = maybe (minimum matrix) id maybeLowerBound

multiplyMatrices :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
multiplyMatrices full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= multiplyMatrices full
multiplyMatrices diagonal@DiagonalMatrix{} full@FullMatrix{} = toFull diagonal >>= flip multiplyMatrices full
multiplyMatrices firstMatrix@(DiagonalMatrix firstSize firstVector) secondMatrix@(DiagonalMatrix secondSize secondVector)
    | firstSize /= secondSize = Left $ "Cannot multiply matrix " ++ showSize firstMatrix ++ " with matrix " ++ showSize secondMatrix
    | otherwise = Right $ DiagonalMatrix firstSize $ DV.zipWith (*) firstVector secondVector
multiplyMatrices firstMatrix@(FullMatrix firstRows firstColumns _) secondMatrix@(FullMatrix secondRows secondColumns _)
    | firstColumns /= secondRows = Left $ "Cannot multiply matrix " ++ showSize firstMatrix ++ " with matrix " ++ showSize secondMatrix
    | otherwise = Right $ Matrix firstRows secondColumns $ toRows firstMatrix >>= \row -> U.dotProduct row <$> toColumns secondMatrix

singleton :: a -> Matrix a
singleton value = unsafeFromList 1 1 [value]

showSize :: Matrix a -> String
showSize (Matrix rows columns _) = "[" <> show rows <> " x " <> show columns <> "]"

take :: RealFloat a => Axis -> Int -> Matrix a -> Either String (Matrix a)
take axis value diagonal@DiagonalMatrix{} = toFull diagonal >>= Matrix.take axis value
take Columns value matrix@(Matrix rows columns vector)
    | value >= columns = Right empty
    | otherwise = Right $ Matrix rows value $ toRows matrix >>= DV.take value
take Rows value matrix@(Matrix rows columns vector)
    | value >= rows = Right empty
    | otherwise = Right $ Matrix value columns $ DV.take (value * columns) vector

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
transpose (FullMatrix rows columns vector) = Matrix columns rows transposedVector
  where
    transposedVector = DV.backpermute vector (DV.fromList newIndexes)
    newIndexes = concat . DL.transpose . P.take rows . iterate (fmap (+columns)) $ [0 .. columns - 1]

unsafeFromList :: Int -> Int -> [a] -> Matrix a
unsafeFromList rows columns list = Matrix rows columns $ DV.fromList list
