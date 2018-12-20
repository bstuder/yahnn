{-# LANGUAGE Strict, DeriveGeneric, PatternSynonyms, ViewPatterns #-}

module Matrix
(
    Axis(..),
    Matrix,
    pattern ColumnVector,

    addMatrices,
    --concatenate,
    --convolve,
    empty,
    equal,
    fromList,
    fromVector,
    fromHmat,
    imap,
    map,
    maximum,
    minimum,
    multiplyMatrices,
    normalize,
    singleton,
    showSize,
    Matrix.sum,
    transpose,
    unsafeFromList,
    zipWith,
) where

import qualified Data.Binary as DB (Binary)
import qualified Data.List as DL (transpose, zipWith)
import qualified Data.Maybe as DM (fromMaybe)
import qualified Data.Vector as DV (toList)
import qualified Data.Vector.Unboxed as DVU ((++), (!), and, backpermute, concat, fromList, generate, imap, length, map, maximum, minimum, sum, take, toList, Vector, zipWith, empty)
import qualified GHC.Generics as GG (Generic)
import Prelude as P hiding (init, map, maximum, minimum, zipWith)
import qualified Numeric.LinearAlgebra as NL
import qualified Numeric.LinearAlgebra.Devel as NLD


{----- TYPES -----}

data Matrix = Matrix {
    rows    :: Int,
    columns :: Int,
    hmat    :: NL.Matrix Double
} deriving (Eq, GG.Generic, Show)

data Axis = Columns | Rows deriving (Eq, Show)


{----- PATTERNS -----}

validateSize :: NL.Matrix Double -> Maybe (NL.Matrix Double)
validateSize mat = if NL.cols mat == 1 then Just mat else Nothing

pattern ColumnVector size mat <- Matrix size _ (validateSize -> Just mat)

{----- INSTANCES -----}

instance DB.Binary Matrix

{----- EXPORTED METHODS -----}

addMatrices ::  Matrix -> Matrix -> Either String Matrix
addMatrices leftMat rightMat = Right $ Matrix 0 0 (hmat leftMat `NL.add` hmat rightMat)

{-
 -concatenate :: Axis -> Matrix -> Matrix -> Either String Matrix
 -concatenate axis full@FullMatrix{} diagonal@DiagonalMatrix{} = toFull diagonal >>= concatenate axis full
 -concatenate axis diagonal@DiagonalMatrix{} full@FullMatrix{} = toFull diagonal >>= \matrix -> concatenate axis matrix full
 -concatenate Columns firstMatrix@(Matrix firstRows firstColumns _) secondMatrix@(Matrix secondRows secondColumns _)
 -    | firstRows /= secondRows = Left $ "Cannot append columns of matrix " ++ showSize secondMatrix ++ " to matrix " ++ showSize firstMatrix
 -    | otherwise = Right $ Matrix firstRows (firstColumns + secondColumns) $ DVU.concat $ DL.zipWith (DVU.++) (toRows firstMatrix) (toRows secondMatrix)
 -concatenate Rows firstMatrix@(Matrix firstRows firstColumns firstVector) secondMatrix@(Matrix secondRows secondColumns secondVector)
 -    | firstColumns /= secondColumns = Left $ "Cannot append rows of matrix " ++ showSize secondMatrix ++ " to matrix " ++ showSize firstMatrix
 -    | otherwise = Right $ Matrix (firstRows + secondRows) firstColumns $ firstVector DVU.++ secondVector
 -}

{-
 -convolve :: Matrix -> Matrix -> Matrix
 -convolve matrix@(Matrix matrixRows matrixColumns _) kernel@(Matrix kernelRows kernelColumns _) =
 -    generate matrixRows matrixColumns computeConvolutedValue
 -  where
 -    kernelRowsWindow = quot kernelRows 2
 -    kernelColumnsWindow = quot kernelColumns 2
 -    computeConvolutedValue (row, column) = P.sum [
 -        if row - kernelRowsWindow + i < 0 ||
 -           row - kernelRowsWindow + i >= matrixRows ||
 -           column - kernelColumnsWindow + j < 0 ||
 -           column - kernelColumnsWindow + j >= matrixColumns
 -            then 0
 -            else matrix ! (row - kernelRowsWindow + i, column - kernelColumnsWindow + j) * kernel ! (i, j)
 -        | i <- [0 .. kernelRows - 1], j <- [0 .. kernelColumns - 1]
 -        ]
 -}

empty :: Matrix
empty = Matrix 0 0 mempty

equal ::  Double -> Matrix -> Matrix -> Bool
equal precision (Matrix _ _ fhmat) (Matrix _ _ shmat) =  fhmat == shmat

fromList :: Int -> Int -> [Double] -> Either String Matrix
fromList rows columns list
    | (rows == columns && rows == length list) || (rows * columns == length list) = Right $ unsafeFromList rows columns list
    | otherwise = Left "Mismatch between dimensions and list length"

fromVector ::  Int -> Int -> DVU.Vector Double -> Either String Matrix
fromVector rows columns vector = fromList rows columns $ DVU.toList vector

fromHmat :: NL.Matrix Double -> Either String Matrix
fromHmat = Right . Matrix 0 0

imap :: ((Int, Int) -> Double -> Double) -> Matrix -> Matrix
imap function (Matrix rows columns mat) = Matrix rows columns $ NLD.mapMatrixWithIndex function mat

map :: (Double -> Double) -> Matrix -> Matrix
map f (Matrix rows columns mat) = Matrix 0 0 (NL.cmap f mat)

maximum ::  Matrix -> Double
maximum = NL.maxElement . hmat

minimum ::  Matrix -> Double
minimum = NL.minElement . hmat

normalize ::  Maybe Double -> Maybe Double -> Matrix -> Matrix
normalize maybeUpperBound maybeLowerBound matrix = map transform matrix
  where transform  = if upperBound == lowerBound
                       then const 0
                       else \x -> (2 * x - upperBound - lowerBound) / (upperBound - lowerBound)
        upperBound = DM.fromMaybe (maximum matrix) maybeUpperBound
        lowerBound = DM.fromMaybe (minimum matrix) maybeLowerBound

multiplyMatrices :: Matrix -> Matrix -> Either String Matrix
multiplyMatrices leftMat rightMat = Right $ Matrix 0 0 (hmat leftMat NL.<> hmat rightMat)

singleton :: Double -> Matrix
singleton value = unsafeFromList 1 1 [value]

showSize :: Matrix -> String
showSize = show . hmat

sum :: Matrix -> Double
sum = NL.sumElements . hmat

transpose :: Matrix -> Matrix
transpose mat = mat { hmat = NL.tr . hmat $ mat }

unsafeFromList :: Int -> Int -> [Double] -> Matrix
unsafeFromList rows columns list = Matrix rows columns (rows NL.>< columns $ list)

zipWith :: (Double -> Double -> Double) -> Matrix -> Matrix -> Matrix
zipWith f mat1 mat2 = Matrix 0 0 $ NLD.liftMatrix2 (NLD.zipVectorWith f) (hmat mat1) (hmat mat2)
