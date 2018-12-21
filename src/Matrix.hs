{-# LANGUAGE DeriveGeneric, PatternSynonyms, ViewPatterns #-}

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

import Prelude as P hiding (init, map, maximum, minimum, zipWith)
import qualified Data.Binary as DB (Binary)
import qualified Data.Maybe as DM (fromMaybe)
import qualified GHC.Generics as GG (Generic)
import qualified Utils as U (equalDouble)
import qualified Numeric.LinearAlgebra as NL
import qualified Numeric.LinearAlgebra.Devel as NLD


{----- TYPES -----}

data Matrix = Matrix {
    rows    :: Int,
    columns :: Int,
    hmat    :: NL.Matrix Double
} deriving (GG.Generic, Show)

instance Eq Matrix where
    m1 == m2 = hmat m1 == hmat m2

data Axis = Columns | Rows deriving (Eq, Show)


{----- PATTERNS -----}

validateSize :: NL.Matrix Double -> Maybe (NL.Matrix Double)
validateSize mat = if NL.cols mat == 1 then Just mat else Nothing

pattern ColumnVector size mat <- Matrix size _ (validateSize -> Just mat)

{----- INSTANCES -----}

instance DB.Binary Matrix

{----- EXPORTED METHODS -----}

addMatrices ::  Matrix -> Matrix -> Either String Matrix
addMatrices leftMat rightMat = Right . Matrix 0 0 $ NL.add (hmat leftMat) (hmat rightMat)

empty :: Matrix
empty = Matrix 0 0 mempty

equal ::  Double -> Matrix -> Matrix -> Bool
equal precision (Matrix _ _ fhmat) (Matrix _ _ shmat)
    | NL.size fhmat /= NL.size shmat = False
    | otherwise = NLD.foldVector (&&) True $ NLD.zipVectorWith (U.equalDouble precision) (NL.flatten fhmat) (NL.flatten shmat)

fromList :: Int -> Int -> [Double] -> Either String Matrix
fromList rows columns list
    | (rows == columns && rows == length list) || (rows * columns == length list) = Right $ unsafeFromList rows columns list
    | otherwise = Left "Mismatch between dimensions and list length"

fromHmat :: NL.Matrix Double -> Either String Matrix
fromHmat = Right . Matrix 0 0

imap :: ((Int, Int) -> Double -> Double) -> Matrix -> Matrix
imap function (Matrix rows columns mat) = Matrix rows columns $ NLD.mapMatrixWithIndex function mat

map :: (Double -> Double) -> Matrix -> Matrix
map f (Matrix _ _ mat) = Matrix 0 0 (NL.cmap f mat)

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
transpose (Matrix _ _ hmat) = Matrix 0 0 $ NL.tr' hmat

unsafeFromList :: Int -> Int -> [Double] -> Matrix
unsafeFromList rows columns list
    | rows * columns == length list = Matrix rows columns $ (rows NL.>< columns) list
    | otherwise                     = Matrix rows rows $ NL.diagl list

zipWith :: (Double -> Double -> Double) -> Matrix -> Matrix -> Matrix
zipWith f mat1 mat2 = Matrix 0 0 $ NLD.liftMatrix2 (NLD.zipVectorWith f) (hmat mat1) (hmat mat2)
