{-# LANGUAGE PatternSynonyms #-}

module Evaluator
(
    ConfusionMatrix,
    empty,
    f1,
    update
) where

import qualified Data.List as DL (zip4)
import qualified Data.Vector.Unboxed as DVU (foldl, maximum, zip)
import qualified Matrix as M (pattern ColumnVector, Matrix)


{----- TYPES -----}

data ConfusionMatrix = ConfusionMatrix {
    falseNegatives :: [Int],
    falsePositives :: [Int],
    trueNegatives :: [Int],
    truePositives :: [Int]
} deriving (Eq, Show)


{----- HIDDEN METHODS -----}

add :: ConfusionMatrix -> ConfusionMatrix -> ConfusionMatrix
add (ConfusionMatrix firstFN firstFP firstTN firstTP) (ConfusionMatrix secondFN secondFP secondTN secondTP) =
    ConfusionMatrix (zipWith (+) firstFN secondFN) (zipWith (+) firstFP secondFP) (zipWith (+) firstTN secondTN) (zipWith (+) firstTP secondTP)


{----- EXPORTED METHODS -----}

f1 :: ConfusionMatrix -> Double
f1 (ConfusionMatrix falseNegatives falsePositives trueNegatives truePositives) =
    (sum $ compute <$> DL.zip4 falseNegatives falsePositives trueNegatives truePositives) / (fromIntegral $ length falseNegatives)
  where
    compute (falseNegative, falsePositive, _, truePositive) = (fromIntegral $ 2 * truePositive) / (fromIntegral $ 2 * truePositive + falsePositive + falseNegative)

empty :: Int -> ConfusionMatrix
empty numberOfClasses = ConfusionMatrix emptyList emptyList emptyList emptyList
  where
    emptyList = replicate numberOfClasses 0

update :: M.Matrix -> M.Matrix -> ConfusionMatrix -> ConfusionMatrix
update (M.ColumnVector _ outputVector) (M.ColumnVector _ targetVector) confusionMatrix =
    add confusionMatrix $ DVU.foldl appendConfusionMatrix (empty 0) $ DVU.zip outputVector targetVector
  where
    appendConfusionMatrix (ConfusionMatrix falseNegatives falsePositives trueNegatives truePositives) (output, target)
        | output == maximum && target == 1 = ConfusionMatrix (falseNegatives ++ [0]) (falsePositives ++ [0]) (trueNegatives ++ [0]) (truePositives ++ [1])
        | output /= maximum && target == 1 = ConfusionMatrix (falseNegatives ++ [1]) (falsePositives ++ [0]) (trueNegatives ++ [0]) (truePositives ++ [0])
        | output == maximum && target /= 1 = ConfusionMatrix (falseNegatives ++ [0]) (falsePositives ++ [1]) (trueNegatives ++ [0]) (truePositives ++ [0])
        | output /= maximum && target /= 1 = ConfusionMatrix (falseNegatives ++ [0]) (falsePositives ++ [0]) (trueNegatives ++ [1]) (truePositives ++ [0])
      where
        maximum = DVU.maximum outputVector
