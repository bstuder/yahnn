{-# LANGUAGE PatternSynonyms #-}

module Evaluator where

import qualified Matrix as M (pattern ColumnVector, Matrix)
import qualified Data.Vector as DV (elemIndex, maximum)


{----- TYPES -----}

data ConfusionMatrix = ConfusionMatrix {
    falseNegatives :: Int,
    falsePositives :: Int,
    trueNegatives :: Int,
    truePositives :: Int
} deriving (Eq, Show)

data ClassificationMatrix = ClassificationMatrix {
    fails :: Int,
    successes :: Int
} deriving (Eq, Show)


{----- EXPORTED METHODS -----}

computeF1 :: ConfusionMatrix -> Double
computeF1 confusionMatrix = 0

computeClassificationRate :: ClassificationMatrix -> Double
computeClassificationRate (ClassificationMatrix fails successes) = (fromIntegral successes) / (fromIntegral $ fails + successes)

empty :: ClassificationMatrix
empty = ClassificationMatrix 0 0

updateClassificationMatrix :: RealFloat a => M.Matrix a -> M.Matrix a -> ClassificationMatrix -> ClassificationMatrix
updateClassificationMatrix (M.ColumnVector outputSize outputVector) (M.ColumnVector targetSize targetVector) (ClassificationMatrix fails successes) =
    if DV.elemIndex (DV.maximum outputVector) outputVector == DV.elemIndex (DV.maximum targetVector) targetVector
        then ClassificationMatrix fails (successes + 1)
        else ClassificationMatrix (fails + 1) successes
