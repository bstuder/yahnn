{-# LANGUAGE Strict, PatternSynonyms #-}

module Loss where

import qualified Data.Vector.Unboxed as DVU (map, sum, zipWith)
import qualified Matrix as M (pattern ColumnVector, fromVector, map, Matrix, sum, transpose, zipWith)
import qualified Utils as U (dotProduct)


{----- TYPES -----}

data Loss = MSE | CrossEntropy deriving (Eq, Show)


{----- EXPORTED METHODS -----}

backward :: Loss -> M.Matrix -> M.Matrix -> Either String M.Matrix
backward loss outputMatrix@(M.ColumnVector outputSize outputVector) targetMatrix@(M.ColumnVector targetSize targetVector)
    | outputSize /= targetSize = Left "Mismatching dimensions between output and target"
    | otherwise = case loss of
        MSE -> M.fromVector 1 outputSize $ DVU.map ((/ fromIntegral outputSize) . (*2)) (DVU.zipWith (-) outputVector targetVector)
        CrossEntropy -> if M.sum targetMatrix == 1
            then M.transpose <$> (M.zipWith (*) targetMatrix $ M.map (negate . (1/)) outputMatrix)
            else Left $ "Cross-Entropy loss requires a normalized target"
backward _ _ _ = Left "Wrong matrix types to compute loss backward"

forward :: Loss -> M.Matrix -> M.Matrix -> Either String Double
forward loss (M.ColumnVector outputSize outputVector) targetMatrix@(M.ColumnVector targetSize targetVector)
    | outputSize /= targetSize = Left "Mismatching dimensions between output and target"
    | otherwise = case loss of
        MSE -> Right $ (/ fromIntegral outputSize) . DVU.sum . DVU.map (**2) $ DVU.zipWith (-) outputVector targetVector
        CrossEntropy -> if M.sum targetMatrix == 1
            then Right $ U.dotProduct targetVector $ DVU.map (negate . log) outputVector
            else Left "Cross-Entropy loss requires a normalized target"
forward _ _ _ = Left "Wrong matrix types to compute loss forward"
