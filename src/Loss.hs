{-# LANGUAGE Strict, PatternSynonyms #-}

module Loss where

import qualified Activation as A (Activation(..), forward)
import qualified Data.Vector.Unboxed as DVU (map, sum, zipWith)
import qualified Matrix as M (addMatrices, pattern ColumnVector, fromVector, map, Matrix, sum, transpose, zipWith)
import qualified Utils as U (dotProduct)


{----- TYPES -----}

data Loss = CrossEntropy | MSE | NLLSoftMax deriving (Eq, Show)


{----- EXPORTED METHODS -----}

backward :: Loss -> M.Matrix -> M.Matrix -> Either String M.Matrix
backward loss outputMatrix@(M.ColumnVector outputSize outputVector) targetMatrix@(M.ColumnVector targetSize targetVector)
    | outputSize /= targetSize = Left "Mismatching dimensions between output and target"
    | otherwise = case loss of
        CrossEntropy -> if M.sum targetMatrix == 1
            then M.transpose <$> (M.zipWith (*) targetMatrix $ M.map (negate . (1/)) outputMatrix)
            else Left $ "Cross-Entropy loss requires a normalized target"
        MSE -> M.fromVector 1 outputSize $ DVU.map ((/ fromIntegral outputSize) . (*2)) (DVU.zipWith (-) outputVector targetVector)
        NLLSoftMax -> do
            let targetNorm = M.sum targetMatrix
            M.transpose <$> ((M.map (* targetNorm) <$> A.forward A.SoftMax outputMatrix) >>= M.addMatrices targetMatrix)
backward _ _ _ = Left "Wrong matrix types to compute loss backward"

forward :: Loss -> M.Matrix -> M.Matrix -> Either String Double
forward loss outputMatrix@(M.ColumnVector outputSize outputVector) targetMatrix@(M.ColumnVector targetSize targetVector)
    | outputSize /= targetSize = Left "Mismatching dimensions between output and target"
    | otherwise = case loss of
        CrossEntropy -> if M.sum targetMatrix == 1
            then Right $ U.dotProduct targetVector $ DVU.map (negate . log) outputVector
            else Left "Cross-Entropy loss requires a normalized target"
        MSE -> Right $ (/ fromIntegral outputSize) . DVU.sum . DVU.map (**2) $ DVU.zipWith (-) outputVector targetVector
        NLLSoftMax -> A.forward A.SoftMax outputMatrix >>= \output -> forward CrossEntropy output targetMatrix
forward _ _ _ = Left "Wrong matrix types to compute loss forward"
