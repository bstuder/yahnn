{-# LANGUAGE Strict, PatternSynonyms #-}

module Loss where

import qualified Activation as A (Activation(..), forward)
import qualified Matrix as M (addMatrices, pattern ColumnVector, fromHmat, map, Matrix, sum, transpose, zipWith)
import qualified Numeric.LinearAlgebra as NL


{----- TYPES -----}

data Loss = CrossEntropy | MSE | NLLSoftMax deriving (Eq, Show)


{----- EXPORTED METHODS -----}

backward :: Loss -> M.Matrix -> M.Matrix -> Either String M.Matrix
backward loss outputMatrix@(M.ColumnVector _ outputVector) targetMatrix@(M.ColumnVector _ targetVector)
    | NL.size outputVector /= NL.size targetVector = Left "Mismatching dimensions between output and target"
    | otherwise = case loss of
        CrossEntropy -> if M.sum targetMatrix == 1
            then Right $ M.transpose $ M.zipWith (*) targetMatrix $ M.map (negate . (1 /)) outputMatrix
            else Left "Cross-Entropy loss requires a normalized target"
        MSE -> M.fromHmat . NL.tr' $ NL.cmap ((/ fromIntegral (NL.rows outputVector)) . (*2)) (outputVector - targetVector)
        NLLSoftMax -> do
            let targetNorm = M.sum targetMatrix
            M.transpose <$> ((M.map (* targetNorm) <$> A.forward A.SoftMax outputMatrix) >>= M.addMatrices (M.map negate targetMatrix))
backward _ _ _ = Left "Wrong matrix types to compute loss backward"

forward :: Loss -> M.Matrix -> M.Matrix -> Either String Double
forward loss outputMatrix@(M.ColumnVector _ outputVector) targetMatrix@(M.ColumnVector _ targetVector)
    | NL.size outputVector /= NL.size targetVector = Left "Mismatching dimensions between output and target"
    | otherwise = case loss of
        CrossEntropy -> if M.sum targetMatrix == 1
            then Right $ NL.flatten targetVector NL.<.> (NL.flatten . NL.cmap (negate . log) $ outputVector)
            else Left "Cross-Entropy loss requires a normalized target"
        MSE -> Right $ (/ fromIntegral (NL.rows outputVector)) . NL.sumElements . NL.cmap (**2) $ outputVector - targetVector
        NLLSoftMax -> A.forward A.SoftMax outputMatrix >>= \output -> forward CrossEntropy output targetMatrix
forward _ _ _ = Left "Wrong matrix types to compute loss forward"
