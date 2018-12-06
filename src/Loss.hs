{-# LANGUAGE Strict, PatternSynonyms #-}

module Loss where

import qualified Data.Vector.Unboxed as DVU (Unbox, zipWith, map, sum)
import qualified Matrix as M (pattern ColumnVector, empty, fromVector, Matrix)

data Loss = MSE deriving (Eq, Show)

forward :: Loss -> M.Matrix -> M.Matrix -> Either String Double
forward MSE (M.ColumnVector outputSize outputVector) (M.ColumnVector targetSize targetVector)
    | outputSize /= targetSize = Left "Mismatching dimensions between output and target"
    | otherwise = Right $ (/ fromIntegral outputSize) . DVU.sum . DVU.map (**2) $ DVU.zipWith (-) outputVector targetVector
forward MSE _ _ = Left "Wrong matrix types to compute loss forward"

backward :: Loss -> M.Matrix -> M.Matrix -> Either String M.Matrix
backward MSE (M.ColumnVector outputSize outputVector) (M.ColumnVector targetSize targetVector)
    | outputSize /= targetSize = Left "Mismatching dimensions between output and target"
    | otherwise = M.fromVector 1 outputSize $ DVU.map ((/ fromIntegral outputSize) . (*2)) (DVU.zipWith (-) outputVector targetVector)
backward MSE _ _ = Left "Wrong matrix types to compute loss backward"
