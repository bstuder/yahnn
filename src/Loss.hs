{-# LANGUAGE Strict, PatternSynonyms #-}

module Loss where

import qualified Data.Vector.Unboxed as DV (Unbox, zipWith, map, sum)
import qualified Matrix as M (pattern ColumnVector, empty, fromVector, Matrix)

data Loss = MSE deriving (Eq, Show)

forward :: (DV.Unbox a, RealFloat a) => Loss -> M.Matrix a -> M.Matrix a -> Either String a
forward MSE (M.ColumnVector outputSize outputVector) (M.ColumnVector targetSize targetVector)
    | outputSize /= targetSize = Left "Mismatching dimensions between output and target"
    | otherwise = Right $ (/ fromIntegral outputSize) . DV.sum . DV.map (**2) $ DV.zipWith (-) outputVector targetVector
forward MSE _ _ = Left "Wrong matrix types to compute loss forward"

backward :: (DV.Unbox a, RealFloat a) => Loss -> M.Matrix a -> M.Matrix a -> Either String (M.Matrix a)
backward MSE (M.ColumnVector outputSize outputVector) (M.ColumnVector targetSize targetVector)
    | outputSize /= targetSize = Left "Mismatching dimensions between output and target"
    | otherwise = M.fromVector 1 outputSize $ DV.map ((/ fromIntegral outputSize) . (*2)) (DV.zipWith (-) outputVector targetVector)
backward MSE _ _ = Left "Wrong matrix types to compute loss backward"
