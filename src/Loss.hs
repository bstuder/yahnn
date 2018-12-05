{-# LANGUAGE Strict, PatternSynonyms #-}

module Loss where

import qualified Data.Vector as DV (imap, zipWith)
import qualified Matrix as M (pattern ColumnVector, empty, fromVector, imap, Matrix, sum, zipWith)
import qualified Utils as U (chunksOf, dotProduct)

{----- TYPES -----}

data Loss = MSE | CrossEntropy deriving (Eq, Show)


{----- EXPORTED METHODS -----}

backward :: RealFloat a => Loss -> M.Matrix a -> M.Matrix a -> Either String (M.Matrix a)
backward loss outputMatrix@(M.ColumnVector outputSize outputVector) targetMatrix@(M.ColumnVector targetSize targetVector)
    | outputSize /= targetSize = Left "Mismatching dimensions between output and target"
    | otherwise = case loss of
        MSE -> M.fromVector 1 outputSize $ (/ fromIntegral outputSize) . (*2) <$> DV.zipWith (-) outputVector targetVector
        CrossEntropy -> if M.sum targetMatrix == 1
            then M.zipWith (*) targetMatrix $ M.imap (\_ value -> -1 / value) outputMatrix
            else Left "Cross-Entropy loss requires the target to be normalized"
backward _ _ _ = Left "Wrong matrix types to compute loss backward"

forward :: RealFloat a => Loss -> M.Matrix a -> M.Matrix a -> Either String a
forward loss outputMatrix@(M.ColumnVector outputSize outputVector) targetMatrix@(M.ColumnVector targetSize targetVector)
    | outputSize /= targetSize = Left "Mismatching dimensions between output and target"
    | otherwise = case loss of
        MSE -> Right $ (/ fromIntegral outputSize) . sum . fmap (**2) $ DV.zipWith (-) outputVector targetVector
        CrossEntropy -> if M.sum targetMatrix == 1
            then Right $ U.dotProduct (negate . log <$> outputVector) targetVector
            else Left "Cross-Entropy loss requires the target to be normalized"
forward _ _ _ = Left "Wrong matrix types to compute loss forward"
