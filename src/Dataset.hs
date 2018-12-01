{-# LANGUAGE DeriveGeneric, PatternSynonyms #-}

module Dataset where

import qualified Data.ByteString.Lazy as DBL
import qualified Data.Serialize as DS (decodeLazy, encodeLazy, Serialize)
import qualified GHC.Generics as GG (Generic(..))
import qualified Matrix as M (pattern ColumnVector, fromVector, maximum, minimum, Matrix)

data Dataset a = Dataset {
    datapoints :: [M.Matrix a],
    targets :: [M.Matrix a]
} deriving (Eq, GG.Generic, Show)

instance (DS.Serialize a) => DS.Serialize (Dataset a)

data NormalizationFlag = Both | Datapoints | Targets deriving (Eq, Show)

fromByteString :: (RealFloat a, DS.Serialize a) => DBL.ByteString -> Either String (Dataset a)
fromByteString byteString = do
    (Dataset datapoints targets) <- DS.decodeLazy byteString
    return $ Dataset (convertList datapoints) (convertList targets)
    where convertList = fmap (fromInteger <$>)

fromLists :: [M.Matrix a] -> [M.Matrix a] -> Either String (Dataset a)
fromLists datapoints targets
    | length datapoints /= length targets = Left "Mismatching dimensions between datapoints and targets."
    | otherwise = Right $ Dataset datapoints targets

normalize :: RealFloat a => NormalizationFlag -> Dataset a -> Dataset a
normalize flag (Dataset datapoints targets) = Dataset newDatapoints newTargets
  where
    newDatapoints = if (flag == Both) || (flag == Datapoints)
        then normalizeMatrices datapoints
        else datapoints
    newTargets = if (flag == Both) || (flag == Targets)
        then normalizeMatrices targets
        else targets
    normalizeMatrices matrices = normalizeMatrix upperBound lowerBound <$> matrices
      where
        upperBound = maximum $ M.maximum <$> matrices
        lowerBound = minimum $ M.minimum <$> matrices
        normalizeMatrix upperBound lowerBound matrix =
            if upperBound == lowerBound
                then const 1 <$> matrix
                else (\x -> (2 * x - upperBound - lowerBound) / (upperBound - lowerBound)) <$> matrix

toByteString :: (DS.Serialize a) => Dataset a -> DBL.ByteString
toByteString = DS.encodeLazy
