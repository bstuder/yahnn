{-# LANGUAGE DeriveGeneric #-}

module Dataset
(
    Dataset(..),
    Flag(..),

    fromByteString,
    fromLists,
    normalize,
    toByteString
) where

import qualified Data.ByteString.Lazy as DBL (ByteString)
import qualified Data.Serialize as DS (decodeLazy, encodeLazy, Serialize)
import qualified Data.Vector as DV (fromList, maximum, minimum, Vector)
import qualified GHC.Generics as GG (Generic)
import qualified Matrix as M (Matrix, maximum, minimum, normalize)


{----- TYPES -----}

data Dataset = Dataset {
    datapoints :: DV.Vector M.Matrix,
    targets    :: DV.Vector M.Matrix
} deriving (Eq, GG.Generic, Show)

data Flag = Both | Datapoints | Targets deriving (Eq, Show)


{----- INSTANCES -----}

instance DS.Serialize Dataset


{----- HIDDEN METHODS -----}

normalizeFlag :: Flag -> Flag -> DV.Vector M.Matrix -> DV.Vector M.Matrix
normalizeFlag inputFlag referenceFlag matrices =
    if (inputFlag == referenceFlag) || (inputFlag == Both)
        then M.normalize (Just upperBound) (Just lowerBound) <$> matrices
        else matrices
  where
    upperBound = DV.maximum $ M.maximum <$> matrices
    lowerBound = DV.minimum $ M.minimum <$> matrices


{----- EXPORTED METHODS -----}

fromByteString :: DBL.ByteString -> Either String Dataset
fromByteString = DS.decodeLazy

fromLists :: [M.Matrix] -> [M.Matrix] -> Either String Dataset
fromLists datapoints targets
    | length datapoints /= length targets = Left "Mismatching dimensions between datapoints and targets."
    | otherwise = Right $ Dataset (DV.fromList datapoints) (DV.fromList targets)

normalize :: Flag -> Dataset -> Dataset
normalize flag (Dataset datapoints targets) = Dataset (normalizeFlag flag Datapoints datapoints) (normalizeFlag flag Targets targets)

toByteString :: Dataset -> DBL.ByteString
toByteString = DS.encodeLazy
