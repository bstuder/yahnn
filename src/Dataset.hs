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

import qualified Data.Binary as DB (Binary, encode, decode)
import qualified Data.ByteString.Lazy as DBL (ByteString)
import qualified GHC.Generics as GG (Generic)
import qualified Matrix as M (Matrix, maximum, minimum, normalize)
import qualified Data.List as DL (maximum, minimum)


{----- TYPES -----}

data Dataset = Dataset {
    datapoints :: [M.Matrix],
    targets    :: [M.Matrix]
} deriving (Eq, GG.Generic, Show)

data Flag = Both | Datapoints | Targets deriving (Eq, Show)


{----- INSTANCES -----}

instance DB.Binary Dataset


{----- HIDDEN METHODS -----}

normalizeFlag :: Flag -> Flag -> [M.Matrix] -> [M.Matrix]
normalizeFlag inputFlag referenceFlag matrices =
    if (inputFlag == referenceFlag) || (inputFlag == Both)
        then M.normalize (Just upperBound) (Just lowerBound) <$> matrices
        else matrices
  where
    upperBound = DL.maximum $ M.maximum <$> matrices
    lowerBound = DL.minimum $ M.minimum <$> matrices


{----- EXPORTED METHODS -----}

fromByteString :: DBL.ByteString -> Dataset
fromByteString = DB.decode

fromLists :: [M.Matrix] -> [M.Matrix] -> Either String Dataset
fromLists datapoints targets
    | length datapoints /= length targets = Left "Mismatching dimensions between datapoints and targets."
    | otherwise = Right $ Dataset datapoints targets

normalize :: Flag -> Dataset -> Dataset
normalize flag (Dataset datapoints targets) = Dataset (normalizeFlag flag Datapoints datapoints) (normalizeFlag flag Targets targets)

toByteString :: Dataset -> DBL.ByteString
toByteString = DB.encode
