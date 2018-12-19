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


import qualified Data.Maybe as DM (fromMaybe)
import qualified Numeric.LinearAlgebra as NL
import qualified Data.Binary as DB
import qualified GHC.Generics as GG (Generic)
import qualified Data.ByteString.Lazy as DBL (ByteString)


{----- TYPES -----}

data Dataset = Dataset {
    datapoints :: [NL.Matrix Double],
    targets    :: [NL.Matrix Double]
} deriving (Show, Eq, GG.Generic)

data Flag = Both
          | Datapoints
          | Targets
          deriving (Eq, Show)

{----- INSTANCES -----}

instance DB.Binary Dataset

{----- HIDDEN METHODS -----}

normalizeMatrix ::  Maybe Double -> Maybe Double -> NL.Matrix Double -> NL.Matrix Double
normalizeMatrix maybeUpperBound maybeLowerBound matrix = NL.cmap transform matrix
  where transform  = if upperBound == lowerBound
                       then const 0
                       else \x -> (2 * x - upperBound - lowerBound) / (upperBound - lowerBound)
        upperBound = DM.fromMaybe (NL.maxElement matrix) maybeUpperBound
        lowerBound = DM.fromMaybe (NL.minElement matrix) maybeLowerBound


normalizeFlag :: Flag -> Flag -> [NL.Matrix Double] -> [NL.Matrix Double]
normalizeFlag inputFlag referenceFlag matrices =
    if (inputFlag == referenceFlag) || (inputFlag == Both)
        then normalizeMatrix (Just upperBound) (Just lowerBound) <$> matrices
        else matrices
  where
    upperBound = maximum $ NL.maxElement <$> matrices
    lowerBound = minimum $ NL.minElement <$> matrices


{----- EXPORTED METHODS -----}

fromByteString :: DBL.ByteString -> Either String Dataset
fromByteString = DB.decode

fromLists :: [NL.Matrix Double] -> [NL.Matrix Double] -> Either String Dataset
fromLists datapoints targets
    | length datapoints /= length targets = Left "Mismatching dimensions between datapoints and targets."
    | otherwise = Right $ Dataset datapoints targets

normalize :: Flag -> Dataset -> Dataset
normalize flag (Dataset datapoints targets) = Dataset (normalizeFlag flag Datapoints datapoints) (normalizeFlag flag Targets targets)

toByteString :: Dataset -> DBL.ByteString
toByteString = DB.encode
