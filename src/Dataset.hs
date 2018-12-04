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

import qualified Data.ByteString.Lazy as DBL
import qualified Data.Vector.Unboxed as DV (Unbox)
import qualified Data.Serialize as DS (decodeLazy, encodeLazy, Serialize)
import qualified GHC.Generics as GG (Generic)
import qualified Matrix as M (fromVector, maximum, minimum, normalize, Matrix)


{----- TYPES -----}

data Dataset a = Dataset {
    datapoints :: [M.Matrix a],
    targets    :: [M.Matrix a]
} deriving (Eq, GG.Generic, Show)

data Flag = Both | Datapoints | Targets deriving (Eq, Show)


{----- INSTANCES -----}

instance (DV.Unbox a, DS.Serialize a) => DS.Serialize (Dataset a)

{----- HIDDEN METHODS -----}

normalizeFlag :: (DV.Unbox a, RealFloat a) => Flag -> Flag -> [M.Matrix a] -> [M.Matrix a]
normalizeFlag inputFlag referenceFlag matrices =
    if (inputFlag == referenceFlag) || (inputFlag == Both)
        then M.normalize (Just upperBound) (Just lowerBound) <$> matrices
        else matrices
  where
    upperBound = maximum $ M.maximum <$> matrices
    lowerBound = minimum $ M.minimum <$> matrices


{----- EXPORTED METHODS -----}

fromByteString :: DBL.ByteString -> Either String (Dataset Double)
fromByteString = DS.decodeLazy

fromLists :: [M.Matrix a] -> [M.Matrix a] -> Either String (Dataset a)
fromLists datapoints targets
    | length datapoints /= length targets = Left "Mismatching dimensions between datapoints and targets."
    | otherwise = Right $ Dataset datapoints targets

normalize :: (DV.Unbox a, RealFloat a) => Flag -> Dataset a -> Dataset a
normalize flag (Dataset datapoints targets) = Dataset (normalizeFlag flag Datapoints datapoints) (normalizeFlag flag Targets targets)

toByteString :: Dataset Double -> DBL.ByteString
toByteString = DS.encodeLazy
