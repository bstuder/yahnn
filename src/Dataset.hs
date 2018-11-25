{-# LANGUAGE DeriveGeneric #-}

module Dataset where

import qualified Data.ByteString.Lazy as DBL
import qualified Data.Serialize as DS (decodeLazy, encodeLazy, Serialize)
import qualified Data.Vector as DV (length, Vector(..))
import qualified Data.Vector.Serialize as DVS (genericGetVector, genericPutVector)
import qualified GHC.Generics as GG (Generic(..))

data Dataset a = Dataset {
    datapoints :: [DV.Vector a],
    targets :: [DV.Vector a]
} deriving (Eq, GG.Generic, Show)

instance (DS.Serialize a) => DS.Serialize (Dataset a)

{-normalize :: (RealFloat a) => Dataset a -> Dataset a
normalize dataset = ...-}

fromByteString :: (RealFloat a, DS.Serialize a) => DBL.ByteString -> Either String (Dataset a)
fromByteString byteString = do
    (Dataset datapoints targets) <- DS.decodeLazy byteString
    return $ Dataset (convertList datapoints) (convertList targets)
    where convertList = fmap (fromInteger <$>)

fromLists :: [DV.Vector a] -> [DV.Vector a] -> Either String (Dataset a)
fromLists datapoints targets
    | length datapoints /= length targets = Left "Mismatching dimensions between datapoints and targets."
    | otherwise = Right $ Dataset datapoints targets

toByteString :: (DS.Serialize a) => Dataset a -> DBL.ByteString
toByteString = DS.encodeLazy