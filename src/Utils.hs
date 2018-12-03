module Utils where

import qualified Data.Vector as DV (cons, drop, empty, fromListN, maximum, minimum, null, take, Vector(..), zipWith)

chunksOf :: Int -> DV.Vector a -> DV.Vector (DV.Vector a)
chunksOf length vector
    | length <= 0 = DV.empty
    | DV.null vector = DV.empty
    | otherwise = DV.take length vector `DV.cons` chunksOf length (DV.drop length vector)

dotProduct :: RealFloat a => DV.Vector a -> DV.Vector a -> a
dotProduct firstVector secondVector = sum $ DV.zipWith (*) firstVector secondVector
