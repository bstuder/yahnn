module Utils where

import qualified Data.Vector as DV (Vector, empty, enumFromStepN)
import qualified Data.Vector.Unboxed as DVU (Vector, null, empty, sum, zipWith, slice, length, init, fromListN)

chunksOf :: Int -> DVU.Vector Double -> DV.Vector (DVU.Vector Double)
chunksOf chunkSize vector
    | chunkSize <= 0 = DV.empty
    | DVU.null vector = DV.empty
    | otherwise = slicer <$> DV.enumFromStepN 0 chunkSize numberOfSlices
  where numberOfSlices    = DVU.length vector `div` chunkSize
        slicer startIndex = DVU.slice startIndex chunkSize vector

dotProduct :: DVU.Vector Double -> DVU.Vector Double -> Double
dotProduct firstVector secondVector = DVU.sum $ DVU.zipWith (*) firstVector secondVector
