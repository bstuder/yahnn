module Utils where

import qualified Data.Vector as DV (empty, enumFromStepN, Vector)
import qualified Data.Vector.Unboxed as DVU (length, null, slice, sum, Vector, zipWith)

chunksOf :: Int -> DVU.Vector Double -> DV.Vector (DVU.Vector Double)
chunksOf chunkSize vector
    | chunkSize <= 0 = DV.empty
    | DVU.null vector = DV.empty
    | otherwise = slicer <$> DV.enumFromStepN 0 chunkSize numberOfSlices
  where numberOfSlices    = DVU.length vector `div` chunkSize
        slicer startIndex = DVU.slice startIndex chunkSize vector

dotProduct :: DVU.Vector Double -> DVU.Vector Double -> Double
dotProduct firstVector secondVector = DVU.sum $ DVU.zipWith (*) firstVector secondVector

equalDouble :: Double -> Double -> Double -> Bool
equalDouble precision firstDouble secondeDouble = firstDouble == secondeDouble || abs (firstDouble - secondeDouble) / max (abs firstDouble) (abs secondeDouble) <= precision
