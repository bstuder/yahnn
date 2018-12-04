module Utils where

import qualified System.Random as SR (randomRs, StdGen(..))
import qualified Data.Vector as DVB (Vector, empty, enumFromStepN)
import qualified Data.Vector.Unboxed as DV (Vector, null, empty, sum, zipWith, slice, length, init, fromListN)

chunksOf :: Int -> DV.Vector Double -> DVB.Vector (DV.Vector Double)
chunksOf chunkSize vector
    | chunkSize <= 0 = DVB.empty
    | DV.null vector = DVB.empty
    | otherwise = slicer <$> DVB.enumFromStepN 0 chunkSize numberOfSlices
  where numberOfSlices    = DV.length vector `div` chunkSize
        slicer startIndex = DV.slice startIndex chunkSize vector

dotProduct :: DV.Vector Double -> DV.Vector Double -> Double
dotProduct firstVector secondVector = DV.sum $ DV.zipWith (*) firstVector secondVector

generateVector :: Int -> SR.StdGen -> DV.Vector Double
generateVector n = DV.fromListN n . SR.randomRs (-1.0, 1.0)
