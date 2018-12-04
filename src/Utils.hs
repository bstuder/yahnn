module Utils where

import qualified Data.Vector as DVB (Vector(..), cons, empty)
import qualified Data.Vector.Unboxed as DV (Vector(..), Unbox, cons, drop, empty, fromListN, maximum, minimum, null, take, zipWith, sum)
import qualified System.Random as SR (randomRs, StdGen(..))

chunksOf :: Int -> DV.Vector Double -> DVB.Vector (DV.Vector Double)
chunksOf length vector
    | length <= 0 = DVB.empty
    | DV.null vector = DVB.empty
    | otherwise = DV.take length vector `DVB.cons` chunksOf length (DV.drop length vector)

dotProduct :: DV.Vector Double -> DV.Vector Double -> Double
dotProduct firstVector secondVector = DV.sum $ DV.zipWith (*) firstVector secondVector

generateVector :: Int -> SR.StdGen -> DV.Vector Double
generateVector n = DV.fromListN n . SR.randomRs (-1.0, 1.0)
