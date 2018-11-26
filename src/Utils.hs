module Utils where

import qualified Data.Vector as DV (cons, drop, empty, fromListN, maximum, minimum, null, take, Vector(..), zipWith)
import qualified System.Random as SR (randomRs, StdGen(..))

chunksOf :: Int -> DV.Vector a -> DV.Vector (DV.Vector a)
chunksOf length vector
    | length <= 0 = DV.empty
    | DV.null vector = DV.empty
    | otherwise = DV.take length vector `DV.cons` chunksOf length (DV.drop length vector)

dotProduct :: RealFloat a => DV.Vector a -> DV.Vector a -> a
dotProduct firstVector secondVector = sum $ DV.zipWith (*) firstVector secondVector

generateVector :: Int -> SR.StdGen -> DV.Vector Double
generateVector n = DV.fromListN n . SR.randomRs (-1.0, 1.0)

normalizeVector :: (RealFloat a) => DV.Vector a -> DV.Vector a
normalizeVector vector =
    if  maximum == minimum
        then vector
        else (\x -> (2 * x - maximum - minimum) / (maximum - minimum)) <$> vector
    where
        maximum = DV.maximum vector
        minimum = DV.minimum vector
