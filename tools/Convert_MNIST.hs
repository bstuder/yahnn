module Main where

import qualified Data.Binary.Get as DBG
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Vector as DV (fromList, singleton, Vector(..))
import qualified Dataset as D (Dataset(..), toByteString)
import qualified GHC.Word as GW (Word8(..))

chunksOf :: Int -> [a] -> [[a]]
chunksOf length list
    | length <= 0 = []
    | null list = []
    | otherwise = take length list : chunksOf length (drop length list)

parseData :: DBG.Get [DV.Vector GW.Word8]
parseData = do
    magicNumber <- DBG.getInt32be
    numberOfItems <- DBG.getInt32be
    rows <- DBG.getInt32be
    columns <- DBG.getInt32be
    rawData <- DBG.getLazyByteString $ fromIntegral (numberOfItems * rows * columns)
    return $! fmap (DV.fromList) $ chunksOf (fromIntegral (rows * columns)) $ DBL.unpack rawData

parseLabels :: DBG.Get [DV.Vector GW.Word8]
parseLabels = do
    magicNumber <- DBG.getInt32be
    numberOfItems <- DBG.getInt32be
    rawData <- DBG.getLazyByteString $ fromIntegral numberOfItems
    return $! fmap (DV.singleton) $ DBL.unpack rawData

main :: IO ()
main = do
    input <- DBL.readFile "data/MNIST/train_data"
    let datapoints = DBG.runGet parseData input
    input <- DBL.readFile "data/MNIST/train_labels"
    let labels = DBG.runGet parseLabels input
    let dataset = D.Dataset datapoints labels
    DBL.writeFile "data/MNIST/trainset" $ D.toByteString dataset

    input <- DBL.readFile "data/MNIST/test_data"
    let datapoints = DBG.runGet parseData input
    input <- DBL.readFile "data/MNIST/test_labels"
    let labels = DBG.runGet parseLabels input
    let dataset = D.Dataset datapoints labels
    DBL.writeFile "data/MNIST/testset" $ D.toByteString dataset
