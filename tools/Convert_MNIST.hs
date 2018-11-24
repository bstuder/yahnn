module Main where

import qualified Data.Binary.Get as DBG
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString as DB
import qualified Data.Serialize as DS
import qualified Data.Vector as DV (fromList)
import qualified Data.Vector.Serialize as DVS

chunksOf :: Int -> [a] -> [[a]]
chunksOf length list
    | length <= 0 = []
    | null list = []
    | otherwise = take length list : chunksOf length (drop length list)

parseData = do
    magicNumber <- DBG.getInt32be
    numberOfItems <- DBG.getInt32be
    rows <- DBG.getInt32be
    columns <- DBG.getInt32be
    rawData <- DBG.getByteString $ fromIntegral (numberOfItems * rows * columns)
    return $! map (DV.fromList) $ chunksOf (fromIntegral (rows * columns)) $ DB.unpack rawData

parseLabels = do
    magicNumber <- DBG.getInt32be
    numberOfItems <- DBG.getInt32be
    rawData <- DBG.getByteString $ fromIntegral numberOfItems
    return $! map (DV.fromList) $ chunksOf 1 $ DB.unpack rawData

main :: IO ()
main = do
    input <- DBL.readFile "data/MNIST/train_data"
    let trainingSet = DBG.runGet parseData input
    DB.writeFile "data/MNIST/train_data_encoded" $ DS.encode trainingSet

    input <- DBL.readFile "data/MNIST/train_labels"
    let trainingSet = DBG.runGet parseLabels input
    DB.writeFile "data/MNIST/train_labels_encoded" $ DS.encode trainingSet

    input <- DBL.readFile "data/MNIST/test_data"
    let trainingSet = DBG.runGet parseData input
    DB.writeFile "data/MNIST/test_data_encoded" $ DS.encode trainingSet

    input <- DBL.readFile "data/MNIST/test_labels"
    let trainingSet = DBG.runGet parseLabels input
    DB.writeFile "data/MNIST/test_labels_encoded" $ DS.encode trainingSet
