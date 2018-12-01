module Main where

import qualified Data.Binary.Get as DBG (Get(..), getInt32be, getLazyByteString, runGet)
import qualified Data.ByteString.Lazy as DBL (readFile, unpack, writeFile)
import qualified Dataset as D (Dataset(..), fromLists, toByteString)
import qualified GHC.Word as GW (Word8(..))
import qualified Matrix as M (fromList, Matrix)
import qualified System.Exit as SE (die)

chunksOf :: Int -> [a] -> [[a]]
chunksOf length list
    | length <= 0 = []
    | null list = []
    | otherwise = take length list : chunksOf length (drop length list)

parseDatapoints :: DBG.Get (Either String [M.Matrix GW.Word8])
parseDatapoints = do
    magicNumber <- DBG.getInt32be
    numberOfItems <- DBG.getInt32be
    size <- (*) <$> DBG.getInt32be <*> DBG.getInt32be
    rawData <- DBG.getLazyByteString $ fromIntegral (size * numberOfItems)
    return $! sequence $ M.fromList (fromIntegral size) 1 <$> chunksOf (fromIntegral size) (DBL.unpack rawData)

parseLabels :: DBG.Get (Either String [M.Matrix GW.Word8])
parseLabels = do
    magicNumber <- DBG.getInt32be
    numberOfItems <- DBG.getInt32be
    rawData <- DBG.getLazyByteString $ fromIntegral numberOfItems
    return $! sequence $ M.fromList 1 1 <$> chunksOf 1 (DBL.unpack rawData)

main :: IO ()
main = do
    print "Start trainset conversion."
    input_data <- DBL.readFile "data/MNIST/training_data"
    input_labels <- DBL.readFile "data/MNIST/training_labels"
    let dataset = do
        datapoints <- DBG.runGet parseDatapoints input_data
        labels <- DBG.runGet parseLabels input_labels
        D.fromLists datapoints labels
    either SE.die (DBL.writeFile "data/MNIST/training_set" . D.toByteString) dataset
    print "Trainset conversion finished."

    print "Start testset conversion."
    input_data <- DBL.readFile "data/MNIST/test_data"
    input_labels <- DBL.readFile "data/MNIST/test_labels"
    let dataset = do
        datapoints <- DBG.runGet parseDatapoints input_data
        labels <- DBG.runGet parseLabels input_labels
        D.fromLists datapoints labels
    either SE.die (DBL.writeFile "data/MNIST/test_set" . D.toByteString) dataset
    print "Testset conversion finished."
