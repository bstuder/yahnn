module Main where

import qualified Data.Binary.Get as DBG (Get, getInt32be, getLazyByteString, runGet)
import qualified Data.ByteString.Lazy as DBL (ByteString, readFile, unpack, writeFile)
import qualified Data.Int as DI (Int32)
import qualified Dataset as D (Dataset, fromLists, toByteString)
import qualified GHC.Word as GW (Word8)
import qualified Matrix as M (fromList, Matrix)
import qualified System.Exit as SE (die)

chunksOf :: Int -> [a] -> [[a]]
chunksOf length list
    | length <= 0 = []
    | null list = []
    | otherwise = take length list : chunksOf length (drop length list)

toOneHot :: (Num a, Ord a) => a -> a -> [Double]
toOneHot 0 value = []
toOneHot size value = toOneHot (size - 1) value ++ [if value + 1 == size then 1.0 else 0.0] 

parseDatapoints :: Maybe DI.Int32 -> DBG.Get (Either String [M.Matrix])
parseDatapoints threshold = do
    magicNumber <- DBG.getInt32be
    items <- DBG.getInt32be
    let numberOfItems = maybe (fromIntegral items) (min (fromIntegral items)) threshold
    itemSize <- (*) <$> DBG.getInt32be <*> DBG.getInt32be
    rawData <- DBG.getLazyByteString $ fromIntegral (itemSize * numberOfItems)
    return $! sequence $ M.fromList (fromIntegral itemSize) 1 <$> chunksOf (fromIntegral itemSize) (fromIntegral <$> DBL.unpack rawData)

parseLabels :: Maybe DI.Int32 -> DBG.Get (Either String [M.Matrix])
parseLabels threshold = do
    magicNumber <- DBG.getInt32be
    items <- DBG.getInt32be
    let numberOfItems = maybe (fromIntegral items) (min (fromIntegral items)) threshold
    rawData <- DBG.getLazyByteString $ fromIntegral numberOfItems
    return $! sequence $ M.fromList 10 1 . toOneHot 10 <$> DBL.unpack rawData

convertMNIST :: Maybe DI.Int32 -> String -> String -> String -> String -> IO ()
convertMNIST threshold baseFolder dataFileName labelFileName outputFileName = do
    input_data <- DBL.readFile (baseFolder <> dataFileName)
    input_labels <- DBL.readFile (baseFolder <> labelFileName)
    let dataset = do
            datapoints <- DBG.runGet (parseDatapoints threshold) input_data
            labels <- DBG.runGet (parseLabels threshold) input_labels
            D.fromLists datapoints labels
    either SE.die (DBL.writeFile (baseFolder <> outputFileName) . D.toByteString) dataset

main :: IO ()
main = do
    let baseFolder = "data/MNIST/"

    print "Start trainset conversion."
    convertMNIST (Just 5000) baseFolder "training_data" "training_labels" "training_set"
    print "Trainset conversion finished."

    print "Start testset conversion."
    convertMNIST Nothing baseFolder "test_data" "test_labels" "test_set"
    print "Testset conversion finished."
