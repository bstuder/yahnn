module Main where

import qualified Activation as A
import qualified Data.Vector as DV (fromList)
import qualified Loss as L
import qualified Network as N
import qualified Optimizer as O
import qualified System.Random as SR (mkStdGen)

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

parseImageFile = do
    magic <- getWord32be
    numImages <- getWord32be
    numRows <- getWord32be
    numColumns <- getWord32be
    rawData <- getByteString $ fromIntegral (numImages * numRows * numColumns)
    let vec = BS.unpack rawData
    return $! (magic, numImages, numRows, numColumns, vec)

main :: IO ()
main = do
    input <- BSL.readFile "data/train-images-idx3-ubyte"
    print $ runGet parseImageFile input
