module Main where

import qualified Activation as A (Activation(..))
import qualified Data.ByteString.Lazy as DBL (readFile)
import qualified Dataset as D (Dataset(..), fromByteString, normalize, Flag(..))
import qualified Loss as L (Loss(..))
import qualified Network as N (Network(..), random, train)
import qualified Optimizer as O (Optimizer(..))
import qualified System.Random as SR (mkStdGen)

main :: IO ()
main = do
    print "Training the network on MNIST..."
    input <- DBL.readFile "data/MNIST/training_set"
    let generator = SR.mkStdGen 12345

    let result = do
            dataset <- D.normalize D.Datapoints <$> D.fromByteString input
            network <- N.random [784, 300, 10] [A.Sigmoid, A.Sigmoid] generator
            N.train (O.SGD 1 0.01) L.MSE dataset network

    either print (print . fst) result
    print "Training achieved."
