module Main where

import qualified Activation as A (Activation(..))
import qualified Data.ByteString.Lazy as DBL (readFile)
import qualified Data.Vector as DV (fromList)
import qualified Dataset as D (Dataset(..), fromByteString, normalize, NormalizationFlag(..))
import qualified Loss as L (Loss(..))
import qualified Network as N (fromList, Network(..), train)
import qualified Optimizer as O (Optimizer(..))
import qualified System.Exit as SE (die)
import qualified System.Random as SR (mkStdGen)

main :: IO ()
main = do
    print "Just do it !"

    {-print "Read the training set..."
    let dataset = do
            input <- DBL.readFile "data/MNIST/training_set"
            return $ D.normalize D.Datapoints $ D.fromByteString input

    print "Train the network..."
    let generator = SR.mkStdGen 12345
    let (trainedNetwork, losses) = do
            network <- N.fromList [28 * 28, 300, 10] [A.Sigmoid, A.Sigmoid, A.Sigmoid] generator
            return $ N.train (O.SGD 1 0.01) L.MSE dataset network

    print "Finished..."-}
