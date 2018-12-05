module Main where

import qualified Activation as A (Activation(..))
import qualified Data.ByteString.Lazy as DBL (readFile)
import qualified Data.Either as DE (fromRight)
import qualified Data.Time as DT (diffUTCTime, getCurrentTime)
import qualified Dataset as D (Dataset(..), fromByteString, normalize, Flag(..))
import qualified Graphics.Rendering.Chart.Easy as GRCE ((.=), def, layout_title, line, plot)
import qualified Graphics.Rendering.Chart.Backend.Cairo as GRCBC (toFile)
import qualified Loss as L (Loss(..))
import qualified Network as N (Network(..), random, train)
import qualified Optimizer as O (Optimizer(..))
import qualified System.Exit as SE (die)
import qualified System.Random as SR (mkStdGen)

import qualified Matrix as M (sum)

main :: IO ()
main = do
    let lossFileName = "loss"
    let generator = SR.mkStdGen 12345

    putStrLn "Training the network on MNIST..."
    startTime <- DT.getCurrentTime
    input <- DBL.readFile "data/MNIST/training_set"

    let result = do
            dataset <- D.normalize D.Datapoints <$> D.fromByteString input
            network <- N.random [784, 300, 10] [A.Sigmoid, A.SoftMax] generator
            N.train (O.SGD 1 0.01) L.CrossEntropy network dataset

    either SE.die ((writeFile (lossFileName ++ ".txt")) . show . snd) result

    GRCBC.toFile GRCE.def (lossFileName ++ ".png") $ do
        let losses = DE.fromRight [] $ snd <$> result
        GRCE.layout_title GRCE..= "Training loss evolution"
        GRCE.plot (GRCE.line "Loss" [zip [1.. length losses] losses])

    endTime <- DT.getCurrentTime
    putStrLn $ "Training achieved in " ++ show (DT.diffUTCTime endTime startTime)
    putStrLn $ "Loss values have been saved under \"" ++ lossFileName ++".txt\""
    putStrLn $ "Loss evolution chart has been saved under \"" ++ lossFileName ++".png\""