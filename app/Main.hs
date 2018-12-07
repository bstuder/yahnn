module Main where

import qualified Activation as A (Activation(..))
import qualified Data.ByteString.Lazy as DBL (readFile)
import qualified Data.Either as DE (fromRight)
import qualified Data.Time as DT (diffUTCTime, getCurrentTime)
import qualified Dataset as D (fromByteString, normalize, Flag(..))
import qualified Evaluator as E (f1, empty)
import qualified Graphics.Rendering.Chart.Easy as GRCE ((.=), def, layout_title, line, plot)
import qualified Graphics.Rendering.Chart.Backend.Cairo as GRCBC (toFile)
import qualified Loss as L (Loss(..))
import qualified Network as N (evaluateClassification, random, train)
import qualified Optimizer as O (Optimizer(..))
import qualified System.Exit as SE (die)
import qualified System.Random as SR (mkStdGen)

main :: IO ()
main = do
    let lossFileName = "loss"
    let generator = SR.mkStdGen 12345

    putStrLn "Training the network on MNIST..."
    startTime <- DT.getCurrentTime
    input <- DBL.readFile "data/MNIST/training_set"

    let trainingResult = do
            dataset <- D.normalize D.Datapoints <$> D.fromByteString input
            network <- N.random [784, 300, 10] [A.Sigmoid, A.Sigmoid] generator
            N.train (O.SGD 1 0.01) L.MSE network dataset

    either SE.die ((writeFile (lossFileName ++ ".txt")) . show . snd) trainingResult

    GRCBC.toFile GRCE.def (lossFileName ++ ".png") $ do
        let losses = DE.fromRight [] $ snd <$> trainingResult
        GRCE.layout_title GRCE..= "Training loss evolution"
        GRCE.plot (GRCE.line "Loss" [zip [1.. length losses] losses])

    endTime <- DT.getCurrentTime
    putStrLn $ "Loss values have been saved under \"" ++ lossFileName ++".txt\""
    putStrLn $ "Loss evolution chart has been saved under \"" ++ lossFileName ++".png\""
    putStrLn $ "Training performed in " ++ show (DT.diffUTCTime endTime startTime)

    putStrLn "Testing the trained network..."
    startTime <- DT.getCurrentTime
    input <- DBL.readFile "data/MNIST/test_set"

    let evaluationResult = do
            (trainedNetwork, _) <- trainingResult
            dataset <- D.normalize D.Datapoints <$> D.fromByteString input
            N.evaluateClassification trainedNetwork (E.empty 10) dataset

    either SE.die (print . show . E.f1) evaluationResult
    endTime <- DT.getCurrentTime
    putStrLn $ "Testing performed in " ++ show (DT.diffUTCTime endTime startTime)
