module Main where

import qualified Activation as A
import qualified Data.Vector as V (fromList)
import qualified System.Random as S (mkStdGen)
import qualified Loss as L
import qualified Network as N

main :: IO ()
main = do
    let generator = S.mkStdGen 12345
    let datapoint = V.fromList [1, -3, 2, 7, 12, 5, 2, 6, -5, 0]
    let network = N.fromList [10, 5, 2, 5, 10] [A.ReLu, A.ReLu, A.ReLu, A.ReLu] generator
    let forwardResult = network >>= N.forward datapoint
    print forwardResult

    print $ do
        justForward <- forwardResult
        justNetwork <- network
        N.backward justNetwork justForward datapoint L.MSE