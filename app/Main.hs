module Main where

import qualified Activation as A
import qualified Data.Vector as DV (fromList)
import qualified Loss as L
import qualified Network as N
import qualified Optimizer as O
import qualified System.Random as SR (mkStdGen)

main :: IO ()
main = do
    let generator = SR.mkStdGen 12345
    let datapoint = DV.fromList [1, -3, 2, 7, 12, 5, 2, 6, -5, 0]

    let network = N.fromList [10, 5, 2, 5, 10] [A.ReLu, A.ReLu, A.ReLu, A.ReLu] generator

    let forwardResult = network >>= N.forward datapoint
    print forwardResult

    let gradient = do justNetwork <- network
                      justForward <- forwardResult
                      N.backward justNetwork justForward datapoint L.MSE

    print gradient

    let newNetwork = do justNetwork <- network
                        justGradient <- gradient
                        O.apply justNetwork justGradient (O.SGD 1 0.01)
    print newNetwork
