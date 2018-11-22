module Main where

import qualified Activation as A
import qualified Data.Vector as V (fromList)
import qualified System.Random as S (mkStdGen)
import qualified Loss as L
import qualified Network as N
import qualified Optimizer as O

main :: IO ()
main = do
    let generator = S.mkStdGen 12345
    let datapoint = V.fromList [1, -3, 2, 7, 12, 5, 2, 6, -5, 0]

    let network = N.fromList [10, 5, 2, 5, 10] [A.ReLu, A.ReLu, A.ReLu, A.ReLu] generator
    let forwardResult = network >>= N.forward datapoint
    print forwardResult

    let gradient = network >>= (\justNetwork -> forwardResult >>= (\justForward -> N.backward justNetwork justForward datapoint L.MSE))
    print gradient

    let newNetwork = network >>= (\justNetwork -> gradient >>= (\justGradient -> O.apply justNetwork justGradient O.SGD [1, 0.01]))
    print newNetwork
