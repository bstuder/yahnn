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
    let dataset = [
            DV.fromList [1, -3, 2, 7, 12, 5, 2, 6, -5, 0],
            DV.fromList [8, 1, -5, 4, -3, -8, 4, 4, 0, 7],
            DV.fromList [-1, 7, 3, 0, -5, -3, 7, 6, -1, 4],
            DV.fromList [0, -2, 4, 9, -2, -4, 3, -5, -7, 4],
            DV.fromList [-7, -2, -4, 3, 0, 0, -4, 1, 5, 5]
            ]
    let network = N.fromList [10, 5, 2, 5, 10] [A.ReLu, A.ReLu, A.ReLu, A.ReLu] generator
    let result = N.train (O.SGD 1 0.01) L.MSE dataset dataset <$> network
    print result
