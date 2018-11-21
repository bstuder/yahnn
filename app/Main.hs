module Main where

import qualified Activation as A
import qualified Data.Vector as V (fromList)
import qualified System.Random as S (mkStdGen)
import qualified Matrix as M
import qualified Network as N
import qualified Utils
import qualified Loss as L

main :: IO ()
main = do
    let network = N.Network [A.ReLu, A.ReLu] [
                M.Matrix 2 3 $ V.fromList [2, -1, 3, 7, -5, 0],
                M.Matrix 1 2 $ V.fromList [3, -1]
            ]
    let forwardResult = N.forward (V.fromList [1, -3, 2]) network
    print forwardResult

    let target = V.fromList [9.5]
    let backwardResult = forwardResult >>= \fr -> N.backward network fr target L.MSE
    print backwardResult
