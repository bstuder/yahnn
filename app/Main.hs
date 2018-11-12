module Main where

import qualified Activation as A
import qualified Data.Vector as V (fromList)
import qualified System.Random as S (mkStdGen)
import qualified Matrix as M
import qualified Network as N
import qualified Utils

main :: IO ()
main = do
    let network = N.Network [A.ReLu, A.ReLu] [
                M.Matrix 2 3 $ V.fromList [2, -1, 3, 7, -5, 0],
                M.Matrix 1 2 $ V.fromList [3, -1]
            ]
    print $ N.forward (V.fromList [1, -3, 2]) network
