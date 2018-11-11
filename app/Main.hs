module Main where

import qualified Activation as A
import qualified System.Random as S (mkStdGen)
import qualified Matrix as M
import qualified Network as N
import qualified Utils

main :: IO ()
main = print $ N.fromList [3, 2, 1] [A.ReLu, A.Sign] (S.mkStdGen 65498465465)
