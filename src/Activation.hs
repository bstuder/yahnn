module Activation where

import qualified Matrix as M (applyRow, Matrix(..))
import qualified Data.Vector as V (Vector(..))

reLu ::  (Real a) => M.Matrix a -> V.Vector a
reLu mat = fmap (\x -> if x < 0 then 0 else x) (M.applyRow sum mat) 

sign :: (Real a) => M.Matrix a -> V.Vector a
sign mat = fmap (\x -> if x < 0 then -1 else 1) (M.applyRow sum mat) 