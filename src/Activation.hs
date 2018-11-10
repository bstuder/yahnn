module Activation where

import qualified Data.Vector as V (Vector(..))

reLu ::  (Real a) => V.Vector a -> V.Vector a
reLu = fmap (\x -> if x < 0 then 0 else x)

sign :: (Real a) => V.Vector a -> V.Vector a
sign = fmap (\x -> if x < 0 then -1 else 1)