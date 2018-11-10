module Activation where

import qualified Data.Vector as V (Vector(..))

reLu ::  (Real a) => V.Vector a -> V.Vector a
reLu vector = fmap (\x -> if x < 0 then 0 else x) vector

sign :: (Real a) => V.Vector a -> V.Vector a
sign vector = fmap (\x -> if x < 0 then -1 else 1) vector