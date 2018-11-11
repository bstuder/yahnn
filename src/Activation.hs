module Activation where

import qualified Data.Vector as V (Vector(..))

data Activation = ReLu | Sigmoid | Sign | TanH deriving (Eq, Show)

activationForward :: (Real a) => Activation -> V.Vector a -> V.Vector a
activationForward ReLu = fmap (\x -> if x < 0 then 0 else x)
activationForward Sign = fmap (\x -> if x < 0 then -1 else 1)

activationDerivate :: (Real a) => Activation -> V.Vector a -> V.Vector a
activationDerivate ReLu = fmap (\x -> if x < 0 then 0 else 1)
