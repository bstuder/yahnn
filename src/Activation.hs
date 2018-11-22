module Activation where

import qualified Data.Vector as V (Vector(..))

data Activation = ReLu | Sigmoid | Sign | TanH deriving (Eq, Show)

forward :: (RealFloat a) => Activation -> V.Vector a -> V.Vector a
forward ReLu = fmap (\x -> if x < 0 then 0 else x)
forward Sigmoid = fmap (\x -> 1 / (1 + exp(-x)))
forward TanH = fmap tanh

derivate :: (RealFloat a) => Activation -> V.Vector a -> V.Vector a
derivate ReLu = fmap (\x -> if x < 0 then 0 else 1)
derivate Sigmoid = fmap (\x -> 1 / (2 + exp(x) + exp(-x)))
derivate TanH = fmap (\x -> 1 - (tanh x) ** 2)
