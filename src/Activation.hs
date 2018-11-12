module Activation where

import qualified Data.Vector as V (Vector(..))

data Activation = ReLu | Sigmoid | Sign | TanH deriving (Eq, Show)

forward :: (RealFloat a) => Activation -> V.Vector a -> V.Vector a
forward ReLu = fmap (\x -> if x < 0 then 0 else x)
forward Sign = fmap (\x -> if x < 0 then -1 else 1)
forward TanH = fmap (tanh)

derivate :: (RealFloat a) => Activation -> V.Vector a -> V.Vector a
derivate ReLu = fmap (\x -> if x < 0 then 0 else 1)
