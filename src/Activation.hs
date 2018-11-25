module Activation where

import qualified Data.Vector as DV (Vector(..))

data Activation = ReLu | Sigmoid | SoftMax | TanH deriving (Eq, Show)

forward :: (RealFloat a) => Activation -> DV.Vector a -> DV.Vector a
forward ReLu vector =  (\x -> if x < 0 then 0 else x) <$> vector
forward Sigmoid vector = (\x -> 1 / (1 + exp(-x))) <$> vector
forward SoftMax vector = (\x -> (exp x) / normalization) <$> vector
    where normalization = sum $ exp <$> vector
forward TanH vector = tanh <$> vector

derivate :: (RealFloat a) => Activation -> DV.Vector a -> DV.Vector a
derivate ReLu vector = (\x -> if x < 0 then 0 else 1) <$> vector
derivate Sigmoid vector = (\x -> 1 / (2 + exp x + exp (-x))) <$> vector
--derivate SoftMax vector = #TODO
derivate TanH vector = (\x -> 1 - (tanh x) ** 2) <$> vector
