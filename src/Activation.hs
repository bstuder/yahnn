{-# LANGUAGE Strict, PatternSynonyms #-}

module Activation where

import qualified Data.Vector as DV (Vector(..))
import qualified Matrix as M (pattern ColumnVector, empty, fromVector, Matrix)

data Activation = ReLu | Sigmoid | SoftMax | TanH deriving (Eq, Show)

forward :: RealFloat a => Activation -> M.Matrix a -> Either String (M.Matrix a)
forward ReLu (M.ColumnVector size vector) = M.fromVector size 1 $ (\x -> if x < 0 then 0 else x) <$> vector
forward Sigmoid (M.ColumnVector size vector) = M.fromVector size 1 $ (\x -> 1 / (1 + exp(-x))) <$> vector
forward SoftMax (M.ColumnVector size vector) = M.fromVector size 1 $ (\x -> (exp x) / normalization) <$> vector
    where normalization = sum $ exp <$> vector
forward TanH (M.ColumnVector size vector) = M.fromVector size 1 $ tanh <$> vector

backward :: RealFloat a => Activation -> M.Matrix a -> Either String (M.Matrix a)
backward ReLu (M.ColumnVector size vector) = M.fromVector size size $ (\x -> if x < 0 then 0 else 1) <$> vector
backward Sigmoid (M.ColumnVector size vector) = M.fromVector size size $ (\x -> 1 / (2 + exp x + exp (-x))) <$> vector
--derivate SoftMax vector = #TODO
backward TanH (M.ColumnVector size vector) = M.fromVector size size $ (\x -> 1 - (tanh x) ** 2) <$> vector
