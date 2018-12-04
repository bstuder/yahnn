{-# LANGUAGE Strict, PatternSynonyms #-}

module Activation where

import qualified Data.Vector.Unboxed as DV (Vector(..), Unbox, map, sum)
import qualified Matrix as M (pattern ColumnVector, empty, fromVector, Matrix)

data Activation = ReLu | Sigmoid | SoftMax | TanH deriving (Eq, Show)

forward :: Activation -> M.Matrix -> Either String M.Matrix
forward ReLu (M.ColumnVector size vector) =  M.fromVector size 1 $ DV.map (\x -> if x < 0 then 0 else x) vector
forward Sigmoid (M.ColumnVector size vector) = M.fromVector size 1 $ DV.map (\x -> 1 / (1 + exp(-x))) vector
forward SoftMax (M.ColumnVector size vector) = M.fromVector size 1 $ DV.map (\x -> (exp x) / normalization) vector
    where normalization = DV.sum $ DV.map exp vector
forward TanH (M.ColumnVector size vector) = M.fromVector size 1 $ DV.map tanh vector

backward :: Activation -> M.Matrix -> Either String M.Matrix
backward ReLu (M.ColumnVector size vector) = M.fromVector size size $ DV.map (\x -> if x < 0 then 0 else 1) vector
backward Sigmoid (M.ColumnVector size vector) = M.fromVector size size $ DV.map (\x -> 1 / (2 + exp x + exp (-x))) vector
--derivate SoftMax vector = #TODO
backward TanH (M.ColumnVector size vector) = M.fromVector size size $ DV.map (\x -> 1 - (tanh x) ** 2) vector
