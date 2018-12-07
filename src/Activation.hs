{-# LANGUAGE Strict, PatternSynonyms #-}

module Activation
(
    Activation(..),

    backward,
    forward
) where

import qualified Data.Vector.Unboxed as DVU (map)
import qualified Matrix as M (pattern ColumnVector, fromList, fromVector, imap, map, Matrix, maximum, multiplyMatrices, sum, transpose)


{----- TYPES -----}

data Activation = Identity | ReLu | Sigmoid | SoftMax | TanH deriving (Eq, Show)


{----- HIDDEN METHODS -----}

stabilizedSigmoid :: Double -> Double
stabilizedSigmoid value = if value >= 0 then 1 / (1 + exp (-value)) else exponentialValue / (1 + exponentialValue)
  where
    exponentialValue = exp value

stabilizedSigmoidDerivative :: Double -> Double
stabilizedSigmoidDerivative value = stabilizedValue * (1 - stabilizedValue)
  where
    stabilizedValue = stabilizedSigmoid value


{----- EXPORTED METHODS -----}

backward :: Activation -> M.Matrix -> Either String M.Matrix
backward Identity (M.ColumnVector size _) = M.fromList size size $ replicate size 1
backward ReLu (M.ColumnVector size vector) = M.fromVector size size $ DVU.map (\x -> if x < 0 then 0 else 1) vector
backward Sigmoid (M.ColumnVector size vector) = M.fromVector size size $ DVU.map stabilizedSigmoidDerivative vector
backward SoftMax matrix@M.ColumnVector{} = do
    normalizedMatrix <- forward SoftMax matrix
    fullMatrix <- M.multiplyMatrices normalizedMatrix $ M.transpose normalizedMatrix
    return $ M.imap (\(row, column) value -> if row == column then sqrt value - value else negate value) fullMatrix
backward TanH (M.ColumnVector size vector) = M.fromVector size size $ DVU.map (\x -> 1 - tanh x ** 2) vector

forward :: Activation -> M.Matrix -> Either String M.Matrix
forward Identity matrix@M.ColumnVector{} = Right matrix
forward ReLu (M.ColumnVector size vector) = M.fromVector size 1 $ DVU.map (\x -> if x < 0 then 0 else x) vector
forward Sigmoid (M.ColumnVector size vector) = M.fromVector size 1 $ DVU.map stabilizedSigmoid vector
forward SoftMax matrix@M.ColumnVector{} = do
    let maximum = M.maximum matrix
    let normalization = M.sum $ M.map (exp . (\value -> value - maximum)) matrix
    Right $ M.map (\value -> exp (value - maximum) / normalization) matrix
forward TanH (M.ColumnVector size vector) = M.fromVector size 1 $ DVU.map tanh vector
