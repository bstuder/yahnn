{-# LANGUAGE Strict, PatternSynonyms #-}

module Activation
(
    Activation(..),
    backward,
    forward
) where

import qualified Data.Vector as DV (maximum, Vector(..))
import qualified Matrix as M (pattern ColumnVector, empty, fromList, fromVector, imap, Matrix, maximum, multiplyMatrices, sum, transpose)


{----- TYPES -----}

data Activation = Identity | ReLu | Sigmoid | SoftMax | TanH deriving (Eq, Show)


{----- HIDDEN METHODS -----}

stabilizedSigmoid :: RealFloat a => a -> a
stabilizedSigmoid value = if value >= 0 then 1 / (1 + exp (-value)) else exponentialValue / (1 + exponentialValue)
  where
    exponentialValue = exp value

stabilizedSigmoidDerivative :: RealFloat a => a -> a
stabilizedSigmoidDerivative value = stabilizedValue * (1 - stabilizedValue)
  where
    stabilizedValue = stabilizedSigmoid value


{----- EXPORTED METHODS -----}

backward :: RealFloat a => Activation -> M.Matrix a -> Either String (M.Matrix a)
backward Identity (M.ColumnVector size _) = M.fromList size size $ replicate size 1
backward ReLu (M.ColumnVector size vector) = M.fromVector size size $ (\x -> if x < 0 then 0 else 1) <$> vector
backward Sigmoid (M.ColumnVector size vector) = M.fromVector size size $ stabilizedSigmoidDerivative <$> vector
backward SoftMax matrix@M.ColumnVector{} = do
    normalizedMatrix <- forward SoftMax matrix
    fullMatrix <- M.multiplyMatrices normalizedMatrix $ M.transpose normalizedMatrix
    return $ M.imap (\(row, column) value -> if row == column then (sqrt value) - value else negate value) fullMatrix
backward TanH (M.ColumnVector size vector) = M.fromVector size size $ (\x -> 1 - (tanh x) ** 2) <$> vector

forward :: RealFloat a => Activation -> M.Matrix a -> Either String (M.Matrix a)
forward Identity matrix@M.ColumnVector{} = Right matrix
forward ReLu (M.ColumnVector size vector) = M.fromVector size 1 $ (\x -> if x < 0 then 0 else x) <$> vector
forward Sigmoid (M.ColumnVector size vector) = M.fromVector size 1 $ stabilizedSigmoid <$> vector
forward SoftMax matrix@M.ColumnVector{} = do
    let maximum = M.maximum matrix
    let normalization = M.sum $ exp . (\value -> value - maximum) <$> matrix
    Right $ (\value -> (exp $ value - maximum) / normalization) <$> matrix
forward TanH (M.ColumnVector size vector) = M.fromVector size 1 $ tanh <$> vector
