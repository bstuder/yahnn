{-# LANGUAGE Strict, PatternSynonyms #-}

module Activation
(
    Activation(..),

    backward,
    forward
) where

import qualified Matrix as M (pattern ColumnVector, fromHmat, imap, map, Matrix, maximum, multiplyMatrices, sum, transpose)
import qualified Numeric.LinearAlgebra as NL


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
backward Identity (M.ColumnVector size _) = M.fromHmat $ NL.ident size
backward ReLu (M.ColumnVector _ hmat)     = M.fromHmat . NL.diag . NL.flatten $ NL.cmap (\x -> if x < 0 then 0 else 1) hmat
backward Sigmoid (M.ColumnVector _ hmat)  = M.fromHmat . NL.diag . NL.flatten $ NL.cmap stabilizedSigmoidDerivative hmat
backward TanH (M.ColumnVector _ hmat)     = M.fromHmat . NL.diag . NL.flatten $ NL.cmap (\x -> 1 - tanh x ** 2) hmat
backward SoftMax matrix@M.ColumnVector{}  = do
    normalizedMatrix <- forward SoftMax matrix
    fullMatrix <- M.multiplyMatrices normalizedMatrix $ M.transpose normalizedMatrix
    return $ M.imap (\(row, column) value -> if row == column then sqrt value - value else negate value) fullMatrix

forward :: Activation -> M.Matrix -> Either String M.Matrix
forward Identity matrix@M.ColumnVector{} = Right matrix
forward ReLu (M.ColumnVector _ hmat)     = M.fromHmat $ NL.cmap (\x -> if x < 0 then 0 else x) hmat
forward Sigmoid (M.ColumnVector _ hmat)  = M.fromHmat $ NL.cmap stabilizedSigmoid hmat
forward TanH (M.ColumnVector _ hmat)     = M.fromHmat $ NL.cmap tanh hmat
forward SoftMax matrix@M.ColumnVector{}  = do
    let maximum = M.maximum matrix
    let normalization = M.sum $ M.map (exp . (\value -> value - maximum)) matrix
    Right $ M.map (\value -> exp (value - maximum) / normalization) matrix
