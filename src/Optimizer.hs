{-# LANGUAGE Strict #-}

module Optimizer where

import qualified Control.Monad as CM (zipWithM)
import qualified Matrix as M (addMatrices, map, Matrix)

data Optimizer = SGD { momentum :: Double, learningRate :: Double } deriving (Eq, Show)

optimize :: [M.Matrix] -> [M.Matrix] -> Optimizer -> Either String [M.Matrix]
optimize weights gradients (SGD momentum learningRate) =
    CM.zipWithM applyGradient weights gradients
    where
        applyGradient weight gradient = M.addMatrices (M.map (*momentum) weight) (M.map (*(-1 * learningRate)) gradient)
