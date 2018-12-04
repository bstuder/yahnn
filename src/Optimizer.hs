{-# LANGUAGE Strict #-}

module Optimizer where

import qualified Control.Monad as CM (zipWithM)
import qualified Matrix as M (addMatrices, Matrix(..))

data Optimizer a = SGD { momentum :: a, learningRate :: a } deriving (Eq, Show)

optimize :: RealFloat a => [M.Matrix a] -> [M.Matrix a] -> Optimizer a -> Either String [M.Matrix a]
optimize values gradients (SGD momentum learningRate) =
    CM.zipWithM applyGradient values gradients
  where
    applyGradient weight gradient = M.addMatrices ((*momentum) <$> weight) ((*(-1 * learningRate)) <$> gradient)
