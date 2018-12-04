{-# LANGUAGE Strict #-}

module Optimizer where

import qualified Control.Monad as CM (zipWithM)
import qualified Matrix as M (Matrix(..), addMatrices, map)
import qualified Data.Vector.Unboxed as DV (Unbox)

data Optimizer a = SGD { momentum :: a, learningRate :: a } deriving (Eq, Show)

optimize :: (DV.Unbox a, RealFloat a) => [M.Matrix a] -> [M.Matrix a] -> Optimizer a -> Either String [M.Matrix a]
optimize weights gradients (SGD momentum learningRate) =
    CM.zipWithM applyGradient weights gradients
    where
        applyGradient weight gradient = M.addMatrices (M.map (*momentum) weight) (M.map (*(-1 * learningRate)) gradient)
