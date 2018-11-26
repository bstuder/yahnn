module Optimizer where

import qualified Control.Monad as CM (zipWithM)
import qualified Matrix as M (addMatrices, Matrix(..))
import {-# SOURCE #-} qualified Network as N (Network(..))

data Optimizer a = SGD { momentum :: a, learningRate :: a } deriving (Eq, Show)

apply :: (RealFloat a) => N.Network a -> [M.Matrix a] -> Optimizer a -> Either String (N.Network a)
apply (N.Network activations weights) gradients (SGD momentum learningRate) =
    N.Network activations <$> CM.zipWithM applyGradient weights gradients
      where
          applyGradient weight gradient = M.addMatrices ((*momentum) <$> weight) ((*(-1 * learningRate)) <$> gradient)
