module Optimizer where

import qualified Control.Monad as CM (zipWithM)
import qualified Data.Either as E (Either(..))
import qualified Matrix as M (addMatrices, Matrix(..))
import qualified Network as N (Network(..))

data Optimizer a = SGD { momentum :: a, learningRate :: a } deriving (Eq, Show)

apply :: (RealFloat a) => N.Network a -> [M.Matrix a] -> Optimizer a -> E.Either String (N.Network a)
apply (N.Network activations weights) gradients (SGD momentum learningRate) =
    N.Network activations <$> CM.zipWithM applyGradient weights gradients
      where
          applyGradient weight gradient = M.addMatrices (fmap (*momentum) weight) (fmap (*(-1 * learningRate)) gradient)