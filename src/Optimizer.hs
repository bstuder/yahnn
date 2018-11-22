module Optimizer where

import qualified Control.Monad as CM (zipWithM)
import qualified Data.Either as E (Either(..))
import qualified Matrix as M (addMatrices, generate, Matrix(..))
import qualified Network as N (Network(..))

data Optimizer = SGD deriving (Eq, Show)

apply :: (RealFloat a) => N.Network a -> [M.Matrix a] -> Optimizer -> [a] -> E.Either String (N.Network a)
apply (N.Network activations weights) gradients SGD parameters
    | length parameters /= 2 = E.Left "Invalid number of argument for SGD optimizer."
    | otherwise =
        N.Network activations <$> CM.zipWithM applyGradient weights gradients
        where
            [momentum, learningRate] = parameters
            applyGradient weight gradient = M.addMatrices (fmap (*momentum) weight) (fmap (*(-1 * learningRate)) gradient)
