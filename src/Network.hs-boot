module Network where

import qualified Activation as A (Activation(..))
import qualified Matrix as M (Matrix)

data Network a = Network {
    activations :: [A.Activation],
    weights :: [M.Matrix a]
}
