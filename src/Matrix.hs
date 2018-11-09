module Matrix where

import qualified Data.Vector as V

data Matrix a = Matrix {
    nrow :: !Int,
    ncol :: !Int,
    vec :: V.Vector a
}
