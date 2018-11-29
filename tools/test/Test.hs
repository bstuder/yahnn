{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Main where

import qualified Data.Vector as DV (empty, fromList, Vector(..))

data Matrix a = Matrix {
    rows       :: !Int,
    columns    :: !Int,
    vector     :: DV.Vector a
} deriving (Show)

pattern FullMatrix rows columns vector = Matrix rows columns vector
pattern DiagonalMatrix size vector <- Matrix size ((== size) -> True) vector where                           
    DiagonalMatrix size vector = Matrix size size vector
pattern ColumnVector rows vector = Matrix rows 1 vector
pattern RowVector columns vector = Matrix 1 columns vector

test :: Matrix a -> String
test (DiagonalMatrix size vector) = "Diagonal matrix"
test (FullMatrix rows columns vector) = "Full matrix"

main :: IO ()
main = do
    let (FullMatrix rows columns vector) = DiagonalMatrix 50 DV.empty
    print rows
    print columns
    -- print $ test $ FullMatrix 50 50 DV.empty

    {-print "Read the training set..."
    let dataset = do
            input <- DBL.readFile "data/MNIST/training_set"
            return $ D.normalize D.Datapoints $ D.fromByteString input

    print "Train the network..."
    let generator = SR.mkStdGen 12345
    let (trainedNetwork, losses) = do
            network <- N.random [28 * 28, 300, 10] [A.Sigmoid, A.Sigmoid, A.Sigmoid] generator
            return $ N.train (O.SGD 1 0.01) L.MSE dataset network

    print "Finished..."-}
