{-# LANGUAGE PatternSynonyms #-}

module Matrix where

import qualified Data.List as DL (transpose)
import qualified Data.Vector as DV (and, backpermute, fromList, generate, length, map, Vector(..), zipWith, zip)
import qualified System.Random as R (StdGen(..), split)
import qualified Utils as U (chunksOf, dotProduct, generateVector)

data Matrix a = FullMatrix {
    rows       :: !Int,
    columns    :: !Int,
    vector     :: DV.Vector a,
    isDiagonal :: Bool
} deriving (Show)

instance (Eq a) => Eq (Matrix a) where {
    (FullMatrix rowsL columnsL vectorL _) == (FullMatrix rowsR columnsR vectorR _) =
        rowsL == rowsR && columnsL == columnsR && vectorL == vectorR
}

pattern Matrix rows columns vectors = FullMatrix rows columns vectors False
pattern MatrixDiagonal rows columns vector = FullMatrix rows columns vector True

-- |Extract the (main) diagonal of any Matrix
diagonal :: Matrix a -> DV.Vector a
diagonal (Matrix rows columns vector) = DV.backpermute vector indexs
  where indexs = DV.fromList $ (*(1 + columns)) <$> [0, (min rows columns) - 1]

instance Functor Matrix where
    fmap function (Matrix rows columns vector) = Matrix rows columns $ DV.map function vector

addMatrices :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
addMatrices (Matrix firstRows firstColumns firstVector) (Matrix secondRows secondColumns secondVector)
    | (firstRows /= secondRows) || (firstColumns /= secondColumns) = Left "Mismatching dimensions between both matrices"
    | otherwise = Right $ Matrix firstRows firstColumns $ DV.zipWith (+) firstVector secondVector

applyRow :: (DV.Vector a -> b) -> Matrix a -> DV.Vector b
applyRow function = fmap function . toRows

empty :: Matrix a
empty = Matrix 0 0 mempty

equal :: RealFloat a => a -> Matrix a -> Matrix a -> Bool
equal precision (Matrix firstRows firstColumns firstVector) (Matrix secondRows secondColumns secondVector)
    | (firstRows, firstColumns) /= (secondRows, secondColumns) = False
    | otherwise = DV.and $ DV.zipWith (\x y -> abs (x - y) <= precision) firstVector secondVector

fromColumnVector :: DV.Vector a -> Matrix a
fromColumnVector vector = Matrix (DV.length vector) 1 vector

fromList :: [Int]               -- ^ List of number or neurons per layer
            -> R.StdGen         -- ^ Random generator
            -> [Matrix Double]  -- ^ Matrix with the specified layers and random weights
fromList [] _ = []
fromList [x] _ = []
fromList (x:y:xs) generator = Matrix y x randomVector : fromList (y:xs) secondGenerator
  where
    (firstGenerator, secondGenerator) = R.split generator
    randomVector = U.generateVector (x * y) firstGenerator

fromRowVector :: DV.Vector a -> Matrix a
fromRowVector vector = Matrix 1 (DV.length vector) vector

fromVectors :: RealFloat a =>
               DV.Vector a      -- ^ Column vector
               -> DV.Vector a   -- ^ Row vector
               -> Matrix a      -- ^ Matrix resulting from the multiplication of both vectors.
fromVectors firstVector secondVector = Matrix (length firstVector) (length secondVector) (firstVector >>= \x -> fmap (*x) secondVector)

generate :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
generate rows columns function =
    Matrix rows columns $ DV.generate (rows * columns) (\indice -> function (indice `div` columns, indice `mod` columns))

multiplyMatrices :: RealFloat a => Matrix a -> Matrix a -> Either String (Matrix a)
multiplyMatrices leftMatrix rightMatrix
    | columns leftMatrix /= rows rightMatrix = Left "Mismatching dimensions between both matrices"
    | otherwise = case (leftMatrix, rightMatrix) of
        (MatrixDiagonal _ _ vecL, MatrixDiagonal _ _ vecR) -> Right $ MatrixDiagonal newRows newColumns (DV.zipWith (*) vecL vecR)
        (MatrixDiagonal{}       , Matrix{})                -> Right $ Matrix newRows newColumns (newDiagVector leftMatrix rightMatrix)
        (Matrix{}               , MatrixDiagonal{})        -> transpose <$> rightMatrix `multiplyMatrices` transpose leftMatrix
        _                                                  -> Right $ Matrix newRows newColumns newVector
  where
    (newRows, newColumns)     = (rows leftMatrix, columns rightMatrix)
    newVector                 = toRows leftMatrix >>= \row -> U.dotProduct row <$> toColumns rightMatrix
    newDiagVector matDiag mat = DV.zip (diagonal matDiag) (toRows mat) >>= \(d,row) -> (*d) <$> row

multiplyVectorL :: (RealFloat a) => DV.Vector a -> Matrix a -> Either String (DV.Vector a)
multiplyVectorL vector matrix
    | rows matrix /= DV.length vector = Left $ "Mismatching dimensions between vector " <> show (DV.length vector) <> " and matrix " <> showSize matrix
    | otherwise = transpose matrix `multiplyVectorR` vector

multiplyVectorR :: (RealFloat a) => Matrix a -> DV.Vector a -> Either String (DV.Vector a)
multiplyVectorR matrix vector
    | columns matrix /= DV.length vector = Left $ "Mismatching dimensions between matrix " <> showSize matrix <> " and vector " <> show (DV.length vector)
    | otherwise = Right $ U.dotProduct vector <$> toRows matrix

showSize :: Matrix a -> String
showSize (Matrix rows columns _) = "(" <> show rows <> "," <> show columns <> ")"

toRows :: Matrix a -> DV.Vector (DV.Vector a)
toRows (Matrix _ columns vector) = U.chunksOf columns vector

toColumns :: Matrix a -> DV.Vector (DV.Vector a)
toColumns = toRows . transpose

transpose :: Matrix a -> Matrix a
transpose (Matrix rows columns vector) = Matrix columns rows transposedVector
  where
    transposedVector = DV.backpermute vector (DV.fromList newIndexes)
    newIndexes = concat . DL.transpose . take rows . iterate (fmap (+columns)) $ [0 .. columns - 1]
