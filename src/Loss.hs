module Loss where

import qualified Data.Vector as DV (length, Vector(..), zipWith)

data Loss = MSE deriving (Eq, Show)

forward :: (RealFloat a) => Loss -> DV.Vector a -> DV.Vector a -> a
forward MSE output target = (/ fromIntegral (DV.length output)) . sum . fmap (**2) $ DV.zipWith (-) output target

derivate :: (RealFloat a) => Loss -> DV.Vector a -> DV.Vector a -> DV.Vector a
derivate MSE output target = (/ fromIntegral (DV.length output)) . (*2) <$> DV.zipWith (-) output target
