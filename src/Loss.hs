module Loss where

import qualified Data.Vector as V

data Loss = MSE deriving (Eq, Show)

forward :: (RealFloat a) => Loss -> V.Vector a -> V.Vector a -> a
forward MSE output target = (/ fromIntegral (V.length output)) . sum . fmap (**2) $ V.zipWith (-) output target

derivate :: (RealFloat a) => Loss -> V.Vector a -> V.Vector a -> V.Vector a
derivate MSE output target = (/ fromIntegral (V.length output)) . (*2) <$> V.zipWith (-) output target
