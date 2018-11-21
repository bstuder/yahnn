module Loss where

import qualified Data.Vector as V

data Loss = MSE deriving (Eq, Show)

forward :: (RealFloat a) => Loss -> V.Vector a -> V.Vector a -> a
forward MSE o t = (/ fromIntegral (V.length o)) . sum . fmap (**2) $ V.zipWith (-) o t

derivate :: (RealFloat a) => Loss -> V.Vector a -> V.Vector a -> V.Vector a
derivate MSE o t = (/ fromIntegral (V.length o)) . (*2) <$> V.zipWith (-) o t
