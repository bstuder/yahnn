module Loss where

import qualified Data.Vector as V (Vector(..))

data Loss = RMSE deriving (Eq, Show)

forward :: (RealFloat a) => Loss -> V.Vector a -> V.Vector a -> Double
forward RMSE x y = sqrt . (/ V.length x) . sum . fmap (**2) $ zipWith (-) x y

derivate :: (RealFloat a) => Loss -> V.Vector a -> V.Vector a
derivate RMSE = fmap (\x -> if x < 0 then 0 else 1)
