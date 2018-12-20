module Utils where

equalDouble :: Double -> Double -> Double -> Bool
equalDouble precision firstDouble secondeDouble = firstDouble == secondeDouble || abs (firstDouble - secondeDouble) / max (abs firstDouble) (abs secondeDouble) <= precision
