module Utils where

chunksOf :: Int -> [a] -> [[a]]
chunksOf n _ | n <= 0 = []
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
