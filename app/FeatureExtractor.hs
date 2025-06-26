module FeatureExtractor (avg) where

-- takes Integer list and returns the average (double)
-- FromIntegral is used to convert the sum and length to Double
avg :: [Int] -> Double
avg xs = if null xs then 0 else fromIntegral (sum xs) / fromIntegral (length xs)
