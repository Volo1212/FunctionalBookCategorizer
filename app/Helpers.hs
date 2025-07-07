module Helpers where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set  as Set

-- module to include all helper functions for the main logic
-- so that our eyes wont turn to ash as we slowly dive into madness from Haskell anyway

---------------------------
-- TEXT BASED 
---------------------------
safeDiv :: Double -> Int -> Double
safeDiv _ 0 = 0.0
safeDiv n d = n / fromIntegral d

getSentences :: T.Text -> [T.Text]
getSentences = L.filter (not . T.null) . T.splitOn (T.pack ".")

getWords :: [T.Text] -> [[T.Text]]
getWords = L.map (T.words . T.toLower)

calculateSentenceLengths :: [[T.Text]] -> [Int]
calculateSentenceLengths = L.map L.length

calculateAvgSentenceLength :: [Int] -> Double
calculateAvgSentenceLength lengths = safeDiv (fromIntegral $ L.sum lengths) (L.length lengths)

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

countSyllablesInWord :: T.Text -> Int
countSyllablesInWord word
  | T.null word = 0
  | otherwise = max 1 $ fst $ T.foldl' count (0, ' ') (T.toLower word) -- go through word with fold and sum up syllables
  where
    count (n, lastChar) currentChar
      | isVowel currentChar && not (isVowel lastChar) = (n + 1, currentChar) -- only count if current is vowel and previous isnt (to prevent aaaaaa ?)
      | otherwise = (n, currentChar)

-- TODO: rename
calculateSyllablesPerWord :: [[T.Text]] -> [Int]
calculateSyllablesPerWord = L.map countSyllablesInWord . L.concat

-- TODO: rename
calculateAvgSyllablesPerWord :: [Int] -> Double
calculateAvgSyllablesPerWord syllables = safeDiv (fromIntegral $ L.sum syllables) (L.length syllables)

calculateWordLengths :: [[T.Text]] -> [Int]
calculateWordLengths = L.map T.length . L.concat

calculateAvgWordLength :: [Int] -> Double
calculateAvgWordLength lengths = safeDiv (fromIntegral $ L.sum lengths) (L.length lengths)

-- based on mathematical formula for english texts
calculateFleschScore :: Double -> Double -> Double
calculateFleschScore avgSentLen avgSyllables = 206.835 - (1.015 * avgSentLen) - (84.6 * avgSyllables)

calculateUniqueWordRatio :: [[T.Text]] -> Double
calculateUniqueWordRatio wordLists =
  let allWords = L.concat wordLists
      totalWords = L.length allWords
      uniqueWords = Set.size $ Set.fromList allWords
  in safeDiv (fromIntegral uniqueWords) totalWords

countCommas :: T.Text -> Int
countCommas sentence = T.count (T.pack ",") sentence

calculateAvgCommasPerSentence :: [T.Text] -> Double
calculateAvgCommasPerSentence sentences =
  let totalCommas = fromIntegral $ L.sum $ L.map countCommas sentences
  in safeDiv totalCommas (L.length sentences)

---------------------------
-- STATISTICS ARE AWESOME BRO JUST SMILE PLEEEEEEASE
---------------------------
calculateMean :: [Double] -> Double
calculateMean xs
  | null xs = 0.0
  | otherwise = L.sum xs / fromIntegral (L.length xs)

-- square root of the variance, mean val is need obviously like duh
calculateStdDev :: Double -> [Double] -> Double
calculateStdDev meanVal xs
  | null xs = 0.0
  | otherwise = sqrt $ L.sum (L.map (\x -> (x - meanVal) ^ 2) xs) / fromIntegral (L.length xs)

-- mathemtical formula standardization of a value: 
-- value minus mean of that value divided by standard deviation
normalizeValue :: Double -> FeatureStats -> Double
normalizeValue val stats
  | stdDev stats == 0 = 0.0
  | otherwise = (val - mean stats) / stdDev stats

-- used to convert linear sum of weighted features which could be astronomical, negative, etc...
-- in a range between 0 and 1 to make it actually meaningful (a probability)
-- e.g. z = -3.5, sigmoid(z) = 0.029
sigmoid :: Double -> Double
sigmoid z = 1.0 / (1.0 + exp (-z))

