module CoreLogic (
  extractFeaturesFromText,
  calculateCategoryFeatures
) where

import qualified Data.Text as T
import qualified Data.Set as Set
import Data.List (foldl')

import Types 

-- own util implementation of avg 
-- which ensures that if count == 0 there is no error by just taking count = 1
-- could be replaced by Maybe Monad with "Nothing" result if count == 0
myAvg :: [Double] -> Double
myAvg xs = sum xs / fromIntegral (max 1 (length xs))

-- IMPORTANT function which is basically used everywhere
-- takes: Lambda f, and a list xs
-- calculates: f mapped on xs, and then takes average of result
-- NOTE: map f xs IS THE SAME as f <$> xs
averageOf :: (a -> Double) -> [a] -> Double
averageOf f xs = myAvg $ map f xs 

-- filter criteria for getSentences (util)
textNotNull :: T.Text -> Bool
textNotNull = (not . T.null)

-- getting all non-empty sentences from Text
getSentences :: T.Text -> [T.Text]
getSentences = filter textNotNull . T.splitOn (T.pack ".") . T.toLower 

-- getting all words from sentence
-- first: get sentences, then map a list of words on each sentence
-- then: concatenate the list of list of words into one long list
getWordsFromSentence :: T.Text -> [T.Text]
getWordsFromSentence = concat . map T.words . getSentences

-- pure function which extracts features from text
-- saves it in BookFeature struct
extractFeaturesFromText :: FilePath -> T.Text -> BookFeatures
extractFeaturesFromText path text =
  let
    sentences = getSentences text
    sentenceCount = max 1 (length sentences)

    -- calling function words on sentences via functor 
    -- to get list of lift of words and then concatenating them
    words' = T.words <$> sentences 
    totalWords = concat words'
    totalWordCount = max 1 (length totalWords)

    -- Berechnung der einzelnen Metriken
    avgSLen = fromIntegral (sum $ length <$> words') / fromIntegral sentenceCount
    avgCommas = fromIntegral (sum $ T.count (T.pack ",") <$> sentences) / fromIntegral sentenceCount
    -- TODO: Set. not ideal as it is O(log n)
    uniqueWords = Set.fromList totalWords
    ratio = fromIntegral (Set.size uniqueWords) / fromIntegral totalWordCount
    avgWLen = fromIntegral (sum $ T.length <$> totalWords) / fromIntegral totalWordCount

  in BookFeatures path avgSLen avgCommas ratio avgWLen

calculateCategoryFeatures :: [BookFeatures] -> String -> BookFeatures
calculateCategoryFeatures features categoryName = 
  let
    n = fromIntegral $ max 1 (length features)
    sumAvgSentenceLength = sum $ avgSentenceLength <$> features
    sumAvgCommas = sum $ avgCommasPerSentence <$> features
    sumRatio = sum $ uniqueWordRatio <$> features
    sumAvgWordLength = sum $ avgWordLength <$> features
  in BookFeatures
       categoryName
       (sumAvgSentenceLength / n)
       (sumAvgCommas / n)
       (sumRatio / n)
       (sumAvgWordLength / n)

