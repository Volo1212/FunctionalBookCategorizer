module CoreLogic (
  extractFeaturesFromText,
  calculateCategoryFeatures,
  calculateThresholds
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

-- TODO:
-- getTotalWordCount = max 1 (length totalWords)

calculateAvgSentenceLength :: [T.Text] -> Double
calculateAvgSentenceLength sentences = averageOf (fromIntegral . length . T.words) sentences

calculateUniqueWordRatio :: [T.Text] -> Double
calculateUniqueWordRatio totalWords = 
    let totalWordCount = fromIntegral $ max 1 (length totalWords)
        uniqueWordCount = fromIntegral $ Set.size (Set.fromList totalWords)
    in uniqueWordCount / totalWordCount

-- pure function which extracts features from text
-- saves it in BookFeature struct
extractFeaturesFromText :: FilePath -> T.Text -> BookFeatures
extractFeaturesFromText path text =
  let
    sentences = getSentences text
    totalWords = concatMap T.words sentences 
  in
    BookFeatures
      path
      (calculateAvgSentenceLength sentences)
      (averageOf (fromIntegral . T.count (T.pack ",")) sentences) -- TODO: calculateAvgCommas func
      0
      (averageOf (fromIntegral . T.length) totalWords) -- TODO: calculateAvgWorldLength func

calculateCategoryFeatures :: [BookFeatures] -> String -> BookFeatures
calculateCategoryFeatures features categoryName =
  BookFeatures
    categoryName
    (averageOf avgSentenceLength features) -- gets list of avg sentence lengths, then takes avg func
    (averageOf avgCommasPerSentence features) -- similarly
    (averageOf uniqueWordRatio features)
    (averageOf avgWordLength features)

calculateThresholds :: [BookFeatures] -> [BookFeatures] -> Thresholds
calculateThresholds childrenFeatures adultFeatures =
  let
    avgChild = calculateCategoryFeatures childrenFeatures "children"
    avgAdult = calculateCategoryFeatures adultFeatures "adults"
    mid a b = (a + b) / 2
  in Thresholds
       (mid (avgSentenceLength avgChild) (avgSentenceLength avgAdult))
       (mid (avgCommasPerSentence avgChild) (avgCommasPerSentence avgAdult))
       (mid (uniqueWordRatio avgChild) (uniqueWordRatio avgAdult))
       (mid (avgWordLength avgChild) (avgWordLength avgAdult))