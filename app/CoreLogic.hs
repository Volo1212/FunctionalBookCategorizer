module CoreLogic (
  extractFeaturesFromText,
  calculateCategoryFeatures
) where

import qualified Data.Text as T
import qualified Data.Set as Set
import Data.List (foldl')

import Types 

-- isolating functions into pure small blocks -> has to be CONTINUED
getNonEmptySentences :: T.Text -> [T.Text]
getNonEmptySentences text = filter (not . T.null) . T.splitOn (T.pack ".") . T.toLower $ text


-- pure function which extracts features from text
-- saves it in BookFeature struct
extractFeaturesFromText :: FilePath -> T.Text -> BookFeatures
extractFeaturesFromText path text =
  let
    sentences = getNonEmptySentences text
    sentenceCount = max 1 (length sentences)

    -- calling function words on sentences via functor 
    -- to get list of lift of words and then concatenating them
    words' = T.words <$> sentences 
    totalWords = concat words'
    totalWordCount = max 1 (length totalWords)

    -- Berechnung der einzelnen Metriken
    avgSLen = fromIntegral (sum $ length <$> words') / fromIntegral sentenceCount
    avgCommas = fromIntegral (sum $ T.count (T.pack ",") <$> sentences) / fromIntegral sentenceCount
    uniqueWords = Set.fromList totalWords
    ratio = fromIntegral (Set.size uniqueWords) / fromIntegral totalWordCount
    avgWLen = fromIntegral (sum $ T.length <$> totalWords) / fromIntegral totalWordCount

  in BookFeatures path avgSLen avgCommas ratio avgWLen

calculateCategoryFeatures :: [BookFeatures] -> BookFeatures
calculateCategoryFeatures features = 
  let
    n = fromIntegral $ max 1 (length features)

    sumBy f = sum (map f features)

    avg f = sumBy f / n
  in
    BookFeatures
      "AVERAGE"
      (avg avgSentenceLength)
      (avg avgCommasPerSentence)
      (avg uniqueWordRatio)
      (avg avgWordLength)