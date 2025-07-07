module CoreLogic (
  getWordsFromSentences,
  getSentences,
  extractFeaturesFromText,
  calculateCategoryFeatures,
  calculateThresholds,
  classifyBook,
  classifyWithWeights,
  totalWeight,
  calcStats,
  normalizeFeatures,
  classifyByDistance,
  euclideanDistance,
  classifyCombined,
) where

import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Char (toLower)
import Data.List (group)
import qualified Data.Vector as V
--import Data.List (foldl')

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
getWordsFromSentences :: [T.Text] -> [T.Text]
getWordsFromSentences sentences = concatMap T.words sentences

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
    totalWords = getWordsFromSentences sentences
    totalWordsStr = map T.unpack totalWords  -- Convert Text to String for syllable counting
    
    avgSentLen = calculateAvgSentenceLength sentences
    avgCommas = averageOf (fromIntegral . T.count (T.pack ",")) sentences
    uniqRatio = calculateUniqueWordRatio totalWords
    avgWordLen = averageOf (fromIntegral . T.length) totalWords
    
    avgSyllPerWord = avgSyllablesPerWord totalWordsStr
    flesch = fleschScore avgSentLen avgSyllPerWord
    lengthWords = length totalWords
  in
    BookFeatures
      path
      avgSentLen
      avgCommas
      uniqRatio
      avgWordLen
      flesch
      lengthWords

calculateCategoryFeatures :: [BookFeatures] -> String -> BookFeatures
calculateCategoryFeatures features categoryName =
  BookFeatures
    categoryName
    (averageOf avgSentenceLength features)
    (averageOf avgCommasPerSentence features)
    (averageOf uniqueWordRatio features)
    (averageOf avgWordLength features)
    (averageOf fleschReadingEase features)
    (round $ averageOf (fromIntegral . totalLength) features)



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
     (mid (fleschReadingEase avgChild) (fleschReadingEase avgAdult))
     (mid (fromIntegral $ totalLength avgChild) (fromIntegral $ totalLength avgAdult))

-- calculateScore = avgSentenceLength * w1 + avgCommasPerSentence * w2 + ...
-- assessScore = (calculateScore > Threshhold??) AdultBook else ChildrensBook

classifyBook :: Thresholds -> BookFeatures -> Classification
classifyBook thresholds features = 
  let
    childScore :: Int
    childScore = 0
               + (if avgSentenceLength features < sentenceLengthThreshold thresholds then 1 else 0)
               + (if avgCommasPerSentence features < commasThreshold thresholds then 1 else 0)
               + (if uniqueWordRatio features < ratioThreshold thresholds then 1 else 0) 
               + (if avgWordLength features < wordLengthThreshold thresholds then 1 else 0)
                + (if fleschReadingEase features >= fleschThreshold thresholds then 1 else 0)
    
    -- Einfache Logik: Wenn die Mehrheit der Kriterien für "Kind" spricht, ist es ein Kinderbuch.
    -- Dies kann man viel komplexer machen!
    in if childScore >= 3
       then ChildrensBook
       else AdultBook

classifyWithWeights :: Thresholds -> Weights -> BookFeatures -> Classification
classifyWithWeights thresholds weights features =
  let
    score = sum [
        if avgSentenceLength features < sentenceLengthThreshold thresholds then wSentenceLength weights else 0,
        if avgCommasPerSentence features < commasThreshold thresholds then wCommasPerSentence weights else 0,
        if uniqueWordRatio features < ratioThreshold thresholds then wUniqueWordRatio weights else 0,
        if avgWordLength features < wordLengthThreshold thresholds then wAvgWordLength weights else 0,
        if fleschReadingEase features >= fleschThreshold thresholds then wFleschReadingEase weights else 0,
        if fromIntegral (totalLength features) < totalLengthThreshold thresholds then wTotalLength weights else 0
      ]
  in
    if score >= totalWeight weights / 2
    then ChildrensBook
    else AdultBook


totalWeight :: Weights -> Double
totalWeight w = wSentenceLength w + wCommasPerSentence w + wUniqueWordRatio w + wAvgWordLength w + wFleschReadingEase w + wTotalLength w

-- Count syllables in a word (simple heuristic)
countSyllables :: String -> Int
countSyllables word = max 1 $ length (filter isVowelGroups $ group $ map toLower word)
  where
    isVowelGroups grp = head grp `elem` "aeiouy"
    
-- Calculate avg syllables per word in the whole text
avgSyllablesPerWord :: [String] -> Double
avgSyllablesPerWord words = 
  let totalSyllables = sum (map (fromIntegral . countSyllables) words)
      totalWords = fromIntegral (length words)
  in totalSyllables / totalWords

-- Calculate Flesch Reading Ease score given avg sentence length and avg syllables per word
fleschScore :: Double -> Double -> Double
fleschScore avgWordsPerSentence avgSyllPerWord = 
  206.835 - 1.015 * avgWordsPerSentence - 84.6 * avgSyllPerWord


-- Returns a FeatureStats object containing mean, variance, and standard deviation
calcStats :: [Double] -> FeatureStats
calcStats xs =
  let vxs = V.fromList xs
      n = fromIntegral (V.length vxs)
      m = if n == 0 then 0 else V.sum vxs / n
      var = if n <= 1 then 0 else V.sum (V.map (\x -> (x - m) ^ 2) vxs) / (n - 1)
  in FeatureStats m var (sqrt var)


-- Normalize a value using mean and stddev
zNormalize :: FeatureStats -> Double -> Double
zNormalize stats x =
  if stddev stats == 0 then 0 else (x - mean stats) / stddev stats

-- Convert BookFeatures into normalized vector [Double] using the given CategoryStats
normalizeFeatures :: CategoryStats -> BookFeatures -> [Double]
normalizeFeatures stats bf =
  [ zNormalize (slenStats stats) (avgSentenceLength bf)
  , zNormalize (commaStats stats) (avgCommasPerSentence bf)
  , zNormalize (uniqRatioStats stats) (uniqueWordRatio bf)
  , zNormalize (wordLenStats stats) (avgWordLength bf)
  , zNormalize (fleschStats stats) (fleschReadingEase bf)
  , zNormalize (lengthStats stats) (fromIntegral $ totalLength bf)
  ]

-- Compute Euclidean distance between two vectors
euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance xs ys = sqrt $ sum $ zipWith (\x y -> (x - y) ^ 2) xs ys

-- Classify a book using distance to category centroids
classifyByDistance :: CategoryStats -> CategoryStats -> BookFeatures -> Classification
classifyByDistance childStats adultStats book =
  let
    normalized = normalizeFeatures childStats book  -- use childStats for normalization
    childCentroid = replicate 6 0.0  -- z-normalized mean of child is zero-vector
    adultCentroid = zipWith (\sChild sAdult ->
                              zNormalize sChild (mean sAdult))
                            (toStatsList childStats)
                            (toStatsList adultStats)

    distToChild = euclideanDistance normalized childCentroid
    distToAdult = euclideanDistance normalized adultCentroid
  in
    if distToChild < distToAdult then ChildrensBook else AdultBook

-- Helper: convert CategoryStats to list of FeatureStats
toStatsList :: CategoryStats -> [FeatureStats]
toStatsList cat =
  [ slenStats cat
  , commaStats cat
  , uniqRatioStats cat
  , wordLenStats cat
  , fleschStats cat
  , lengthStats cat
  ]


-- Combined classifier: normalize, weight features, compute weighted distance
classifyCombined :: Weights -> CategoryStats -> CategoryStats -> BookFeatures -> Classification
classifyCombined weights childStats adultStats book =
  let
    -- Convert CategoryStats to list of FeatureStats
    toStatsList cat =
      [ slenStats cat
      , commaStats cat
      , uniqRatioStats cat
      , wordLenStats cat
      , fleschStats cat
      , lengthStats cat
      ]

    -- Normalize book features using childStats (zero-mean normalization)
    normalized = normalizeFeatures childStats book

    -- Feature weights as list for convenience
    wList = [ wSentenceLength weights
            , wCommasPerSentence weights
            , wUniqueWordRatio weights
            , wAvgWordLength weights
            , wFleschReadingEase weights
            , wTotalLength weights
            ]

    -- Weighted normalized features
    weightedFeatures = zipWith (*) normalized wList

    -- Calculate weighted centroids by normalizing means and applying weights
    normalizeMean stat = zNormalize (slenStats childStats) (mean stat)  -- using slenStats from childStats just as zero baseline
    -- Actually better to normalize each adult mean using child mean/stddev:
    normalizeMeanPair childStat adultStat = zNormalize childStat (mean adultStat)

    childCentroid = replicate 6 0.0  -- child centroid is origin in normalized space
    adultCentroid = zipWith (*) 
                      (zipWith normalizeMeanPair (toStatsList childStats) (toStatsList adultStats))
                      wList

    -- Weighted Euclidean distance
    euclideanDistance xs ys = sqrt . sum $ zipWith (\x y -> (x - y)^2) xs ys

    distToChild = euclideanDistance weightedFeatures childCentroid
    distToAdult = euclideanDistance weightedFeatures adultCentroid
  in
    if distToChild < distToAdult then ChildrensBook else AdultBook

