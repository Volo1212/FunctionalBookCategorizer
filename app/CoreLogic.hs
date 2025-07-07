-- CoreLogic.hs

module CoreLogic (
  -- Deine bestehenden Exporte...
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
  -- NEU: Unser Export für das Training
  trainWeightsWithGradientDescent
) where

import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Char (toLower)
import Data.List (group, foldl')
import qualified Data.Vector as V
import Text.Printf
import Types

-- #############################################################################
-- DEINE BESTEHENDEN FUNKTIONEN (unverändert)
-- ... hier kommt dein gesamter bisheriger Code aus CoreLogic.hs rein ...
-- myAvg, averageOf, getSentences, extractFeaturesFromText, etc.
-- Ich lasse sie hier weg, um die Antwort übersichtlich zu halten.
-- Die neuen Funktionen kommen einfach ans Ende der Datei.
-- #############################################################################

-- Dein Code bis hierher...
myAvg :: [Double] -> Double
myAvg xs = sum xs / fromIntegral (max 1 (length xs))

averageOf :: (a -> Double) -> [a] -> Double
averageOf f xs = myAvg $ map f xs 

textNotNull :: T.Text -> Bool
textNotNull = (not . T.null)

getSentences :: T.Text -> [T.Text]
getSentences = filter textNotNull . T.splitOn (T.pack ".") . T.toLower 

getWordsFromSentences :: [T.Text] -> [T.Text]
getWordsFromSentences sentences = concatMap T.words sentences

calculateAvgSentenceLength :: [T.Text] -> Double
calculateAvgSentenceLength sentences = averageOf (fromIntegral . length . T.words) sentences

calculateUniqueWordRatio :: [T.Text] -> Double
calculateUniqueWordRatio totalWords = 
    let totalWordCount = fromIntegral $ max 1 (length totalWords)
        uniqueWordCount = fromIntegral $ Set.size (Set.fromList totalWords)
    in uniqueWordCount / totalWordCount

extractFeaturesFromText :: FilePath -> T.Text -> BookFeatures
extractFeaturesFromText path text =
  let
    sentences = getSentences text
    totalWords = getWordsFromSentences sentences
    totalWordsStr = map T.unpack totalWords
    avgSentLen = calculateAvgSentenceLength sentences
    avgCommas = averageOf (fromIntegral . T.count (T.pack ",")) sentences
    uniqRatio = calculateUniqueWordRatio totalWords
    avgWordLen = averageOf (fromIntegral . T.length) totalWords
    avgSyllPerWord = avgSyllablesPerWord totalWordsStr
    flesch = fleschScore avgSentLen avgSyllPerWord
    lengthWords = length totalWords
  in
    BookFeatures path avgSentLen avgCommas uniqRatio avgWordLen flesch lengthWords

calculateCategoryFeatures :: [BookFeatures] -> String -> BookFeatures
calculateCategoryFeatures features categoryName =
  BookFeatures categoryName
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
    in if childScore >= 3 then ChildrensBook else AdultBook

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
  in if score >= totalWeight weights / 2 then ChildrensBook else AdultBook

totalWeight :: Weights -> Double
totalWeight w = wSentenceLength w + wCommasPerSentence w + wUniqueWordRatio w + wAvgWordLength w + wFleschReadingEase w + wTotalLength w

countSyllables :: String -> Int
countSyllables word = max 1 $ length (filter isVowelGroups $ group $ map toLower word)
  where isVowelGroups grp = head grp `elem` "aeiouy"

avgSyllablesPerWord :: [String] -> Double
avgSyllablesPerWord words = 
  let totalSyllables = sum (map (fromIntegral . countSyllables) words)
      totalWords = fromIntegral (length words)
  in totalSyllables / totalWords

fleschScore :: Double -> Double -> Double
fleschScore avgWordsPerSentence avgSyllPerWord = 206.835 - 1.015 * avgWordsPerSentence - 84.6 * avgSyllPerWord

calcStats :: [Double] -> FeatureStats
calcStats xs =
  let vxs = V.fromList xs
      n = fromIntegral (V.length vxs)
      m = if n == 0 then 0 else V.sum vxs / n
      var = if n <= 1 then 0 else V.sum (V.map (\x -> (x - m) ^ 2) vxs) / (n - 1)
  in FeatureStats m var (sqrt var)

zNormalize :: FeatureStats -> Double -> Double
zNormalize stats x = if stddev stats == 0 then 0 else (x - mean stats) / stddev stats

normalizeFeatures :: CategoryStats -> BookFeatures -> [Double]
normalizeFeatures stats bf =
  [ zNormalize (slenStats stats) (avgSentenceLength bf)
  , zNormalize (commaStats stats) (avgCommasPerSentence bf)
  , zNormalize (uniqRatioStats stats) (uniqueWordRatio bf)
  , zNormalize (wordLenStats stats) (avgWordLength bf)
  , zNormalize (fleschStats stats) (fleschReadingEase bf)
  , zNormalize (lengthStats stats) (fromIntegral $ totalLength bf)
  ]

euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance xs ys = sqrt $ sum $ zipWith (\x y -> (x - y) ^ 2) xs ys

classifyByDistance :: CategoryStats -> CategoryStats -> BookFeatures -> Classification
classifyByDistance childStats adultStats book =
  let
    normalized = normalizeFeatures childStats book
    childCentroid = replicate 6 0.0
    adultCentroid = zipWith (\sChild sAdult -> zNormalize sChild (mean sAdult))
                            (toStatsList childStats)
                            (toStatsList adultStats)
    distToChild = euclideanDistance normalized childCentroid
    distToAdult = euclideanDistance normalized adultCentroid
  in if distToChild < distToAdult then ChildrensBook else AdultBook

toStatsList :: CategoryStats -> [FeatureStats]
toStatsList cat = [slenStats cat, commaStats cat, uniqRatioStats cat, wordLenStats cat, fleschStats cat, lengthStats cat]

-- #############################################################################
-- NEUE FUNKTIONEN FÜR DEN GRADIENTENABSTIEG
-- #############################################################################

-- GEÄNDERT: Diese Funktion muss jetzt auch mit der symmetrischen Logik arbeiten.
-- Sie braucht die globalen Statistiken, um die Features des neuen Buchs korrekt zu normalisieren.
classifyCombined :: Weights -> CategoryStats -> CategoryStats -> CategoryStats -> BookFeatures -> Classification
classifyCombined weights overallStats childStats adultStats book =
  let
    -- 1. Normalisiere das neue Buch mit den globalen Statistiken
    normalizedBookFeatures = normalizeFeatures overallStats book
    wList = weightsToList weights

    -- 2. Berechne die Zentren für Kinder und Erwachsene, ebenfalls mit globalen Statistiken
    -- Wir brauchen die Mittelwerte der unnormalisierten Features jeder Kategorie
    getMeans catStats = [mean (slenStats catStats), mean (commaStats catStats), mean (uniqRatioStats catStats), mean (wordLenStats catStats), mean (fleschStats catStats), mean (lengthStats catStats)]
    
    -- Normalisiere die Mittelwerte, um die Zentren zu bekommen
    childCentroid = normalizeFeatures overallStats (BookFeatures "" (head (getMeans childStats)) (getMeans childStats !! 1) (getMeans childStats !! 2) (getMeans childStats !! 3) (getMeans childStats !! 4) 0)
    adultCentroid = normalizeFeatures overallStats (BookFeatures "" (head (getMeans adultStats)) (getMeans adultStats !! 1) (getMeans adultStats !! 2) (getMeans adultStats !! 3) (getMeans adultStats !! 4) 0)

    -- 3. Berechne die gewichtete Distanz zu beiden Zentren
    weightedFeatures = zipWith (*) wList normalizedBookFeatures
    weightedChildCentroid = zipWith (*) wList childCentroid
    weightedAdultCentroid = zipWith (*) wList adultCentroid

    distSq xs ys = sum $ zipWith (\x y -> (x - y)^2) xs ys
    distToChild = distSq weightedFeatures weightedChildCentroid
    distToAdult = distSq weightedFeatures weightedAdultCentroid
  in
    if distToChild < distToAdult then ChildrensBook else AdultBook

-- ... (Deine anderen Helfer wie weightsToList, addVectors etc. bleiben)
-- Hilfsfunktionen zur Vektorrechnung
addVectors :: [Double] -> [Double] -> [Double]
addVectors = zipWith (+)

scaleVector :: Double -> [Double] -> [Double]
scaleVector scalar = map (* scalar)

-- Hilfsfunktionen zur Konvertierung zwischen Weights-Record und Vektor ([Double])
weightsToList :: Weights -> [Double]
weightsToList w = [wSentenceLength w, wCommasPerSentence w, wUniqueWordRatio w, wAvgWordLength w, wFleschReadingEase w, wTotalLength w]

listToWeights :: [Double] -> Weights
listToWeights [w1, w2, w3, w4, w5, w6] = Weights w1 w2 w3 w4 w5 w6
listToWeights _ = error "Falsche Anzahl an Gewichten in der Liste!"

-- #############################################################################
-- GRADIENTENABSTIEG MIT SYMMETRISCHER NORMALISIERUNG
-- #############################################################################

-- NEU: Eine Hilfsfunktion, um den Mittelwert-Vektor aus einer Liste von Vektoren zu berechnen
calculateMeanVector :: [[Double]] -> [Double]
calculateMeanVector vectors =
    let numVectors = fromIntegral $ max 1 (length vectors)
        initialSum = replicate (length (head vectors)) 0.0
        totalSum = foldl' addVectors initialSum vectors
    in scaleVector (1.0 / numVectors) totalSum

-- GEÄNDERT: Nimmt jetzt beide Zentren entgegen
calculatePredictionScore :: [Double] -> [Double] -> [Double] -> [Double] -> Double
calculatePredictionScore weights normalizedFeatures childCentroid adultCentroid =
  let weightedFeatures = zipWith (*) weights normalizedFeatures
      weightedChildCentroid = zipWith (*) weights childCentroid
      weightedAdultCentroid = zipWith (*) weights adultCentroid
      distSq xs ys = sum $ zipWith (\x y -> (x - y)^2) xs ys
      distToChildSq = distSq weightedFeatures weightedChildCentroid
      distToAdultSq = distSq weightedFeatures weightedAdultCentroid
  in
      distToChildSq - distToAdultSq

-- GEÄNDERT: Nimmt jetzt beide Zentren entgegen, die Formel für die Ableitung ändert sich!
calculateGradientForBook :: [Double] -> [Double] -> [Double] -> [Double] -> Double -> [Double]
calculateGradientForBook weights features childCentroid adultCentroid label =
  let score = calculatePredictionScore weights features childCentroid adultCentroid
      lossCondition = label * score < 1.0
  in
      if lossCondition
      then
          let -- Neue Formel für die Ableitung, da kein Zentrum mehr Null ist.
              derivative = zipWith3 (\f cc ac -> 2 * (f - ac)^2 - 2 * (f - cc)^2) features childCentroid adultCentroid
          in scaleVector (-label) derivative
      else
          replicate 6 0.0

-- GEÄNDERT: Nimmt beide Zentren entgegen
gradientDescentStep :: [([Double], Double)] -> [Double] -> [Double] -> [Double] -> Double -> [Double]
gradientDescentStep labeledData currentWeights childCentroid adultCentroid learningRate =
  let
      lambda = 0.01
      gradients = map (\(features, label) -> calculateGradientForBook currentWeights features childCentroid adultCentroid label) labeledData
      numDataPoints = fromIntegral $ max 1 (length labeledData)
      totalGradient = foldl' addVectors (replicate 6 0.0) gradients
      avgGradient = scaleVector (1.0 / numDataPoints) totalGradient
      regularizationTerm = scaleVector lambda currentWeights
      gradientWithReg = addVectors avgGradient regularizationTerm
      updatedWeights = zipWith (-) currentWeights (scaleVector learningRate gradientWithReg)
  in
      updatedWeights

-- GEÄNDERT: Die Haupt-Trainingsfunktion
trainWeightsWithGradientDescent ::
  [BookFeatures] ->      -- Kinderbücher
  [BookFeatures] ->      -- Erwachsenenbücher
  CategoryStats ->       -- NEU: Globale Statistiken für die Normalisierung
  (Weights -> IO ()) ->  -- Funktion zur Ausgabe des Fortschritts
  IO Weights             -- Das Endergebnis
trainWeightsWithGradientDescent children adults overallStats printProgress = do
  let
      learningRate = 0.001 -- Fangen wir mit einer kleineren Lernrate an, da die Dynamik anders ist
      epochs = 300
      initialWeights = replicate 6 1.0

      -- 1. Normalisiere ALLE Daten mit den globalen Statistiken
      normalizedChildren = map (normalizeFeatures overallStats) children
      normalizedAdults = map (normalizeFeatures overallStats) adults

      -- 2. Berechne die Zentren der normalisierten Daten
      childCentroid = calculateMeanVector normalizedChildren
      adultCentroid = calculateMeanVector normalizedAdults
      
      -- 3. Erstelle die gelabelten Daten für das Training
      labeledChildren = zip normalizedChildren (repeat (-1.0))
      labeledAdults = zip normalizedAdults (repeat 1.0)
      allLabeledData = labeledChildren ++ labeledAdults

  putStrLn "\nSymmetrische Zentren berechnet:"
  printf "  Kinder-Zentrum (normalisiert): " >> print childCentroid
  printf "  Erwachsenen-Zentrum (normalisiert): " >> print adultCentroid

  let trainingLoop epoch currentWeights
        | epoch > epochs = return currentWeights
        | otherwise = do
            let newWeights = gradientDescentStep allLabeledData currentWeights childCentroid adultCentroid learningRate
            if epoch `mod` 20 == 0
            then printProgress (listToWeights newWeights)
            else return ()
            trainingLoop (epoch + 1) newWeights

  finalWeightsList <- trainingLoop 1 initialWeights
  return $ listToWeights finalWeightsList