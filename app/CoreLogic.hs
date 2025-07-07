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

classifyCombined :: Weights -> CategoryStats -> CategoryStats -> BookFeatures -> Classification
classifyCombined weights childStats adultStats book =
  let
    toStatsList cat = [slenStats cat, commaStats cat, uniqRatioStats cat, wordLenStats cat, fleschStats cat, lengthStats cat]
    normalized = normalizeFeatures childStats book
    wList = weightsToList weights -- Verwendung der neuen Hilfsfunktion
    weightedFeatures = zipWith (*) normalized wList
    normalizeMeanPair childStat adultStat = zNormalize childStat (mean adultStat)
    childCentroid = replicate 6 0.0
    adultCentroid = zipWith (*) 
                      (zipWith normalizeMeanPair (toStatsList childStats) (toStatsList adultStats))
                      wList
    euclideanDistance xs ys = sqrt . sum $ zipWith (\x y -> (x - y)^2) xs ys
    distToChild = euclideanDistance weightedFeatures childCentroid
    distToAdult = euclideanDistance weightedFeatures adultCentroid
  in if distToChild < distToAdult then ChildrensBook else AdultBook

-- #############################################################################
-- NEUE FUNKTIONEN FÜR DEN GRADIENTENABSTIEG
-- #############################################################################

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

-- Berechnet den "Score" für ein Buch.
-- Positiv = tendiert zu Adult, Negativ = tendiert zu Child.
-- Wir verwenden die quadrierten Distanzen, um die Wurzel zu vermeiden. Das ist einfacher und schneller.
calculatePredictionScore :: [Double] -> [Double] -> [Double] -> Double
calculatePredictionScore weights normalizedFeatures adultCentroid =
  let weightedFeatures = zipWith (*) weights normalizedFeatures
      weightedAdultCentroid = zipWith (*) weights adultCentroid

      -- Quadrierte Euklidische Distanz (ohne die Wurzel)
      distSq xs ys = sum $ zipWith (\x y -> (x - y)^2) xs ys

      distToChildSq = distSq weightedFeatures (replicate 6 0.0)
      distToAdultSq = distSq weightedFeatures weightedAdultCentroid
  in
      -- Score: Distanz zum Kind - Distanz zum Erwachsenen
      -- Ein negativer Score bedeutet, es ist näher am Kind.
      distToChildSq - distToAdultSq

-- Berechnet den Gradienten für EIN EINZIGES Buch.
-- Der Gradient ist der Vektor, der in die Richtung des steilsten Fehleranstiegs zeigt.
calculateGradientForBook :: [Double] -> [Double] -> [Double] -> Double -> [Double]
calculateGradientForBook weights features adultCentroid label =
  let score = calculatePredictionScore weights features adultCentroid
      -- Hinge Loss: Wir bestrafen nur, wenn label * score < 1
      -- (falsch klassifiziert oder zu nah an der Grenze)
      -- Label: -1 für Kind, +1 für Erwachsener
      lossCondition = label * score < 1.0
  in
      if lossCondition
      then
          -- Ableitung des Scores nach den Gewichten, skaliert mit dem Label.
          -- Dies ist der "magische" Teil, aber er ergibt sich direkt aus der Formel für den Score.
          let derivative = zipWith (\f c -> 2 * (f - c) * f) features adultCentroid
          in scaleVector (-label) derivative
      else
          -- Wenn die Klassifizierung korrekt und sicher ist, ist der Gradient 0.
          replicate 6 0.0

-- Führt EINEN Schritt des Gradientenabstiegs für den gesamten Datensatz aus.
gradientDescentStep :: [([Double], Double)] -> [Double] -> [Double] -> Double -> [Double]
gradientDescentStep labeledData currentWeights adultCentroid learningRate =
  let
      -- NEU: Regularisierungs-Parameter (Lambda). Das sind die "Zügel".
      lambda = 0.01 -- Ein guter Startwert.

      gradients = map (\(features, label) -> calculateGradientForBook currentWeights features adultCentroid label) labeledData
      
      numDataPoints = fromIntegral $ max 1 (length labeledData)
      totalGradient = foldl' addVectors (replicate 6 0.0) gradients
      avgGradient = scaleVector (1.0 / numDataPoints) totalGradient

      -- HIER IST DIE NEUE ZEILE: Füge die "Strafe" für große Gewichte hinzu.
      regularizationTerm = scaleVector lambda currentWeights
      gradientWithReg = addVectors avgGradient regularizationTerm

      -- Aktualisiere die Gewichte mit dem neuen, regulierten Gradienten.
      updatedWeights = zipWith (-) currentWeights (scaleVector learningRate gradientWithReg)
  in
      updatedWeights

-- Die Haupt-Trainingsfunktion, die von Main aufgerufen wird.
trainWeightsWithGradientDescent ::
  [BookFeatures] -> -- Kinderbücher
  [BookFeatures] -> -- Erwachsenenbücher
  CategoryStats ->  -- Kinder-Statistiken
  CategoryStats ->  -- Erwachsenen-Statistiken
  (Weights -> IO ()) -> -- Funktion zur Ausgabe des Fortschritts
  IO Weights        -- Das Endergebnis
trainWeightsWithGradientDescent children adults childStats adultStats printProgress = do
  let -- Trainings-Parameter
      learningRate = 0.007
      epochs = 300 -- Anzahl der Durchläufe über den gesamten Datensatz
      initialWeights = replicate 6 1.0 -- Startpunkt: alle Gewichte sind 1.0

      -- Bereite die Trainingsdaten vor: Normalisierte Vektoren mit Labels
      normalizeAndLabel bookfs label = map (\bf -> (normalizeFeatures childStats bf, label)) bookfs
      labeledChildren = normalizeAndLabel children (-1.0)
      labeledAdults = normalizeAndLabel adults 1.0
      allLabeledData = labeledChildren ++ labeledAdults

      -- Der normalisierte Centroid der Erwachsenenbücher (Kinder-Centroid ist der Nullvektor)
      adultCentroid = zipWith (\sChild sAdult -> zNormalize sChild (mean sAdult))
                              (toStatsList childStats)
                              (toStatsList adultStats)

  -- Rekursive Schleife für das Training
  let trainingLoop epoch currentWeights
        | epoch > epochs = return currentWeights -- Training beendet
        | otherwise = do
            -- Führe einen Trainingsschritt aus
            let newWeights = gradientDescentStep allLabeledData currentWeights adultCentroid learningRate
            
            -- Gib den Fortschritt aus (alle 20 Epochen)
            if epoch `mod` 20 == 0
            then printProgress (listToWeights newWeights)
            else return ()

            -- Rufe die Schleife für die nächste Epoche auf
            trainingLoop (epoch + 1) newWeights

  -- Starte die Schleife und konvertiere das Ergebnis zurück in den Weights-Typ
  finalWeightsList <- trainingLoop 1 initialWeights
  return $ listToWeights finalWeightsList