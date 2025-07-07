module CoreLogic where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Set  as Set
import qualified Data.Text as T
import           Types
import           Helpers

extractFeatures :: T.Text -> BookFeatures
extractFeatures text =
  let sentences = getSentences text
      wordLists = getWords sentences
      sentenceLengths = calculateSentenceLengths wordLists
      sentenceLengthsD = map fromIntegral sentenceLengths

      avgSentLen = calculateAvgSentenceLength sentenceLengths
      sentLenMean = calculateMean sentenceLengthsD
      sentLenStdDev = calculateStdDev sentLenMean sentenceLengthsD
      wordLengths = calculateWordLengths wordLists
      avgWordLen = calculateAvgWordLength wordLengths
      syllablesPerWord = calculateSyllablesPerWord wordLists
      avgSyllables = calculateAvgSyllablesPerWord syllablesPerWord
      fleschScore = calculateFleschScore avgSentLen avgSyllables
      uwr = calculateUniqueWordRatio wordLists
      avgCommas = calculateAvgCommasPerSentence sentences
  in BookFeatures
       { avgSentenceLength = avgSentLen
       , avgWordLength = avgWordLen
       , fleschReadingEase = fleschScore
       , avgSyllablesPerWord = avgSyllables
       , uniqueWordRatio = uwr
       , sentenceLengthStdDev = sentLenStdDev
       , avgCommasPerSentence = avgCommas
       }

calculateFeatureStats :: [Double] -> FeatureStats
calculateFeatureStats xs =
  let m = calculateMean xs
      s = calculateStdDev m xs
  in FeatureStats {mean = m, stdDev = s}

calculateGlobalStats :: [BookFeatures] -> CategoryStats
calculateGlobalStats features =
  CategoryStats
    { sentLengthStats = calculateFeatureStats $ L.map avgSentenceLength features
    , wordLengthStats = calculateFeatureStats $ L.map avgWordLength features
    , fleschStats = calculateFeatureStats $ L.map fleschReadingEase features
    , uwrStats = calculateFeatureStats $ L.map uniqueWordRatio features
    , sentLengthStdDevStats = calculateFeatureStats $ L.map sentenceLengthStdDev features
    , commasStats = calculateFeatureStats $ L.map avgCommasPerSentence features
    }

normalizeValue :: Double -> FeatureStats -> Double
normalizeValue val stats
  | stdDev stats == 0 = 0.0
  | otherwise = (val - mean stats) / stdDev stats

normalizeFeatures :: BookFeatures -> CategoryStats -> [Double]
normalizeFeatures features stats =
  [ normalizeValue (avgSentenceLength features) (sentLengthStats stats)
  , normalizeValue (avgWordLength features) (wordLengthStats stats)
  , normalizeValue (fleschReadingEase features) (fleschStats stats)
  , normalizeValue (uniqueWordRatio features) (uwrStats stats)
  , normalizeValue (sentenceLengthStdDev features) (sentLengthStdDevStats stats)
  , normalizeValue (avgCommasPerSentence features) (commasStats stats)
  ]

predict :: Weights -> [Double] -> Double
predict weights features =
  let z = (wSentenceLength weights * features !! 0) +
          (wWordLength weights * features !! 1) +
          (wFlesch weights * features !! 2) +
          (wUniqueWordRatio weights * features !! 3) +
          (wSentLengthStdDev weights * features !! 4) +
          (wCommasPerSentence weights * features !! 5) +
          bias weights
  in sigmoid z

calculateGradient :: Weights -> ([Double], Double) -> Weights
calculateGradient weights (features, y) =
  let prediction = predict weights features
      error' = prediction - y
  in Weights
       { wSentenceLength = error' * (features !! 0)
       , wWordLength = error' * (features !! 1)
       , wFlesch = error' * (features !! 2)
       , wUniqueWordRatio = error' * (features !! 3)
       , wSentLengthStdDev = error' * (features !! 4)
       , wCommasPerSentence = error' * (features !! 5)
       , bias = error'
       }

updateWeights :: Weights -> Weights -> Double -> Double -> Weights
updateWeights oldWeights avgGrad learningRate lambda =
  Weights
    { wSentenceLength = wSentenceLength oldWeights - learningRate * (wSentenceLength avgGrad + lambda * wSentenceLength oldWeights)
    , wWordLength = wWordLength oldWeights - learningRate * (wWordLength avgGrad + lambda * wWordLength oldWeights)
    , wFlesch = wFlesch oldWeights - learningRate * (wFlesch avgGrad + lambda * wFlesch oldWeights)
    , wUniqueWordRatio = wUniqueWordRatio oldWeights - learningRate * (wUniqueWordRatio avgGrad + lambda * wUniqueWordRatio oldWeights)
    , wSentLengthStdDev = wSentLengthStdDev oldWeights - learningRate * (wSentLengthStdDev avgGrad + lambda * wSentLengthStdDev oldWeights)
    , wCommasPerSentence = wCommasPerSentence oldWeights - learningRate * (wCommasPerSentence avgGrad + lambda * wCommasPerSentence oldWeights)
    , bias = bias oldWeights - learningRate * bias avgGrad
    }

trainSingleEpoch :: Double -> Double -> [([Double], Double)] -> Weights -> Weights
trainSingleEpoch learningRate lambda trainingData currentWeights =
  let gradients = L.map (calculateGradient currentWeights) trainingData
      numSamples = fromIntegral $ L.length trainingData
      avgGrad =
        Weights
          { wSentenceLength = L.sum (L.map wSentenceLength gradients) / numSamples
          , wWordLength = L.sum (L.map wWordLength gradients) / numSamples
          , wFlesch = L.sum (L.map wFlesch gradients) / numSamples
          , wUniqueWordRatio = L.sum (L.map wUniqueWordRatio gradients) / numSamples
          , wSentLengthStdDev = L.sum (L.map wSentLengthStdDev gradients) / numSamples
          , wCommasPerSentence = L.sum (L.map wCommasPerSentence gradients) / numSamples
          , bias = L.sum (L.map bias gradients) / numSamples
          }
  in updateWeights currentWeights avgGrad learningRate lambda

trainModel :: Double -> Double -> Int -> [([Double], Double)] -> Weights -> Weights
trainModel learningRate lambda epochs trainingData initialWeights =
  L.foldl' (\w _ -> trainSingleEpoch learningRate lambda trainingData w) initialWeights [1 .. epochs]

classify :: Weights -> CategoryStats -> BookFeatures -> Classification
classify weights stats features =
  let normalized = normalizeFeatures features stats
      prediction = predict weights normalized
  in if prediction > 0.5 then Adult else Children