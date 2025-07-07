{-# LANGUAGE BangPatterns #-}

module Types (
  BookFeatures(..),
  Thresholds(..),
  Classification(..),
  Weights(..),
  FeatureStats(..),
  CategoryStats(..),
) where

-- Merkmale, die aus einem einzigen Buch extrahiert werden
data BookFeatures = BookFeatures {
  filePath               :: !FilePath, 
  avgSentenceLength      :: !Double,
  avgCommasPerSentence   :: !Double,
  uniqueWordRatio        :: !Double, 
  avgWordLength          :: !Double,
  fleschReadingEase      :: !Double,
  totalLength            :: !Int
} deriving (Show, Eq)

-- Die berechneten Schwellenwerte, die Kinder- von Erwachsenenbüchern trennen
data Thresholds = Thresholds {
  sentenceLengthThreshold :: Double,
  commasThreshold         :: Double,
  ratioThreshold          :: Double,
  wordLengthThreshold     :: Double,
  fleschThreshold         :: Double,
  totalLengthThreshold    :: Double
} deriving (Show)

-- Das Endergebnis der Klassifizierung
data Classification = ChildrensBook | AdultBook | Uncertain
  deriving (Show, Eq)

-- Gewichte für die Merkmale, die in der Klassifizierung verwendet werden
data Weights = Weights {
  wSentenceLength     :: Double,
  wCommasPerSentence  :: Double,
  wUniqueWordRatio    :: Double,
  wAvgWordLength      :: Double,
  wFleschReadingEase  :: Double,
  wTotalLength       :: Double
} deriving (Show, Eq)

-- Statistische Merkmale, die für die Klassifizierung verwendet werden
-- Diese Struktur speichert den Mittelwert, die Varianz und die Standardabweichung
data FeatureStats = FeatureStats
  { mean :: Double
  , variance :: Double
  , stddev :: Double
  } deriving (Show)

-- Statistiken für eine Kategorie (z.B. Kinder- oder Erwachsenenbücher)
data CategoryStats = CategoryStats
  { name :: String
  , slenStats :: FeatureStats
  , commaStats :: FeatureStats
  , uniqRatioStats :: FeatureStats
  , wordLenStats :: FeatureStats
  , fleschStats :: FeatureStats
  , lengthStats :: FeatureStats
  } deriving (Show)
