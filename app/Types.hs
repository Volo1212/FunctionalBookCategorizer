module Types (
  BookFeatures(..),
  Thresholds(..),
  Classification(..)
) where

-- Merkmale, die aus einem einzigen Buch extrahiert werden
data BookFeatures = BookFeatures {
  filePath             :: FilePath, -- Nützlich, um zu wissen, woher die Daten kamen
  avgSentenceLength    :: Double,
  avgCommasPerSentence :: Double,
  uniqueWordRatio       :: Double, 
  avgWordLength        :: Double  
} deriving (Show, Eq)

-- Die berechneten Schwellenwerte, die Kinder- von Erwachsenenbüchern trennen
data Thresholds = Thresholds {
  sentenceLengthThreshold :: Double,
  commasThreshold         :: Double,
  ratioThreshold          :: Double,
  wordLengthThreshold     :: Double
} deriving (Show)

-- Das Endergebnis der Klassifizierung
data Classification = ChildrensBook | AdultBook | Uncertain
  deriving (Show, Eq)