{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Types
  ( Classification(..)
  , BookFeatures(..)
  , Weights(..)
  , FeatureStats(..)
  , CategoryStats(..)
  , NormalizedFeatures(..)
  ) where

-- end result 
data Classification
  = Children
  | Adult
  deriving (Show, Eq)

-- metrics for every book
data BookFeatures = BookFeatures
  { avgSentenceLength    :: !Double
  , avgWordLength        :: !Double
  , fleschReadingEase    :: !Double
  , avgSyllablesPerWord  :: !Double
  , uniqueWordRatio      :: !Double
  , sentenceLengthStdDev :: !Double
  , avgCommasPerSentence :: !Double
  } deriving (Eq, Show)

-- represents the metrics of BookFeatures in standardized fashiln (see z standardisation)
data NormalizedFeatures = NormalizedFeatures
  { nfAvgSentenceLength :: !Double
  , nfAvgWordLength     :: !Double
  , nfFleschReadingEase :: !Double
  , nfUniqueWordRatio   :: !Double
  , nfSentLengthStdDev  :: !Double
  , nfAvgCommasPerSentence :: !Double
  } deriving (Show)

-- calculated weights of gradient descent, starting with initial values
-- bias: additional weight which doesnt correspond to a particular feature. 
-- It is also trained and adds an additional "tendency" towards child or adult book
-- Instead of f(x) = w1 * x + ...; -> f(x) = w1 * x + ... + BIAS;
data Weights = Weights
  { wSentenceLength    :: !Double
  , wWordLength        :: !Double
  , wFlesch            :: !Double
  , wUniqueWordRatio   :: !Double
  , wSentLengthStdDev  :: !Double
  , wCommasPerSentence :: !Double
  , bias               :: !Double
  } deriving (Show)

-- calculated for every single feature of every single book
data FeatureStats = FeatureStats
  { mean   :: !Double
  , stdDev :: !Double
  } deriving (Show)

data CategoryStats = CategoryStats
  { sentLengthStats      :: !FeatureStats
  , wordLengthStats      :: !FeatureStats
  , fleschStats          :: !FeatureStats
  , uwrStats             :: !FeatureStats
  , sentLengthStdDevStats:: !FeatureStats
  , commasStats          :: !FeatureStats
  } deriving (Show)