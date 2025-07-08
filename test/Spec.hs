module Main where

import Test.HUnit
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.List as L

import Types
import Helpers 

(~?~) :: Double -> Double -> Assertion
x ~?~ y = assertBool msg (abs (x - y) < epsilon)
  where
    epsilon = 1e-9
    msg = "Expected: " ++ show y ++ "\nBut got:  " ++ show x

--------------------------------------------------------------------------------
-- HUnit Tests 
--------------------------------------------------------------------------------

hunitTests :: Test
hunitTests = TestList
  [ "Text Analysis Functions" ~: testTextAnalysis
  , "Statistics Functions" ~: testStatistics
  ]

testTextAnalysis :: Test
testTextAnalysis = TestList
  [ "countSyllablesInWord" ~:
    [ "empty"         ~: countSyllablesInWord (T.pack "") ~?= 0
    , "simple"        ~: countSyllablesInWord (T.pack "haskell") ~?= 2
    , "complex"       ~: countSyllablesInWord (T.pack "monad") ~?= 2
    , "no vowels"     ~: countSyllablesInWord (T.pack "rhythm") ~?= 1
    ]
  
  , "calculateUniqueWordRatio" ~:
    [ "all unique" ~: calculateUniqueWordRatio [[T.pack "a"], [T.pack "b"], [T.pack "c"]] ~?~ 1.0
    , "some repeated" ~: calculateUniqueWordRatio [[T.pack "a"], [T.pack "b"], [T.pack "a"]] ~?~ (2.0 / 3.0)
    , "empty list" ~: calculateUniqueWordRatio [] ~?~ 0.0
    ]
  ]

testStatistics :: Test
testStatistics = TestList
  [ "safeDiv" ~:
    [ "division by zero" ~: safeDiv 100 0 ~?= 0.0
    , "normal division"  ~: safeDiv 10 4 ~?= 2.5
    ]
  , "calculateMean" ~:
    [ "simple list" ~: calculateMean [1, 2, 3, 4, 5] ~?= 3.0
    , "empty list"  ~: calculateMean [] ~?= 0.0
    ]
  ]

--------------------------------------------------------------------------------
-- QuickCheck Properties 
--------------------------------------------------------------------------------

prop_meanOfIdenticalNumbersIsItself :: Double -> NonEmptyList Double -> Bool
prop_meanOfIdenticalNumbersIsItself x (NonEmpty xs) =
  let list = replicate (length xs) x
      mean = calculateMean list
  in abs (mean - x) < 1e-9

-- Die Standardabweichung darf niemals negativ sein.
prop_stdDevIsNonNegative :: [Double] -> Bool
prop_stdDevIsNonNegative xs =
  let m = calculateMean xs
  in calculateStdDev m xs >= 0.0

-- Die Sigmoid-Funktion muss immer einen Wert zwischen 0 und 1 liefern.
prop_sigmoidIsBounded :: Double -> Bool
prop_sigmoidIsBounded z = let s = sigmoid z in s >= 0.0 && s <= 1.0

-- Die Normalisierung eines Wertes und die sofortige Umkehrung der Operation
-- muss den ursprünglichen Wert ergeben (innerhalb der Toleranz).
prop_normalizeInverse :: Double -> Property
prop_normalizeInverse val =
  -- Wir stellen sicher, dass die Standardabweichung nicht 0 ist,
  -- um eine Division durch Null in der Umkehrrechnung zu vermeiden.
  forAll (arbitrary `suchThat` (\x -> abs x > 1e-6)) $ \stdDevVal ->
  forAll arbitrary $ \meanVal ->
    let stats = FeatureStats { mean = meanVal, stdDev = stdDevVal }
        normalized = normalizeValue val stats
        -- Die Umkehrformel: original = normalized * stdDev + mean
        denormalized = normalized * stdDevVal + meanVal
    in abs (val - denormalized) < 1e-9



main :: IO ()
main = do
  putStrLn "\nRunning HUnit Tests..."
  _ <- runTestTT hunitTests

  putStrLn "\nRunning QuickCheck Properties..."
  quickCheck (withMaxSuccess 1000 prop_meanOfIdenticalNumbersIsItself)
  quickCheck (withMaxSuccess 1000 prop_stdDevIsNonNegative)
  quickCheck (withMaxSuccess 1000 prop_sigmoidIsBounded)
  quickCheck (withMaxSuccess 100 prop_normalizeInverse) -- Weniger, da komplexer
