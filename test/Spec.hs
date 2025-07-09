-- Test.hs
module Main where

import Test.HUnit
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.List as L

import Types
import Helpers
import CoreLogic

-- Operator für Double-Vergleiche mit Toleranz
(~?~) :: Double -> Double -> Assertion
x ~?~ y = assertBool msg (abs (x - y) < epsilon)
  where
    epsilon = 1e-2 -- A slightly larger epsilon can be more robust for complex calculations
    msg = "Expected: " ++ show y ++ "\nBut got:  " ++ show x

-- Haupt-Test-Suite
hunitTests :: Test
hunitTests = TestList
  [ "Text Analysis Functions" ~: testTextAnalysis
  , "Statistics Functions"    ~: testStatistics
  , "Core Logic Functions"    ~: testCoreLogic
  ]

testTextAnalysis :: Test
testTextAnalysis = TestList
  [ 
    "countSyllablesInWord" ~:
    [ "empty"         ~: countSyllablesInWord (T.pack "") ~?= 0
    , "simple"        ~: countSyllablesInWord (T.pack "haskell") ~?= 2
    , "complex"       ~: countSyllablesInWord (T.pack "monad") ~?= 2
    , "consecutive"   ~: countSyllablesInWord (T.pack "beautiful") ~?= 3
    , "no vowels"     ~: countSyllablesInWord (T.pack "rhythm") ~?= 1
    ]
    , "calculateUniqueWordRatio" ~:
    [ "all unique"    ~: assertEqual "" (Just 1.0) (calculateUniqueWordRatio [[T.pack "a"], [T.pack "b"], [T.pack "c"]])
    , "some repeated" ~: assertEqual "" (Just (2.0 / 3.0)) (calculateUniqueWordRatio [[T.pack "a"], [T.pack "b"], [T.pack "a"]])
    , "empty list"    ~: assertEqual "" Nothing (calculateUniqueWordRatio [])
    ]
    , "getSentences" ~:
    [ "no periods" ~: getSentences (T.pack "hello world") ~?= [T.pack "hello world"]
    , "multiple"   ~: getSentences (T.pack "First. Second.") ~?= [T.pack "First", T.pack " Second"]
    , "empty"      ~: getSentences (T.pack "") ~?= []
    ]
    , "getWords" ~:
    [ "simple" ~: getWords [T.pack "hello world", T.pack "BRO"] ~?= [[T.pack "hello", T.pack "world"], [T.pack "bro"]]
    , "two empty strings" ~: getWords [T.pack "", T.pack ""] ~?= [[], []]
    ]
    , "calculateSentenceLengths" ~:
    [ "simple case" ~:
        calculateSentenceLengths [[T.pack "a", T.pack "b"], [T.pack "c"]] ~?= [2,1]
    , "empty input" ~:
        calculateSentenceLengths [] ~?= []
    , "empty sentence" ~:
        calculateSentenceLengths [[]] ~?= [0]
    ]
    , "calculateAvgSentenceLength" ~: 
    [ "simple average" ~: calculateAvgSentenceLength [4,6] ~?= Just 5.0
    , "empty list" ~: calculateAvgSentenceLength [] ~?= Nothing
    ]
    , "calculateSyllablesPerWord" ~:
    [ "one word, three syllables" ~:
        calculateSyllablesPerWord [[T.pack "banana"]] ~?= [3]
    , "multiple words" ~:
        calculateSyllablesPerWord [[T.pack "hello", T.pack "world"]] ~?= [2,1]
    , "empty list" ~:
        calculateSyllablesPerWord [] ~?= []
    , "inner empty" ~:
        calculateSyllablesPerWord [[]] ~?= []
    , "punctuation stays" ~:
        calculateSyllablesPerWord [[T.pack "wow!"]] ~?= [1]
    ]
  ]

testStatistics :: Test
testStatistics = TestList
  [ "safeDiv" ~:
    [ "division by zero" ~: assertEqual "" Nothing (safeDiv 100 0)
    , "normal division"  ~: assertEqual "" (Just 2.5) (safeDiv 10 4)
    ]
  , "calculateMean" ~:
    [ "simple list" ~: calculateMean [1, 2, 3, 4, 5] ~?= Just 3.0
    , "empty list"  ~: calculateMean [] ~?= Nothing
    ]
  , "calculateStdDev" ~:
    [ "no deviation" ~: calculateStdDev (Just 5.0) [5, 5, 5, 5] ~?= Just 0.0
    , "single item"  ~: calculateStdDev (Just 10.0) [10] ~?= Just 0.0
    , "no mean"      ~: calculateStdDev Nothing [1,2,3] ~?= Nothing
    ]
  ]

-- Integration test of extractFeature monad chain
-- two cases since return type is maybe
testCoreLogic :: Test
testCoreLogic = TestList
  [
    "extractFeatures Integration Test" ~: TestCase $ do
      let testText = T.pack "Ein einfacher Satz. Und, noch ein Satz."
      case extractFeatures testText of
        Nothing -> assertFailure "Feature extraction failed unexpectedly."
        Just features -> do
          avgSentenceLength features ~?~ 3.5
          avgWordLength features ~?~ 4.428
          uniqueWordRatio features ~?~ 0.714
          fleschReadingEase features ~?~ 94.51

    , "extractFeatures on Empty" ~: assertEqual "" Nothing (extractFeatures (T.pack ""))
    , "extractFeatures on only periods" ~: assertEqual "" Nothing (extractFeatures (T.pack "......."))
  ]

--------------------------------------------------------------------------------
-- QuickCheck Properties
--------------------------------------------------------------------------------

quickCheckProperties :: IO ()
quickCheckProperties = do
  putStrLn "\nRunning QuickCheck Properties..."
  quickCheck (withMaxSuccess 1000 prop_meanOfIdenticalNumbersIsItself)
  quickCheck (withMaxSuccess 1000 prop_stdDevIsNonNegative)
  quickCheck (withMaxSuccess 1000 prop_sigmoidIsBounded)

prop_meanOfIdenticalNumbersIsItself :: Double -> NonEmptyList Double -> Bool
prop_meanOfIdenticalNumbersIsItself x (NonEmpty xs) =
  let list = replicate (length xs) x
      maybeMean = calculateMean list
  in case maybeMean of
       Nothing -> False
       Just mean -> abs (mean - x) < 1e-9

prop_stdDevIsNonNegative :: [Double] -> Bool
prop_stdDevIsNonNegative xs =
  let maybeMean = calculateMean xs
      maybeStdDev = calculateStdDev maybeMean xs
  in case maybeStdDev of
       Nothing -> True
       Just stdDev -> stdDev >= 0.0

prop_sigmoidIsBounded :: Double -> Bool
prop_sigmoidIsBounded z = let s = sigmoid z in s >= 0.0 && s <= 1.0

main :: IO ()
main = do
  putStrLn "\nRunning HUnit Tests..."
  _ <- runTestTT hunitTests
  quickCheckProperties