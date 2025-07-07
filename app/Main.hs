module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import Control.Monad (forM)
import System.CPUTime
import Text.Printf
import Control.Concurrent.Async (mapConcurrently)
import Data.List (maximumBy)

import Types
import CoreLogic

timeSth :: String -> IO a -> IO a 
timeSth label action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "%s: %.3f sec\n" label (diff :: Double)
    return result

printFeatures :: BookFeatures -> IO ()
printFeatures (BookFeatures fp slen commas uniq wl flesch totalLen) = do
  putStrLn $ "File: " ++ fp
  printf "  avgSentenceLength:     %.2f\n" slen
  printf "  avgCommasPerSentence:  %.2f\n" commas
  printf "  uniqueWordRatio:       %.3f\n" uniq
  printf "  avgWordLength:         %.2f\n" wl
  printf "  FleschReadingEase:     %.2f\n" flesch
  printf "  totalLength:           %d\n" totalLen

-- Recursively process all files in a directory concurrently and extract features
processDir :: FilePath -> IO [BookFeatures]
processDir dir = do
  files <- listDirectory dir
  mapConcurrently (\file -> do
    let fullPath = dir </> file
    content <- TIO.readFile fullPath
    -- putStrLn $ "Verarbeite " ++ fullPath
    return $ extractFeaturesFromText fullPath content
    ) files

testChildrenAccuracy :: Thresholds -> IO ()
testChildrenAccuracy thresholds = do
  children <- processDir "books/children"
  let total = length children
      correct = length $ filter (\bf -> classifyBook thresholds bf == ChildrensBook) children
      percent = fromIntegral correct / fromIntegral total * 100
  putStrLn $ "Kinderbücher korrekt klassifiziert: " ++ show correct ++ " / " ++ show total
  printf "Trefferquote: %.2f%%\n" (percent :: Double)

testAdultAccuracy :: Thresholds -> IO ()
testAdultAccuracy thresholds = do
  adults <- processDir "books/adults"
  let total = length adults
      correct = length $ filter (\bf -> classifyBook thresholds bf == AdultBook) adults
      percent = fromIntegral correct / fromIntegral total * 100
  putStrLn $ "Erwachsenenbücher korrekt klassifiziert: " ++ show correct ++ " / " ++ show total
  printf "Trefferquote: %.2f%%\n" (percent :: Double)

-- Modified: evaluateAccuracy now prints correct counts and percentages and returns IO Double
evaluateAccuracy :: [BookFeatures] -> [BookFeatures] -> Thresholds -> Weights -> CategoryStats -> CategoryStats -> IO Double
evaluateAccuracy children adults thresholds weights childStats adultStats = do
  let
    correctChildren = length $ filter (\bf -> classifyCombined weights childStats adultStats bf == ChildrensBook) children
    correctAdults   = length $ filter (\bf -> classifyCombined weights childStats adultStats bf == AdultBook) adults
    totalChildren = length children
    totalAdults = length adults
    accChildren = if totalChildren == 0 then 0 else fromIntegral correctChildren / fromIntegral totalChildren
    accAdults   = if totalAdults == 0 then 0 else fromIntegral correctAdults / fromIntegral totalAdults
    accMin = min accChildren accAdults

  putStrLn $ "Kinderbücher korrekt klassifiziert: " ++ show correctChildren ++ " / " ++ show totalChildren
  printf "Trefferquote Kinder: %.2f%%\n" (accChildren * 100)
  putStrLn $ "Erwachsenenbücher korrekt klassifiziert: " ++ show correctAdults ++ " / " ++ show totalAdults
  printf "Trefferquote Erwachsene: %.2f%%\n" (accAdults * 100)
  printf "Beste kombinierte Genauigkeit: %.2f%%\n" (accMin * 100)

  return accMin



weightGrid :: [Double]
weightGrid = [0.5,1.0,1.5,2.0,2.5,3.0,4.0,5.0,8.0,13.0,21.0]


generateWeightCombos :: [Weights]
generateWeightCombos =
  [ Weights w1 w2 w3 w4 w5 w6
  | w1 <- weightGrid
  , w2 <- weightGrid
  , w3 <- weightGrid
  , w4 <- weightGrid
  , w5 <- weightGrid
  , w6 <- weightGrid
  , (w1 + w2 + w3 + w4 + w5 + w6) > 0
  ]

findBestWeights :: [BookFeatures] -> [BookFeatures] -> Thresholds -> IO Weights
findBestWeights children adults thresholds = do
  let childStats = calculateCategoryStats "children" children
      adultStats = calculateCategoryStats "adults" adults
      candidates = generateWeightCombos
      scored = [ (ws, evaluateAccuracyPure children adults thresholds ws childStats adultStats) | ws <- candidates ]
      best = maximumBy (\(_, acc1) (_, acc2) -> compare acc1 acc2) scored
  putStrLn $ "Beste Gewichte: " ++ show (fst best)
  printf "Genauigkeit: %.2f%%\n" (snd best * 100)
  return (fst best)
-- Helper pure evaluation for findBestWeights to avoid IO in scoring
evaluateAccuracyPure :: [BookFeatures] -> [BookFeatures] -> Thresholds -> Weights -> CategoryStats -> CategoryStats -> Double
evaluateAccuracyPure children adults thresholds weights childStats adultStats =
  let
    correctChildren = length $ filter (\bf -> classifyCombined weights childStats adultStats bf == ChildrensBook) children
    correctAdults   = length $ filter (\bf -> classifyCombined weights childStats adultStats bf == AdultBook) adults
    totalChildren = length children
    totalAdults = length adults
    accChildren = if totalChildren == 0 then 0 else fromIntegral correctChildren / fromIntegral totalChildren
    accAdults   = if totalAdults == 0 then 0 else fromIntegral correctAdults / fromIntegral totalAdults
  in
    min accChildren accAdults

testChildrenAccuracyWithWeights :: Thresholds -> Weights -> IO ()
testChildrenAccuracyWithWeights thresholds weights = do
  children <- processDir "books/children"
  let total = length children
      correct = length $ filter (\bf -> classifyWithWeights thresholds weights bf == ChildrensBook) children
      percent = fromIntegral correct / fromIntegral total * 100
  putStrLn $ "Kinderbücher korrekt klassifiziert: " ++ show correct ++ " / " ++ show total
  printf "Trefferquote: %.2f%%\n" (percent :: Double)

testAdultAccuracyWithWeights :: Thresholds -> Weights -> IO ()
testAdultAccuracyWithWeights thresholds weights = do
  adults <- processDir "books/adults"
  let total = length adults
      correct = length $ filter (\bf -> classifyWithWeights thresholds weights bf == AdultBook) adults
      percent = fromIntegral correct / fromIntegral total * 100
  putStrLn $ "Erwachsenenbücher korrekt klassifiziert: " ++ show correct ++ " / " ++ show total
  printf "Trefferquote: %.2f%%\n" (percent :: Double)

calculateCategoryStats :: String -> [BookFeatures] -> CategoryStats
calculateCategoryStats label bfs =
  let slens   = map avgSentenceLength bfs
      commas  = map avgCommasPerSentence bfs
      ratios  = map uniqueWordRatio bfs
      wordLen = map avgWordLength bfs
      flesch  = map fleschReadingEase bfs
      lengths = map (fromIntegral . totalLength) bfs  -- convert Int to Double
  in CategoryStats
       label
       (calcStats slens)
       (calcStats commas)
       (calcStats ratios)
       (calcStats wordLen)
       (calcStats flesch)
       (calcStats lengths)

printCategoryStats :: CategoryStats -> IO ()
printCategoryStats (CategoryStats label slen comma ratio wordLen flesch length) = do
  putStrLn $ "Statistiken für Kategorie: " ++ label
  putStrLn "---------------------------------------"
  printStat "Satzlänge" slen
  printStat "Kommas pro Satz" comma
  printStat "Wortvielfalt (Unique Ratio)" ratio
  printStat "Wortlänge" wordLen
  printStat "Flesch Reading Ease" flesch
  printStat "Buchlänge" length
  putStrLn ""
  where
    printStat :: String -> FeatureStats -> IO ()
    printStat name (FeatureStats mean var stddev) = do
      printf "%-25s Mittelwert: %.3f  Varianz: %.3f  StdAbw: %.3f\n"
        name mean var stddev


testDistanceAccuracy :: CategoryStats -> CategoryStats -> IO ()
testDistanceAccuracy childStats adultStats = do
  children <- processDir "books/children"
  adults   <- processDir "books/adults"

  let correctChildren = length $ filter (\bf -> classifyByDistance childStats adultStats bf == ChildrensBook) children
      correctAdults   = length $ filter (\bf -> classifyByDistance childStats adultStats bf == AdultBook) adults
      totalChildren   = length children
      totalAdults     = length adults
      accChildren     = if totalChildren == 0 then 0.0 else (fromIntegral correctChildren / fromIntegral totalChildren) :: Double
      accAdults       = if totalAdults == 0 then 0.0 else (fromIntegral correctAdults / fromIntegral totalAdults) :: Double

  putStrLn $ "Kinderbücher korrekt klassifiziert (Distanzmethode): " ++ show correctChildren ++ " / " ++ show totalChildren
  printf "Trefferquote Kinder (Distanz): %.2f%%\n" (accChildren * 100)
  putStrLn $ "Erwachsenenbücher korrekt klassifiziert (Distanzmethode): " ++ show correctAdults ++ " / " ++ show totalAdults
  printf "Trefferquote Erwachsene (Distanz): %.2f%%\n" (accAdults * 100)


main :: IO ()
main = do
  putStrLn "Processing children books..."
  children <- processDir "books/children"
  putStrLn "Processing adult books..."
  adults <- processDir "books/adults"

  let childStats = calculateCategoryStats "children" children
      adultStats = calculateCategoryStats "adults" adults

  let thresholds = calculateThresholds children adults

  putStrLn "Calculated thresholds:"
  print thresholds

  putStrLn "Searching for best weights..."
  bestWeights <- findBestWeights children adults thresholds

  putStrLn $ "Best weights found: " ++ show bestWeights

  acc <- evaluateAccuracy children adults thresholds bestWeights childStats adultStats
  printf "Best combined weighted distance classification accuracy: %.2f%%\n" (acc * 100)

  printCategoryStats childStats
  printCategoryStats adultStats
