module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import System.CPUTime
import Text.Printf
import Control.Concurrent.Async (mapConcurrently)

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

processDir :: FilePath -> IO [BookFeatures]
processDir dir = do
  files <- listDirectory dir
  mapConcurrently (\file -> do
    let fullPath = dir </> file
    content <- TIO.readFile fullPath
    return $ extractFeaturesFromText fullPath content
    ) files
    
calculateCategoryStats :: String -> [BookFeatures] -> CategoryStats
calculateCategoryStats label bfs =
  let slens   = map avgSentenceLength bfs
      commas  = map avgCommasPerSentence bfs
      ratios  = map uniqueWordRatio bfs
      wordLen = map avgWordLength bfs
      flesch  = map fleschReadingEase bfs
      lengths = map (fromIntegral . totalLength) bfs
  in CategoryStats label (calcStats slens) (calcStats commas) (calcStats ratios) (calcStats wordLen) (calcStats flesch) (calcStats lengths)

printCategoryStats :: CategoryStats -> IO ()
printCategoryStats (CategoryStats label slen comma ratio wordLen flesch length) = do
  putStrLn $ "\nStatistiken für Kategorie: " ++ label
  putStrLn "---------------------------------------"
  printStat "Satzlänge" slen
  printStat "Kommas pro Satz" comma
  printStat "Wortvielfalt (Unique Ratio)" ratio
  printStat "Wortlänge" wordLen
  printStat "Flesch Reading Ease" flesch
  printStat "Buchlänge" length
  putStrLn ""
  where
    printStat name (FeatureStats mean var stddev) =
      printf "%-25s Mittelwert: %.3f  Varianz: %.3f  StdAbw: %.3f\n" name mean var stddev

-- GEÄNDERT: Die Auswertungsfunktion muss jetzt auch die globalen Stats bekommen
evaluateAccuracy :: [BookFeatures] -> [BookFeatures] -> Weights -> CategoryStats -> CategoryStats -> CategoryStats -> IO Double
evaluateAccuracy children adults weights overallStats childStats adultStats = do
  let
    correctChildren = length $ filter (\bf -> classifyCombined weights overallStats childStats adultStats bf == ChildrensBook) children
    correctAdults   = length $ filter (\bf -> classifyCombined weights overallStats childStats adultStats bf == AdultBook) adults
    totalChildren = length children
    totalAdults = length adults
    accChildren :: Double
    accChildren = if totalChildren == 0 then 0 else fromIntegral correctChildren / fromIntegral totalChildren
    accAdults :: Double
    accAdults   = if totalAdults == 0 then 0 else fromIntegral correctAdults / fromIntegral totalAdults
    overallAcc :: Double
    overallAcc = fromIntegral (correctChildren + correctAdults) / fromIntegral (totalChildren + totalAdults)

  printf "Trefferquote Kinder:     %d / %d (%.2f%%)\n" correctChildren totalChildren (accChildren * 100)
  printf "Trefferquote Erwachsene: %d / %d (%.2f%%)\n" correctAdults totalAdults (accAdults * 100)
  printf "Gesamtgenauigkeit:     %.2f%%\n" (overallAcc * 100)
  return overallAcc

-- GEÄNDERT: Wrapper für das Training
findBestWeightsGradient :: [BookFeatures] -> [BookFeatures] -> CategoryStats -> IO Weights
findBestWeightsGradient children adults overallStats = do
  putStrLn "\nStarte Training mit Gradientenabstieg (symmetrische Normalisierung)..."
  let printProgress weights = printf "  -> Epoche... Aktuelle Gewichte: " >> print weights
  timeSth "Training abgeschlossen in" $
    trainWeightsWithGradientDescent children adults overallStats printProgress

-- Die Hauptfunktion, die alles steuert.
main :: IO ()
main = do
  putStrLn "Lade und verarbeite Kinderbücher..."
  children <- timeSth "Kinderbücher verarbeitet in" $ processDir "books/children"
  putStrLn "\nLade und verarbeite Erwachsenenbücher..."
  adults <- timeSth "Erwachsenenbücher verarbeitet in" $ processDir "books/adults"

  -- NEU: Berechne Statistiken für die einzelnen Kategorien UND für den gesamten Datensatz
  let childStats = calculateCategoryStats "children" children
      adultStats = calculateCategoryStats "adults" adults
      overallStats = calculateCategoryStats "overall" (children ++ adults)

  -- Finde die besten Gewichte mit unserer neuen, fairen Methode
  bestWeights <- findBestWeightsGradient children adults overallStats

  putStrLn "\nBeste gefundene Gewichte:"
  print bestWeights

  putStrLn "\nBewerte Genauigkeit mit den finalen Gewichten:"
  _ <- evaluateAccuracy children adults bestWeights overallStats childStats adultStats

  -- Gib die finalen Statistiken aus
  printCategoryStats childStats
  printCategoryStats adultStats
  printCategoryStats overallStats