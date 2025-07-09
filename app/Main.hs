module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import System.CPUTime
import Text.Printf
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.List as L
import Control.Monad (forM, when)
import Control.Exception (evaluate)
import Data.Maybe (mapMaybe)

import Types
import CoreLogic
import Helpers

loadBooksFromDir :: FilePath -> Classification -> IO [(Text, Classification)]
loadBooksFromDir dir classification = do
  files <- listDirectory dir
  let txtFiles = L.filter (L.isSuffixOf ".txt") files
  forM txtFiles $ \file -> do
    content <- TIO.readFile (dir </> file)
    return (content, classification)

timeSth :: String -> IO a -> IO a
timeSth label action = do
  start <- getCPUTime
  result <- action >>= evaluate
  end <- getCPUTime
  let seconds = fromIntegral (end - start) / (10^12)
  printf "   -> %s: %.3f Sekunden\n" label (seconds :: Double)
  return result

-- NEU: Eine sichere Funktion zur Berechnung der Genauigkeit, um Division durch Null zu vermeiden.
safeAccuracy :: Int -> Int -> Double
safeAccuracy correct total
  | total == 0 = 0.0
  | otherwise  = 100 * fromIntegral correct / fromIntegral total

main :: IO ()
main = do
  let learningRate = 0.1
      lambda = 0.01
      epochs = 4000

  putStrLn "Buchklassifizierungs-Modell v2"
  putStrLn "================================"

  putStrLn "1. Lade Trainings-Bücher..."
  let childrenTrainFP = "books/training/children"
      adultsTrainFP   = "books/training/adults"
      -- childrenTestFP  = "books/training/children"
      -- adultsTestFP   = "books/training/adults"
      childrenTestFP  = "books/categorize/children"
      adultsTestFP    = "books/categorize/adults"

  childrenTrainBooks <- loadBooksFromDir childrenTrainFP Children
  adultsTrainBooks   <- loadBooksFromDir adultsTrainFP Adult
  let trainingBooks = childrenTrainBooks ++ adultsTrainBooks

  when (null trainingBooks) $
    error "Keine Trainings-Bücher gefunden. Stelle sicher, dass 'books/training/*' .txt-Dateien enthält."
  printf "   -> %d Trainings-Bücher insgesamt geladen.\n" (length trainingBooks)

  putStrLn "2. Extrahiere Trainings-Features..."
  let labeledFeatures = mapMaybe (\(text, label) ->
          fmap (\features -> (features, label)) (extractFeatures text)
        ) trainingBooks

  when (null labeledFeatures) $
    error "Konnte keine Features aus den Trainings-Büchern extrahieren. Sind die Dateien leer?"

  putStrLn "3. Berechne globale Normalisierungsstatistiken..."

  globalStats <- case calculateGlobalStats $ map fst labeledFeatures of
    Just stats -> return stats
    Nothing    -> error "Konnte keine globalen Statistikdaten berechnen (wahrscheinlich leere Feature-Listen)."

  putStrLn "4. Initialisiere Trainingsdaten..."
  let trainingData =
        [ (normalizeFeatures features globalStats, if label == Adult then 1.0 else 0.0)
        | (features, label) <- labeledFeatures
        ]

  putStrLn $ "5. Trainiere Modell (" ++ show epochs ++ " Epochen)..."
  let initialWeights = Weights 0 0 0 0 0 0 0

  finalWeights <- timeSth "Trainingszeit" $
    return $ trainModel learningRate lambda epochs trainingData initialWeights

  putStrLn "6. Training abgeschlossen. Finale Gewichte:"
  printf "   -> wSentenceLength:    %+.4f\n" (wSentenceLength finalWeights)
  printf "   -> wWordLength:        %+.4f\n" (wWordLength finalWeights)
  printf "   -> wFlesch:            %+.4f\n" (wFlesch finalWeights)
  printf "   -> wUniqueWordRatio:   %+.4f\n" (wUniqueWordRatio finalWeights)
  printf "   -> wSentLengthStdDev:  %+.4f\n" (wSentLengthStdDev finalWeights)
  printf "   -> wCommasPerSentence: %+.4f\n" (wCommasPerSentence finalWeights)
  printf "   -> Bias:               %+.4f\n" (bias finalWeights)

  putStrLn "\n7. Lade Test-Bücher..."
  childrenTestBooks <- loadBooksFromDir childrenTestFP Children
  adultsTestBooks   <- loadBooksFromDir adultsTestFP Adult
  let testBooks = childrenTestBooks ++ adultsTestBooks

  when (null testBooks) $
    error "Keine Test-Bücher gefunden. Stelle sicher, dass 'books/categorize/*' .txt-Dateien enthält."
  printf "   -> %d Test-Bücher insgesamt geladen.\n" (length testBooks)

  putStrLn "8. Extrahiere Test-Features..."
  let testFeatures = mapMaybe (\(text, label) ->
          fmap (\features -> (features, label)) (extractFeatures text)
        ) testBooks

  putStrLn "9. Evaluiere Modellgenauigkeit auf dem TEST-Set..."
  -- KORREKTUR: Das `_ <-` wurde entfernt. Die Aktion wird einfach ausgeführt.
  timeSth "Evaluationszeit" $ do
    let childrenItems = L.filter ((== Children) . snd) testFeatures
        adultItems    = L.filter ((== Adult) . snd) testFeatures

        isCorrect (features, label) =
          classify finalWeights globalStats features == label

        correctChildren = length $ filter isCorrect childrenItems
        correctAdults   = length $ filter isCorrect adultItems
        totalCorrect    = correctChildren + correctAdults

        totalChildren = length childrenItems
        totalAdults   = length adultItems
        totalItems    = totalChildren + totalAdults

    printf "   -> Genauigkeit Kinderbücher: %.2f%% (%d von %d)\n"
      (safeAccuracy correctChildren totalChildren) correctChildren totalChildren

    printf "   -> Genauigkeit Erwachsenenbücher: %.2f%% (%d von %d)\n"
      (safeAccuracy correctAdults totalAdults) correctAdults totalAdults

    printf "   -> Gesamtgenauigkeit: %.2f%% (%d von %d)\n"
      (safeAccuracy totalCorrect totalItems) totalCorrect totalItems

    return ()