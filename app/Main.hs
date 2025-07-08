module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import System.CPUTime
import Text.Printf
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.List as L
import Control.Monad (forM, when)
import Control.Exception (evaluate)

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

main :: IO ()
main = do
  let learningRate = 0.1
      lambda = 0.01
      epochs = 1000

  putStrLn "Buchklassifizierungs-Modell v2"
  putStrLn "================================"

  putStrLn "1. Lade Bücher aus den Verzeichnissen..."
  childrenBooks <- loadBooksFromDir "books/children" Children
  adultBooks    <- loadBooksFromDir "books/adults" Adult
  let allBooks = childrenBooks ++ adultBooks
  when (null allBooks) $
    error "Keine Bücher gefunden. Stelle sicher, dass 'books/children' und 'books/adults' .txt-Dateien enthalten."
  printf "   -> %d Bücher insgesamt geladen.\n" (length allBooks)

  putStrLn "2. Extrahiere Features..."
  let labeledFeatures = [(extractFeatures text, label) | (text, label) <- allBooks]

  putStrLn "3. Berechne globale Normalisierungsstatistiken..."
  let globalStats = calculateGlobalStats $ map fst labeledFeatures

  putStrLn "4. Initialisiere Trainingsdaten (Test-Split übersprungen)..."
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

  putStrLn "7. Evaluiere Modellgenauigkeit auf dem TRAININGS-Set..."
  _ <- timeSth "Evaluationszeit" $ do
    let childrenItems = L.filter ((== Children) . snd) labeledFeatures
        adultItems    = L.filter ((== Adult) . snd) labeledFeatures

        isCorrect (features, label) =
          classify finalWeights globalStats features == label

        correctChildren = length $ filter isCorrect childrenItems
        correctAdults   = length $ filter isCorrect adultItems
        totalCorrect    = correctChildren + correctAdults

        totalChildren = length childrenItems
        totalAdults   = length adultItems
        totalItems    = totalChildren + totalAdults

    printf "   -> Genauigkeit Kinderbücher: %.2f%% (%d von %d)\n"
      (100 * fromIntegral correctChildren / fromIntegral totalChildren :: Double)
      correctChildren totalChildren

    printf "   -> Genauigkeit Erwachsenenbücher: %.2f%% (%d von %d)\n"
      (100 * fromIntegral correctAdults / fromIntegral totalAdults :: Double)
      correctAdults totalAdults

    printf "   -> Gesamtgenauigkeit: %.2f%% (%d von %d)\n"
      (100 * fromIntegral totalCorrect / fromIntegral totalItems :: Double)
      totalCorrect totalItems

    return ()

  putStrLn "Evaluierung abgeschlossen."
