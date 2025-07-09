module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import System.CPUTime
import Text.Printf
import Data.Text (Text)
import qualified Data.List as L
import Control.Monad (forM, when)
import Control.Exception (evaluate)
import Data.Maybe (mapMaybe)

import Types
import CoreLogic

timeSth :: String -> IO a -> IO a
timeSth label action = do
  start <- getCPUTime
  result <- action >>= evaluate
  end <- getCPUTime
  let seconds = (fromIntegral (end - start) :: Double) / 1e12
  printf "   -> %s: %.3f Sekunden\n" label seconds
  return result

loadBooksFromDir :: FilePath -> Classification -> IO [(Text, Classification)]
loadBooksFromDir dir label = do
  allFiles <- listDirectory dir
  let txtFiles = filter (L.isSuffixOf ".txt") allFiles
  forM txtFiles $ \file -> do
    content <- TIO.readFile (dir </> file)
    return (content, label)

safeAccuracy :: Int -> Int -> Double
safeAccuracy correct total
  | total == 0 = 0.0
  | otherwise  = 100 * fromIntegral correct / fromIntegral total

-- mapMaybe because potentially Nothing values
-- fmap extracts value from IO container (features,label)
extractLabeledFeatures :: [(Text, Classification)] -> [(BookFeatures, Classification)]
extractLabeledFeatures =
  mapMaybe (\(text, label) -> fmap (\features -> (features, label)) (extractFeatures text))

-- evaluate MODEL accuracy
evaluateModel :: Weights -> CategoryStats -> [(BookFeatures, Classification)] -> IO ()
evaluateModel weights stats testSet = do
  let isCorrect (features, label) = classify weights stats features == label
      groupByLabel label = filter ((== label) . snd) testSet

      evaluateCategory :: Classification -> IO Int
      evaluateCategory label = do
        let items = groupByLabel label
            correct = length $ filter isCorrect items
            total = length items
        printf "   -> Genauigkeit %s: %.2f%% (%d von %d)\n"
          (show label) (safeAccuracy correct total) correct total
        return correct

  correctChildren <- evaluateCategory Children
  correctAdults   <- evaluateCategory Adult
  let total = length testSet
      correctTotal = correctChildren + correctAdults

  printf "   -> Gesamtgenauigkeit: %.2f%% (%d von %d)\n"
    (safeAccuracy correctTotal total) correctTotal total

main :: IO ()
main = do
  let learningRate     = 0.1
      lambda           = 0.01
      epochs           = 800
      initialWeights   = Weights 0 0 0 0 0 0 0

      childrenTrainFP  = "books/training/children"
      adultsTrainFP    = "books/training/adults"
      childrenTestFP   = "books/categorize/children"
      adultsTestFP     = "books/categorize/adults"

  putStrLn "================================"
  putStrLn "Buchklassifizierungs-Modell"
  putStrLn "================================"

  putStrLn "1. Lade Trainings-Bücher..."

  childrenTrainBooks <- loadBooksFromDir childrenTrainFP Children
  adultsTrainBooks   <- loadBooksFromDir adultsTrainFP Adult
  let trainingBooks = childrenTrainBooks ++ adultsTrainBooks

  when (null trainingBooks) $
    error "Keine Trainings-Bücher gefunden."

  printf "   -> %d Trainings-Bücher geladen.\n" (length trainingBooks)

  putStrLn "2. Extrahiere Trainings-Features..."
  
  let labeledFeatures = extractLabeledFeatures trainingBooks
  when (null labeledFeatures) $
    error "Keine Features extrahiert – prüfen ob Bücher leer sind."

  putStrLn "3. Berechne globale Normalisierungsstatistiken..."
  globalStats <- maybe (error "Leere Feature-Listen.") return $
    calculateGlobalStats (map fst labeledFeatures)

  putStrLn "4. Initialisiere Trainingsdaten..."
  let trainingData =
        [ (normalizeFeatures features globalStats, if label == Adult then 1.0 else 0.0)
        | (features, label) <- labeledFeatures
        ]

  putStrLn $ "5. Trainiere Modell über " ++ show epochs ++ " Epochen..."
  finalWeights <- timeSth "Trainingszeit" $
    return $ trainModel learningRate lambda epochs trainingData initialWeights

  putStrLn "6. Finale Gewichte:"
  printf "   -> wSentenceLength:    %+.4f\n" (wSentenceLength finalWeights)
  printf "   -> wWordLength:        %+.4f\n" (wWordLength finalWeights)
  printf "   -> wFlesch:            %+.4f\n" (wFlesch finalWeights)
  printf "   -> wUniqueWordRatio:   %+.4f\n" (wUniqueWordRatio finalWeights)
  printf "   -> wSentLengthStdDev:  %+.4f\n" (wSentLengthStdDev finalWeights)
  printf "   -> wCommasPerSentence: %+.4f\n" (wCommasPerSentence finalWeights)
  printf "   -> Bias:               %+.4f\n" (bias finalWeights)

  putStrLn "\n7. Lade Test-Bücher..."
  childrenTestBooks <- loadBooksFromDir childrenTestFP Children
  adultTestBooks <- loadBooksFromDir adultsTestFP Adult
  let testBooks = childrenTestBooks ++ adultTestBooks 

  when (null testBooks) $
    error "Keine Test-Bücher gefunden."
  printf "   -> %d Test-Bücher geladen.\n" (length testBooks)

  putStrLn "8. Extrahiere Test-Features..."
  let testFeatures = extractLabeledFeatures testBooks

  putStrLn "9. Evaluiere Modell auf TEST-Set..."
  timeSth "Evaluationszeit" $
    evaluateModel finalWeights globalStats testFeatures
