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

  main :: IO ()
  main = do
    let learningRate = 0.1
    let lambda = 0.01
    let epochs = 1000
    let testSplitRatio = 0.2

    printf "Buchklassifizierungs-Modell v2\n"
    printf "================================\n"
    
    printf "1. Lade Bücher aus den Verzeichnissen...\n"
    childrenBooks <- loadBooksFromDir "books/children" Children
    adultBooks <- loadBooksFromDir "books/adults" Adult
    let allBooks = childrenBooks ++ adultBooks
    when (null allBooks) $ error "Keine Bücher gefunden. Stelle sicher, dass die Ordner books/children und books/adults existieren und .txt-Dateien enthalten."
    printf "   -> %d Bücher insgesamt geladen.\n" (length allBooks)

    printf "2. Extrahiere Features aus jedem Buch...\n"
    let labeledFeatures = [(extractFeatures text, label) | (text, label) <- allBooks]
    
    printf "3. Berechne globale Statistiken für die symmetrische Normalisierung...\n"
    let allFeatures = map fst labeledFeatures
    let globalStats = calculateGlobalStats allFeatures
    
    printf "4. SKIPPING: Aufteilen der Daten in Trainings- und Test-Set...\n"
    -- let splitIndex = floor $ fromIntegral (length labeledFeatures) * (1.0)
    -- let (trainItems, testItems) = L.splitAt splitIndex labeledFeatures
    let trainItems = labeledFeatures

    let trainingData =
          [ (normalizeFeatures features globalStats, if label == Adult then 1.0 else 0.0)
          | (features, label) <- trainItems
          ]

    printf "5. Trainiere das Modell mit %d Epochen (Trainingsdaten: %d)...\n" epochs (length trainingData) 
    let initialWeights = Weights 0 0 0 0 0 0 0
    let finalWeights = trainModel learningRate lambda epochs trainingData initialWeights

    printf "6. Training abgeschlossen. Finale Gewichte:\n"
    printf "   -> wSentenceLength:    %+.4f\n" (wSentenceLength finalWeights)
    printf "   -> wWordLength:        %+.4f\n" (wWordLength finalWeights)
    printf "   -> wFlesch:            %+.4f\n" (wFlesch finalWeights)
    printf "   -> wUniqueWordRatio:   %+.4f\n" (wUniqueWordRatio finalWeights)
    printf "   -> wSentLengthStdDev:  %+.4f\n" (wSentLengthStdDev finalWeights)
    printf "   -> wCommasPerSentence: %+.4f\n" (wCommasPerSentence finalWeights)
    printf "   -> Bias:               %+.4f\n" (bias finalWeights)

    printf "7. Evaluiere Modellgenauigkeit auf dem TRAININGS-Set (nicht empfohlen für echte Evaluation)...\n"
    let correctPredictions =
          length $
          L.filter
            (\(features, actualLabel) ->
              let predictedLabel = classify finalWeights globalStats features
              in predictedLabel == actualLabel)
            trainItems

    let accuracy = safeDiv (fromIntegral correctPredictions * 100) (length trainItems)

    printf "   -> Genauigkeit: %.2f%%\n" accuracy
    printf "================================\n"