module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import Control.Monad (forM)
import Text.Printf (printf)
import System.CPUTime
import Text.Printf

import Types
import CoreLogic

-- timeSth :: String -> IO a -> IO a 
-- timeSth label action = do
--     start <- getCPUTime
--     result <- action
--     end <- getCPUTime
--     let diff = fromIntegral (end - start) / (10^12)
--     printf "%s: %.3f sec\n" label (diff :: Double)
--     return result

printFeatures :: BookFeatures -> IO ()
printFeatures (BookFeatures fp slen commas uniq wl) = do
  putStrLn $ "File: " ++ fp
  printf "  avgSentenceLength:     %.2f\n" slen
  printf "  avgCommasPerSentence:  %.2f\n" commas
  printf "  uniqueWordRatio:       %.3f\n" uniq
  printf "  avgWordLength:         %.2f\n" wl

-- IO monad function which takes dir path and recursively calls FeatureExtractor for every book
-- returns IO wrapper of BookFeatues (IO [BookFeatueres])
processDir :: FilePath -> IO [BookFeatures]
processDir dir = do 
    -- read all file names 
    files <- listDirectory (dir)
    
    -- "monadic" for (like map)
    forM files $ \file -> do 
        let fullPath = dir </> file
        -- putStrLn $ "Verarbeite: " ++ fullPath
        content <- TIO.readFile fullPath

        -- calling pure function
        return $ extractFeaturesFromText fullPath content



main :: IO ()
main = do
    let childrenFP = "books/children"
    let adultsFP = "books/adults"
    -- read in all children/ books -> IO 
    childrenFeatures <- processDir (childrenFP)
    -- read in all adult/ books -> IO
    adultFeatures <- processDir (adultsFP)
    -- calculate thresholds by calculating avg of childrens and adults first
    let avgChildren = calculateCategoryFeatures childrenFeatures childrenFP
    printFeatures avgChildren

    let avgAdults = calculateCategoryFeatures adultFeatures adultsFP
    printFeatures avgAdults

    let thresholds = calculateThresholds childrenFeatures adultFeatures
    print thresholds
    -- read in test/ books and categorize them 

    putStrLn ("Completed")

