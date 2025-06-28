module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import Control.Monad (forM)

import Types
import CoreLogic

-- IO monad function which takes dir path and recursively calls FeatureExtractor for every book
-- returns IO wrapper of BookFeatues (IO [BookFeatueres])
processDir :: FilePath -> IO [BookFeatures]
processDir dir = do 
    -- read all file names 
    files <- listDirectory (dir)
    
    -- "monadic" for (like map)
    forM files $ \file -> do 
        let fullPath = dir </> file
        putStrLn $ "Verarbeite: " ++ fullPath
        content <- TIO.readFile fullPath

        -- calling pure function
        return $ extractFeaturesFromText fullPath content



main :: IO ()
main = do
    -- read in all children/ books -> IO 
    childrenBooks <- processDir ("books/children")
    -- read in all adult/ books -> IO
    adultBooks <- processDir ("books/adults")
    -- calculate thresholds by calculating avg of childrens and adults first
    let avgChildren = calculateCategoryFeatures childrenBooks
    print avgChildren
    -- read in test/ books and categorize them 

    putStrLn ("Completed")

