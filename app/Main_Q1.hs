module Main where

import Data.List.Split (splitOn)
import Data.List (groupBy, sortOn, maximumBy)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Data.Ord (comparing)

-- Main function to execute the program
-- @NOTES We have to follow the proper structure (Input, Process, Output)
main :: IO ()
main = fetchData >>= \input -> do
    let (averages, stateWithMostBeds) = process input
    printOutput averages stateWithMostBeds

-- Read the data from the hospital CSV file and print it
fetchData :: IO String
fetchData = do
    content <- readFile "data/hospital.csv"
    putStrLn "Raw data from hospital.csv:"
    putStrLn content
    return content

