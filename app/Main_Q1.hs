module Main where

import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- fetchData
    let formattedData = formatData input
    putStrLn "Formatted Data (State, Beds, PUI, COVID, Total):"
    mapM_ print formattedData

-- Fetches the data from the hospital CSV file
fetchData :: IO String
fetchData = readFile "data/hospital.csv"

-- Formats raw CSV data
formatData :: String -> [[String]]
formatData raw =
    let 
        rows = lines raw           -- Splits into rows
        rawData = drop 1 rows      -- Drops the header row
        splitRows = map (splitOn ",") rawData -- Splits each row by commas
    in 
        splitRows
