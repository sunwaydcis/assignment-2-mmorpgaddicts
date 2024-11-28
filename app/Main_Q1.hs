module Main where

import Data.List.Split (splitOn)
import Data.List (groupBy, sortOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
    input <- fetchData
    let formattedData = formatData input
    let groupedByState = groupDataByState formattedData
    let bedsByState = map calculateTotalBeds groupedByState
    putStrLn "Total Beds by State:"
    mapM_ print bedsByState

-- Fetches the data from the hospital CSV file
fetchData :: IO String
fetchData = readFile "data/hospital.csv"

-- Formats raw CSV data
formatData :: String -> [(String, Double)]
formatData raw =
    let 
        rows = lines raw           -- Splits into rows
        rawData = drop 1 rows      -- Drops the header row
        splitRows = map (splitOn ",") rawData -- Splits each row by commas
        extracted = mapMaybe extractStateAndBeds splitRows
    in 
        extracted

-- Extracts state and bed counts
extractStateAndBeds :: [String] -> Maybe (String, Double)
extractStateAndBeds row =
    case row of
        (_:state:beds:_) -> case readMaybe beds of
                              Just beds' -> Just (state, beds')
                              _          -> Nothing
        _ -> Nothing

-- Groups data by state
groupDataByState :: [(String, Double)] -> [[(String, Double)]]
groupDataByState = groupBy (\(s1, _) (s2, _) -> s1 == s2) . sortOn fst

-- Calculates total beds for each state
calculateTotalBeds :: [(String, Double)] -> (String, Double)
calculateTotalBeds rows =
    let state = fst (head rows)
        totalBeds = sum (map snd rows)
    in (state, totalBeds)
