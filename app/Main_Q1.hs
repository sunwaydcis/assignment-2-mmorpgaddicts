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
    let averagesByState = map calculateAverages groupedByState
    putStrLn "Average Admissions by State:"
    mapM_ print averagesByState

-- Fetches the data from the hospital CSV file
fetchData :: IO String
fetchData = readFile "data/hospital.csv"

-- Formats raw CSV data
formatData :: String -> [(String, (Double, Double, Double))]
formatData raw =
    let 
        rows = lines raw           -- Splits into rows
        rawData = drop 1 rows      -- Drops the header row
        splitRows = map (splitOn ",") rawData -- Splits each row by commas
        extracted = mapMaybe extractStateAndCategories splitRows
    in 
        extracted

-- Extracts state and category data
extractStateAndCategories :: [String] -> Maybe (String, (Double, Double, Double))
extractStateAndCategories row =
    case row of
        (_:state:_:_:_:admitted_pui:admitted_covid:admitted_total:_) ->
            case (readMaybe admitted_pui, readMaybe admitted_covid, readMaybe admitted_total) of
                (Just pui, Just covid, Just total) -> Just (state, (pui, covid, total))
                _ -> Nothing
        _ -> Nothing

-- Groups data by state
groupDataByState :: [(String, (Double, Double, Double))] -> [[(String, (Double, Double, Double))]]
groupDataByState = groupBy (\(s1, _) (s2, _) -> s1 == s2) . sortOn fst

-- Calculates averages for each state
calculateAverages :: [(String, (Double, Double, Double))] -> (String, (Double, Double, Double))
calculateAverages rows =
    let state = fst (head rows)
        (puiSum, covidSum, totalSum) = foldr (\(_, (pui, covid, total)) (p, c, t) -> (p + pui, c + covid, t + total)) (0, 0, 0) rows
        count = fromIntegral (length rows)
    in (state, (puiSum / count, covidSum / count, totalSum / count))
