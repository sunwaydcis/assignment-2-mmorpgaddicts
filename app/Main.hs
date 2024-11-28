module Main where

import Data.List.Split (splitOn)
import Data.List (groupBy, sortOn, maximumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import System.IO (hSetEncoding, stdout, utf8)
import Text.Printf (printf)

-- Define a data type for hospital records (used for Question 2)
data HospitalRecord = HospitalRecord
  { totalBeds   :: Int
  , covidBeds   :: Int
  } deriving (Show)

-- Main function to execute the program
main :: IO ()
main = do
    -- Ensure UTF-8 encoding
    hSetEncoding stdout utf8

    -- Fetch data from the hospital CSV file
    input <- fetchData
    
    -- Process data for Question 3
    let (averages, stateWithMostBeds) = process input
    
    -- Process data for Question 2
    let hospitalRecords = parseHospitalRecords input
        covidBedRatio = calculateRatio hospitalRecords

    -- Print results
    printOutput averages stateWithMostBeds
    putStrLn "\n~~~~~~~~~"
    putStrLn "Ratio of COVID Beds to Total Beds"
    putStrLn "~~~~~~~~~"
    putStrLn $ "Ratio: " ++ show covidBedRatio

-- Fetches the data from the hospital CSV file
fetchData :: IO String
fetchData = readFile "app/hospital.csv"

-- Process for Question 3: Average admissions and state with the most beds
process :: String -> ([(String, (Double, Double, Double))], (String, Double))
process input =
    let 
        formattedData = formatData input
        groupedByState = groupDataByState formattedData
        averagesByState = map calculateAverages groupedByState
        stateWithMostBeds = findStateWithMostBeds formattedData
    in 
        (averagesByState, stateWithMostBeds)

-- Parse hospital records for Question 2: COVID bed ratio calculation
parseHospitalRecords :: String -> [HospitalRecord]
parseHospitalRecords input =
    let 
        rows = lines input
        rawData = drop 1 rows
        splitRows = map (splitOn ",") rawData
    in 
        mapMaybe extractHospitalRecord splitRows

-- Extract hospital records for Question 2
extractHospitalRecord :: [String] -> Maybe HospitalRecord
extractHospitalRecord row =
    case row of
        (_:_:total:covid:_) ->
            case (readMaybe total, readMaybe covid) of
                (Just totalBeds, Just covidBeds) -> Just $ HospitalRecord totalBeds covidBeds
                _ -> Nothing
        _ -> Nothing

-- Calculate the ratio of COVID beds to total beds (Question 2)
calculateRatio :: [HospitalRecord] -> Double
calculateRatio records =
    let totalBedsSum = sum $ map totalBeds records
        covidBedsSum = sum $ map covidBeds records
    in if totalBedsSum == 0 
       then 0 
       else fromIntegral covidBedsSum / fromIntegral totalBedsSum

-- Formats raw CSV data (used for Question 3)
formatData :: String -> [(String, (Double, Double, Double, Double))]
formatData raw =
    let 
        rows = lines raw
        rawData = drop 1 rows
        splitRows = map (splitOn ",") rawData
    in 
        mapMaybe extractStateAndCategories splitRows

-- Extract state and admission categories for Question 3
extractStateAndCategories :: [String] -> Maybe (String, (Double, Double, Double, Double))
extractStateAndCategories row =
    case row of
        (_:state:beds:_:_:admitted_pui:admitted_covid:admitted_total:_) ->
            case (readMaybe beds, readMaybe admitted_pui, readMaybe admitted_covid, readMaybe admitted_total) of
                (Just beds', Just pui', Just covid', Just total') ->
                    Just (state, (beds', pui', covid', total'))
                _ -> Nothing
        _ -> Nothing

-- Group data by state (used for Question 3)
groupDataByState :: [(String, (Double, Double, Double, Double))] -> [[(String, (Double, Double, Double, Double))]]
groupDataByState = groupBy (\(s1, _) (s2, _) -> s1 == s2) . sortOn fst

-- Calculate averages for each state (Question 3)
calculateAverages :: [(String, (Double, Double, Double, Double))] -> (String, (Double, Double, Double))
calculateAverages rows =
    let 
        state = fst (head rows)
        (_, puiSum, covidSum, totalSum) = foldr (\(_, (_, pui, covid, total)) (b, p, c, t) -> (b, p + pui, c + covid, t + total)) (0, 0, 0, 0) rows
        count = fromIntegral (length rows)
    in 
        (state, (puiSum / count, covidSum / count, totalSum / count))

-- Find the state with the most beds (Question 1)
findStateWithMostBeds :: [(String, (Double, Double, Double, Double))] -> (String, Double)
findStateWithMostBeds rows =
    let bedData = map (\(state, (beds, _, _, _)) -> (state, beds)) rows
    in maximumBy (comparing snd) bedData

-- Display output
printOutput :: [(String, (Double, Double, Double))] -> (String, Double) -> IO ()
printOutput averages (stateWithMostBeds, mostBeds) = do
    putStrLn "\n~~~~~~~~~"
    putStrLn "State-wise Average Admissions"
    putStrLn "~~~~~~~~~"
    mapM_ printStateAverages averages
    putStrLn "~~~~~~~~~"
    putStrLn "State with the Most Beds"
    putStrLn "~~~~~~~~~"
    putStrLn $ "State: " ++ stateWithMostBeds ++ "\nTotal Beds: " ++ printf "%.2f" mostBeds
    putStrLn "~~~~~~~~~"

-- Print averages for a single state
printStateAverages :: (String, (Double, Double, Double)) -> IO ()
printStateAverages (state, (avgPui, avgCovid, avgTotal)) = do
    putStrLn $ "State: " ++ state
    putStrLn $ "  Average Suspected Admissions: " ++ printf "%.2f" avgPui
    putStrLn $ "  Average COVID-19 Admissions: " ++ printf "%.2f" avgCovid
    putStrLn $ "  Average Non-COVID Admissions: " ++ printf "%.2f" avgTotal
