module Main where

import Data.List.Split (splitOn)
import Data.List (groupBy, sortOn, maximumBy)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Data.Ord (comparing)

-- Main function to execute the program
main :: IO ()
main = fetchData >>= \input -> do
    let (averages, stateWithMostBeds) = process input
    printOutput averages stateWithMostBeds

-- Fetches the data from the hospital CSV file. We will fetch this from the hospital.csv file.
fetchData :: IO String
fetchData = readFile "data/hospital.csv"

-- This processes the input data and seperates it into different variables
process :: String -> ([(String, (Double, Double, Double))], (String, Double))
process input =
    let 
        formattedData = formatData input -- raw CSV into a list of tuples
        groupedByState = groupDataByState formattedData -- groups the data by state
        averagesByState = map calculateAverages groupedByState -- calculates the averages for each state
        stateWithMostBeds = findStateWithMostBeds formattedData -- finds the state with the most beds
    in 
        (averagesByState, stateWithMostBeds) -- returns the averages and the state with the most beds


-- Extracts state, total beds, and admission counts for each category, and returns a tuple of the state and the beds, pui, covid, and total. 
-- If it is not possible to read the values as doubles, it returns Nothing.
-- @NOTES Can we check if there is a better way to do this?
extractStateAndCategories :: [String] -> Maybe (String, (Double, Double, Double, Double))
extractStateAndCategories row =
    case row of
        (_:state:beds:_:_:admitted_pui:admitted_covid:admitted_total:_) ->
            case (readMaybe beds, readMaybe admitted_pui, readMaybe admitted_covid, readMaybe admitted_total) of
                (Just beds', Just pui', Just covid', Just total') ->
                    Just (state, (beds', pui', covid', total'))
                _ -> Nothing
        _ -> Nothing

-- Formats raw CSV data
-- @NOTES We are basically splitting them into rows and removing the header row. We then also split each row by commas.
formatData :: String -> [(String, (Double, Double, Double, Double))]
formatData raw =
    let 
        rows = lines raw           -- Splits into rows
        rawData = drop 1 rows      -- Drops the header row
        splitRows = map (splitOn ",") rawData -- Splits each row by commas
    in 
        mapMaybe extractStateAndCategories splitRows -- maps the extracted state and categories to a list of tuples

-- Converts a tuple with Maybe values to a valid tuple if possible
toStateAndCategories :: (String, (Maybe Double, Maybe Double, Maybe Double, Maybe Double)) 
                     -> Maybe (String, (Double, Double, Double, Double))
toStateAndCategories (state, (Just beds, Just pui, Just covid, Just total)) = Just (state, (beds, pui, covid, total))
toStateAndCategories _                                                     = Nothing

-- Groups data by state
groupDataByState :: [(String, (Double, Double, Double, Double))] -> [[(String, (Double, Double, Double, Double))]]
groupDataByState = groupBy (\(s1, _) (s2, _) -> s1 == s2) . sortOn fst

-- Calculates averages for each state
calculateAverages :: [(String, (Double, Double, Double, Double))] -> (String, (Double, Double, Double))
calculateAverages rows =
    let 
        state = fst (head rows)
        (_, puiSum, covidSum, totalSum) = foldr (\(_, (_, pui, covid, total)) (b, p, c, t) -> (b, p + pui, c + covid, t + total)) (0, 0, 0, 0) rows
        count = fromIntegral (length rows)
    in 
        (state, (puiSum / count, covidSum / count, totalSum / count))

-- Finds the state with the most beds
findStateWithMostBeds :: [(String, (Double, Double, Double, Double))] -> (String, Double)
findStateWithMostBeds rows =
    let bedData = map (\(state, (beds, _, _, _)) -> (state, beds)) rows
    in maximumBy (comparing snd) bedData

-- Displays the processed output
printOutput :: [(String, (Double, Double, Double))] -> (String, Double) -> IO ()
printOutput averages (stateWithMostBeds, mostBeds) = do
    putStrLn "\n~~~~~~~~~"
    putStrLn "State-wise Average Admissions"
    putStrLn "~~~~~~~~~"
    mapM_ printStateAverages averages
    putStrLn "~~~~~~~~~"
    putStrLn "State with the Most Beds"
    putStrLn "~~~~~~~~~"
    putStrLn $ "State: " ++ stateWithMostBeds ++ "\nTotal Beds: " ++ show mostBeds
    putStrLn "~~~~~~~~~"

-- Prints averages for a single state
-- @NOTES We print this on a separate function to make it easier for us to print instead of having to write the same thing multiple times. xdd
printStateAverages :: (String, (Double, Double, Double)) -> IO ()
printStateAverages (state, (avgPui, avgCovid, avgTotal)) = do
    putStrLn $ "State: " ++ state
    putStrLn $ "  Average Suspected Admissions: " ++ show avgPui
    putStrLn $ "  Average COVID-19 Admissions: " ++ show avgCovid
    putStrLn $ "  Average Non-COVID Admissions: " ++ show avgTotal

-- PLEASE ADD IN QUESTION 2 AND INTEGRATE IT INTO THE CODE.
-- Kah Shun
