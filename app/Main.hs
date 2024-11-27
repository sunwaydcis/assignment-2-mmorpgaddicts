module Main where

import Text.CSV
import Data.Maybe (mapMaybe)
import System.IO (hSetEncoding, stdout, utf8)

-- Define a data type to hold the hospital bed information
data HospitalRecord = HospitalRecord
  { totalBeds   :: Int  -- Total number of beds
  , covidBeds   :: Int  -- Number of COVID-specific beds
  } deriving (Show)

-- Function to parse a row into a HospitalRecord
parseRecord :: [String] -> Maybe HospitalRecord
parseRecord row = 
  case row of
    -- Match columns by their headers (order matters)
    [_, _, total, covid, _, _, _, _, _, _, _, _, _, _] -> 
      case (readMaybe total, readMaybe covid) of
        (Just t, Just c) -> Just $ HospitalRecord { totalBeds = t, covidBeds = c }
        _ -> Nothing  -- Return Nothing if parsing fails
    _ -> Nothing  -- Return Nothing if row doesn't match the expected format

-- Function to calculate the ratio of COVID beds to total beds
calculateRatio :: [HospitalRecord] -> Double
calculateRatio records =
  let totalBedsSum = sum $ map totalBeds records  -- Sum of total beds
      covidBedsSum = sum $ map covidBeds records  -- Sum of COVID beds
  in if totalBedsSum == 0 
       then 0 
       else fromIntegral covidBedsSum / fromIntegral totalBedsSum

-- Safe read for Maybe Int
readMaybe :: String -> Maybe Int
readMaybe str = case reads str of
  [(val, "")] -> Just val  -- Successfully parsed an Int
  _ -> Nothing             -- Parsing failed

main :: IO ()
main = do
  -- Set the encoding to UTF-8 to handle any special characters
  hSetEncoding stdout utf8
  
  -- Load the CSV file
  let filePath = "app/hospital.csv"  -- Path to the uploaded file
  result <- parseCSVFromFile filePath
  
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right parsedCsv -> do
      -- Skip the header (assuming the first row is the header)
      let records = mapMaybe parseRecord (tail parsedCsv)

      -- Calculate the ratio
      let ratio = calculateRatio records

      -- Print only the ratio result
      putStrLn $ "Ratio of COVID beds to total beds: " ++ show ratio
