module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Define a data type to hold the hospital bed information
data HospitalRecord = HospitalRecord
  { totalBeds   :: Int
  , covidBeds   :: Int
  } deriving (Show)

-- Instance to parse CSV into HospitalRecord
instance FromNamedRecord HospitalRecord where
  parseNamedRecord r = HospitalRecord 
    <$> r .: "beds" 
    <*> r .: "beds_covid"

main :: IO ()
main = do
  -- Load the CSV file (converted from Excel to CSV before running this)
  csvData <- BL.readFile "hospital.csv"

  case decodeByName csvData of
    Left err -> putStrLn $ "Error: " ++ err
    Right (_, records) -> do
      let hospitalRecords = V.toList records

          -- Sum total beds and COVID-19 beds
          totalBedsSum = sum $ map totalBeds hospitalRecords
          covidBedsSum = sum $ map covidBeds hospitalRecords

          -- Compute the ratio
          ratio = fromIntegral covidBedsSum / fromIntegral totalBedsSum :: Double

      putStrLn $ "Ratio of beds dedicated to COVID-19: " ++ show ratio
