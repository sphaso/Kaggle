{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sort)

import Analysis
import LinearRegression
import Types
import Parser
import Utils

import qualified Data.ByteString.Lazy as BL

linearPrediction :: [House] -> [Double]
linearPrediction houses = map (useLinearModel model) houses
    where
        model = mkModel houses
--
--linearOutput :: [House] -> [House] -> [(Int, Double)]
--linearOutput training validation = zip (map Types.id validation) predictions
--    where
--        model = mkModel training
--        predictions = map (pricePrediction model) validation

test :: [House] -> [Double]
test houses = sort $ zipWith (\h p -> salePrice h - p) houses predictions
    where
       predictions = linearPrediction houses

main :: IO ()
main = do
    training <- parseFullHouse <$> BL.readFile "train.csv"
--  validation <- parseValidationHouse <$> BL.readFile "test.csv"
--  let res = BL.append "Id,SalePrice\r\n" (toCsv $ linearOutput training validation)
--  BL.writeFile "tx" res
--  print $ (*3) $ standardDeviation' $ (map salePrice train)
--  print $ outliers train
--  print $ influencers training
--  print $ test training
    print $ standardDeviation' $ test training
    print $ mkModel training
--  print $ meanPricePerNeigh training
