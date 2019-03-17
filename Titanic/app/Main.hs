{-# LANGUAGE OverloadedStrings #-}

module Main where

import DecisionTree
import NaiveBayes
import Parser
import Types
import Utils

import qualified Data.ByteString.Lazy as BL
import System.Random
import System.Random.Shuffle

createSets :: [Passenger] -> ([Passenger], [Passenger])
createSets ps = (take ntest speople, drop ntest speople)
    where
        genny = mkStdGen 1337
        ntest = 712 -- 80% of the train dataset
        speople = shuffle' ps 891 genny

bayesOutput :: [Passenger] -> [Passenger] -> [(Int, Int)]
bayesOutput train validation = map (\(a, b) -> (a, if b then 1 else 0)) survival
    where
        positiveModel = mkBayesModel train True
        negativeModel = mkBayesModel train False
        survival = map (\p -> (pid p, willSurvive positiveModel negativeModel p)) validation

decisionTreeOutput :: [Passenger] -> [Passenger] -> [(Int, Int)]
decisionTreeOutput train validation = zip (map pid validation) (map (survivalDecision model . passengerToNPassenger) validation)
    where
        model = mkDecisionTree $ (map passengerToNPassenger train)

main :: IO ()
main = do
    train <- (map cleanAgeData) <$> parse <$> BL.readFile "train.csv"
    evaluate <- (map cleanAgeData) <$> parse <$> BL.readFile "test.csv"
--    let res = BL.append "PassengerId,Survived\r\n" (toCsv $ bayesOutput train evaluate)
    let res = BL.append "PassengerId,Survived\r\n" (toCsv $ decisionTreeOutput train evaluate)
    BL.writeFile "tx" res
