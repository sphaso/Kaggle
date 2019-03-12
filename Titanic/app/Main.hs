{-# LANGUAGE OverloadedStrings #-}

module Main where

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

bayes :: [Passenger] -> [Passenger] -> [(Maybe Int, Bool)]
bayes test validation = zip (map survived validation) survival
    where
        positiveModel = mkBayesModel test True
        negativeModel = mkBayesModel test False
        survival = map (willSurvive positiveModel negativeModel) validation

bayesOutput :: [Passenger] -> [Passenger] -> [(Int, Int)]
bayesOutput train validation = map (\(a, b) -> (a, if b then 1 else 0)) survival
    where
        positiveModel = mkBayesModel train True
        negativeModel = mkBayesModel train False
        survival = map (\p -> (pid p, willSurvive positiveModel negativeModel p)) validation

main :: IO ()
main = do
    train <- parse <$> BL.readFile "train.csv"
    evaluate <- parse <$> BL.readFile "test.csv"
    let res = BL.append "PassengerId,Survived\r\n" (toCsv $ bayesOutput train evaluate)
    BL.writeFile "tx" res
