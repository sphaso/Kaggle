{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Binary
import qualified Data.ByteString.Lazy as BL

import NeuralNetwork
import Parser

test :: [(Double, [Double])] -> IO (Int, Int)
test input = do
    model <- mkModel input
    let
        actual = map (round . fst) input
        guessed = map (query model . snd) input
        correctlyGuessed = length $ filter id $ zipWith (==) actual guessed
    pure $ (correctlyGuessed, length actual)

verify :: Model -> [[Double]] -> [(Int, Int)]
verify model validation =
        let
            guess = map (query model) validation
        in
            zip [1..] guess

main :: IO ()
main = do
--  training <- parse <$> BL.readFile "train.csv"
--  model <- mkModel training
--  encodeFile "brain.dat" model
    validation <- parse' <$> BL.readFile "test.csv"
    model <- decodeFile "brain.dat"
    let result = verify model validation
    let output = BL.append "ImageId,Label\r\n" (toCsv result)
    BL.writeFile "tx" output
