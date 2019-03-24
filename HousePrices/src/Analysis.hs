module Analysis where

import Data.List (nub, sort, sortBy)

import Types
import Utils

sortByValue :: [(a, Double)] -> [(a, Double)]
sortByValue = sortBy (\(a, b) (c, d) -> compare b d)

allPearsonR2 :: [House] -> [(String, Double)]
allPearsonR2 houses = sortByValue $ map (\(a, b) -> (a, pearsonR2 houses salePrice b)) features

influencers :: [House] -> [(String, Double)]
influencers house = sortByValue $ filter ((>=0.1) . snd) $ allPearsonR2 house

outliers :: [House] -> [String]
outliers houses = foldr (\(a, b, t) acc -> if any (\x -> t x > 3) houses then a:acc else acc) [] mappa
    where
        std f =  standardDeviation houses f
        mean f = average (map f houses)
        threshold f x = abs((f x - mean f) / standardDeviation houses f)
        mappa = map (\(a, b) -> (a, b, threshold b)) features

meanPricePerNeigh :: [House] -> [(Double, Double)]
meanPricePerNeigh houses = sortByValue $ map (\n -> (n, meanPrice n)) uniqNeigh
    where
        uniqNeigh = nub . sort $ map neighborhood houses
        meanPrice n = average $ map salePrice $ filter ((==n) . neighborhood) houses
