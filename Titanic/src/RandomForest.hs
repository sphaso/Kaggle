module RandomForest where

import Data.List (tails)

import DecisionTree

-- https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

featuresRF :: [String]
featuresRF = [
               "n_pclass"
             , "n_title"
             , "n_sex"
--           , "n_age"
--           , "n_sibsp"
--           , "n_parch"
--           , "n_family"
             , "n_embarked"
             , "n_fare"
             ]

mkForest :: [NPassenger] -> [DTree]
mkForest px = map (flip mkDecisionTree px) (combinations 3 featuresRF)

survivalForest :: [DTree] -> NPassenger -> Int
survivalForest trees p
    | ok >= ko = 1
    | otherwise = 0
  where
      outputs = map (flip survivalDecision p) trees
      (ok, ko) = foldr (\i (a, b) -> if i == 1 then (a + 1, b) else (a, b + 1)) (0, 0) outputs
