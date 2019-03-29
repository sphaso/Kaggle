{-# LANGUAGE TupleSections #-}

module LinearRegression where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix

import Types
import Utils

predictors :: [(House -> Double)]
predictors = [
               msSubClass
             , lotArea
             , bsmtUnfSF
             , firstFloorSF
             , secondFloorSF
             , garageCars
             , woodDeckSF
             , price_per_neighborhood
             , exterCond
             , pos_features_1
             , bsmtExposure
             , kitchenQual
             , house_function
             , pool_good
             , sale_cond
             , overallQual
--           , qual_ext
--           , qual_bsmt
            ]

mkModel :: [House] -> [Double]
mkModel houses = concat $ toLists $ (inv (tr m `mul` m)) `mul` (tr m) `mul` v
    where
        xs = map (\h -> map (\f -> f h) predictors) houses
        w = length predictors
        l = length xs
        m = (><) l w (concat xs)
        ys = map salePrice houses
        v = (><) l 1 ys

useLinearModel :: [Double] -> House -> Double
useLinearModel dd h = sum $ zipWith (*) (map (\f -> f h) predictors) dd

-- EXTRA FEATURES

qual_ext :: House -> Double
qual_ext h = (1 + overallQual h) * (1 + exterCond h) / 2

qual_bsmt :: House -> Double
qual_bsmt h = (1 + overallQual h) * (1 + totalBsmtSF h) / 2

pos_features_1 :: House -> Double
pos_features_1 h
  | condition1 h `elem` [0.375, 0.5] = 1
  | otherwise = 0

house_function :: House -> Double
house_function h
  | functional h `elem` [0.66, 1] = 1
  | otherwise = 0

pool_good :: House -> Double
pool_good h
  | poolQC h == 0 = 1
  | otherwise = 0

sale_cond :: House -> Double
sale_cond h
  | saleCondition h == 0.8 = 1
  | saleCondition h `elem` [0.64, 0.32] = 0.66
  | saleCondition h `elem` [0.48, 0] = 0.33
  | otherwise = 0

lo_neigh :: [Double]
lo_neigh = [0.08, 0.12, 0.28, 0.36, 0.4, 0.72, 0.8]

mid_neigh :: [Double]
mid_neigh = [0, 0.04, 0.2, 0.32, 0.44, 0.52, 0.56, 0.6, 0.76, 0.84]

high_neigh :: [Double]
high_neigh = [0.16, 0.24, 0.64, 0.68, 0.88, 0.92, 0.96, 1]

price_per_neighborhood :: House -> Double
price_per_neighborhood h
  | neighborhood h `elem` lo_neigh = 0
  | neighborhood h `elem` mid_neigh = 0.5
  | neighborhood h `elem` high_neigh = 1
