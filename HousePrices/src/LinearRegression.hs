{-# LANGUAGE TupleSections #-}

module LinearRegression where

import Types
import Utils

data Model = Model {
    bi :: [Double]
  , b0 :: Double
} deriving (Eq, Show)

slope :: [Double] -> [Double] -> Double
slope v1 v2 = cov / var
    where
        cov = sum $ zipWith (*) v1 v2
        var = sum $ map (^2) v2

intercept :: [House] -> [Double] -> Double
intercept houses coeffs = avgY - xs
    where
        avgX = map (\f -> average $ map f houses) predictors
        avgY = average $ map salePrice houses
        xs = sum $ zipWith (*) avgX coeffs

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
--           , pos_features_1
             , bsmtExposure
             , kitchenQual
--           , house_function
             , pool_good
             , sale_cond
             , overallQual
--           , qual_ext
--           , qual_bsmt
            ]

coeff :: [House] -> [Double]
coeff houses = zipWith slope dDeltas mixGam
    where
        others i m = map snd $ filter ((/=i) . fst) m
        -- covariance between Y and Xi
        yAlphas :: [(House -> Double, Double)]
        yAlphas = map (\p -> (p, slope (map salePrice houses) (map p houses))) predictors
        ixYAlphas :: [(Int, (House -> Double, Double))]
        ixYAlphas = zip [1..] yAlphas
        -- residuals to Y
        dDeltas :: [[Double]]
        dDeltas = map (\(i, _) -> deltas houses (others i ixYAlphas)) ixYAlphas
        mixGam :: [[Double]]
        mixGam = gammas houses (mixalphas houses)

deltas :: [House] -> [(House -> Double, Double)] -> [Double]
deltas houses yalpha = zipWith (\y xx -> y + foldr (-) 0 xx) ys xs
    where
        ys :: [Double]
        ys = map salePrice houses
        xs :: [[Double]]
        xs = map (\(f, a) -> map ((*a) . f) houses) yalpha

mixalphas :: [House] -> [(House -> Double, [(House -> Double, Double)])]
mixalphas houses = map (\(i, f) -> (f,) $ map (singleA f) (others i)) fx
    where
        fx = zip [1..] predictors
        others i = map snd $ filter ((/=i) . fst) fx
        v f = map f houses
        singleA f g = (g, slope (v f) (v g))

gammas :: [House] ->  [(House -> Double, [(House -> Double, Double)])] -> [[Double]]
gammas houses mixA = map (\(f, as) -> gamming f as) mixA
    where
        compose h xs = map (\(f, d) -> d * f h) xs
        gamming f as = map (\h -> (f h) + foldl (-) 0 (compose h as)) houses

mkModel :: [House] -> Model
mkModel houses = Model { bi = coeffs, b0 = intercept houses coeffs }
    where
        coeffs = coeff houses

useLinearModel :: Model -> House -> Double
useLinearModel (Model bi b0) h = b0 + (sum $ zipWith (*) (map (\f -> f h) predictors) bi)

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
