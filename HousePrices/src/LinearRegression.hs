module LinearRegression where

import Types
import Utils

data Line = Line {
    b :: Double
  , c :: Double
} deriving (Eq, Show)

data Model = Model {
    bi :: [Double]
  , b0 :: Double
} deriving (Eq, Show)

slope :: [House] -> (House -> Double) -> Double
slope houses f = cov / var
    where
        cov = covariance houses f salePrice
        var = variance houses f

slope' :: [Double] -> [Double] -> Double
slope' prices residuals = cov / var
    where
        cov = covariance' prices residuals
        var = variance' residuals

intercept :: [House] -> (House -> Double) -> Double
intercept houses f = meanY - meanX * (slope houses f)
    where
        meanY = average $ map salePrice houses
        meanX = average $ map f houses

intercept' :: [House] -> [Double] -> Double
intercept' houses coeffs = avgY - xs
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
             , pos_features_1
             , bsmtExposure
             , kitchenQual
             , house_function
             , pool_good
             , sale_cond
             , overallQual
             , qual_ext
             , qual_bsmt
            ]

mkLine :: [House] -> [Line]
mkLine houses = map (\f -> Line { b = slope houses f, c = intercept houses f }) predictors

linePrediction :: Line -> (House -> Double) -> House -> Double
linePrediction (Line b c) f h = b * (f h)

coeff :: [House] -> [Line] -> [Double]
coeff houses lines = map (slope' prices) residuals
    where
        residuals :: [[Double]]
        residuals = map (\(f, l) -> map (\h -> salePrice h - linePrediction l f h) houses) llx
        llx = zip predictors lines
        prices = map salePrice houses

mkModel :: [House] -> Model
mkModel houses = Model { bi = coeffs, b0 = intercept' houses coeffs }
    where
        ll = mkLine houses
        coeffs = coeff houses ll

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
