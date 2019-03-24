module Utils where

import Types

average :: [Double] -> Double
average d = (sum d) / l
    where
        l = fromIntegral $ length d

variance :: [House] -> (House -> Double) -> Double
variance houses f = average (map (\p -> (p - mean) ^ 2) values)
    where
        values = map f houses
        mean = average values

variance' :: [Double] -> Double
variance' d = average (map (\v -> ( v - avg) ^ 2) d)
    where
        avg = average d

standardDeviation :: [House] -> (House -> Double) -> Double
standardDeviation houses f = sqrt $ variance houses f

standardDeviation' :: [Double] -> Double
standardDeviation' d = sqrt $ variance' d

covariance :: [House] -> (House -> Double) -> (House -> Double) -> Double
covariance houses f g = average (zipWith (\x y -> (x - avgF) * (y - avgG)) valuesF valuesG)
    where
        valuesF = map f houses
        valuesG = map g houses
        avgF = average valuesF
        avgG = average valuesG

covariance' :: [Double] -> [Double] -> Double
covariance' y x = average (zipWith (\a b -> (a - avgF) * (b - avgG)) y x)
    where
        avgF = average y
        avgG = average x

pearsonR2 :: [House] -> (House -> Double) -> (House -> Double) -> Double
pearsonR2 houses f g = (cov / (std1 * std2)) ^ 2
    where
        std1 = standardDeviation houses f
        std2 = standardDeviation houses g
        cov = covariance houses f g

features :: [(String, House -> Double)]
features = [
            ("msSubClass", msSubClass),
            ("msZoning", msZoning),
            ("lotFrontage", lotFrontage),
            ("lotArea", lotArea),
            ("street", street),
            ("alley", alley),
            ("lotShape", lotShape),
            ("landContour", landContour),
            ("utilities", utilities),
            ("lotConfig", lotConfig),
            ("landSlope", landSlope),
            ("neighborhood", neighborhood),
            ("condition1", condition1),
            ("condition2", condition2),
            ("bldgType", bldgType),
            ("houseStyle", houseStyle),
            ("overallQual", overallQual),
            ("overallCond", overallCond),
            ("yearBuilt", yearBuilt),
            ("yearRemodAdd", yearRemodAdd),
            ("roofStyle", roofStyle),
            ("roofMatl", roofMatl),
            ("exterior1", exterior1),
            ("exterior2", exterior2),
            ("masVnrType", masVnrType),
            ("masVnrArea", masVnrArea),
            ("exterQual", exterQual),
            ("exterCond", exterCond),
            ("foundation",foundation),
            ("bsmtQual", bsmtQual),
            ("bsmtCond", bsmtCond),
            ("bsmtExposure", bsmtExposure),
            ("bsmtFinType1", bsmtFinType1),
            ("bsmtFinSF1", bsmtFinSF1),
            ("bsmtFinType2", bsmtFinType2),
            ("bsmtFinSF2", bsmtFinSF2),
            ("bsmtUnfSF", bsmtUnfSF),
            ("totalBsmtSF", totalBsmtSF),
            ("heating", heating),
            ("heatingQC", heatingQC),
            ("centralAir", centralAir),
            ("electrical", electrical),
            ("firstFloorSF",firstFloorSF),
            ("secondFloorSF", secondFloorSF),
            ("lowQualFinSF", lowQualFinSF),
            ("grLivArea", grLivArea),
            ("bsmtFullBath", bsmtFullBath),
            ("bsmtHalfBath", bsmtHalfBath),
            ("fullBath", fullBath),
            ("halfBath", halfBath),
            ("bedroomAbvGr", bedroomAbvGr),
            ("kitchenAbvGr", kitchenAbvGr),
            ("kitchenQual", kitchenQual),
            ("totRmsAbvGrd", totRmsAbvGrd),
            ("functional", functional),
            ("fireplaces", fireplaces),
            ("fireplaceQual", fireplaceQual),
            ("garageType", garageType),
            ("garageYrBlt", garageYrBlt),
            ("garageFinish", garageFinish),
            ("garageCars", garageCars),
            ("garageArea", garageArea),
            ("garageQual", garageQual),
            ("garageCond", garageCond),
            ("pavedDrive", pavedDrive),
            ("woodDeckSF", woodDeckSF),
            ("openPorchSF", openPorchSF),
            ("enclosedPorch",enclosedPorch),
            ("ssnPorch3", ssnPorch3),
            ("screenPorch", screenPorch),
            ("poolArea", poolArea),
            ("poolQC", poolQC),
            ("fence", fence),
            ("miscFeature", miscFeature),
            ("miscVal", miscVal),
            ("moSold", moSold),
            ("yrSold", yrSold),
            ("saleType", saleType),
            ("saleCondition",saleCondition),
            ("salePrice", salePrice)]
