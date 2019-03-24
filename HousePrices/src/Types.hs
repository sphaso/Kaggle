{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances #-}

module Types where

import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), DefaultOrdered(..), FromField, runParser, parseField)
import qualified Data.Csv as C
import GHC.Generics

data House = House {
    id :: Int
  , msSubClass :: Double
  , msZoning :: Double
  , lotFrontage :: Double
  , lotArea :: Double
  , street :: Double
  , alley :: Double
  , lotShape :: Double
  , landContour :: Double
  , utilities :: Double
  , lotConfig :: Double
  , landSlope :: Double
  , neighborhood :: Double
  , condition1 :: Double
  , condition2 :: Double
  , bldgType :: Double
  , houseStyle :: Double
  , overallQual :: Double
  , overallCond :: Double
  , yearBuilt :: Double
  , yearRemodAdd :: Double
  , roofStyle :: Double
  , roofMatl :: Double
  , exterior1 :: Double
  , exterior2 :: Double
  , masVnrType :: Double
  , masVnrArea :: Double
  , exterQual :: Double
  , exterCond :: Double
  , foundation :: Double
  , bsmtQual :: Double
  , bsmtCond :: Double
  , bsmtExposure :: Double
  , bsmtFinType1 :: Double
  , bsmtFinSF1 :: Double
  , bsmtFinType2 :: Double
  , bsmtFinSF2 :: Double
  , bsmtUnfSF :: Double
  , totalBsmtSF :: Double
  , heating :: Double
  , heatingQC :: Double
  , centralAir :: Double
  , electrical :: Double
  , firstFloorSF :: Double
  , secondFloorSF :: Double
  , lowQualFinSF :: Double
  , grLivArea :: Double
  , bsmtFullBath :: Double
  , bsmtHalfBath :: Double
  , fullBath :: Double
  , halfBath :: Double
  , bedroomAbvGr :: Double
  , kitchenAbvGr :: Double
  , kitchenQual :: Double
  , totRmsAbvGrd :: Double
  , functional :: Double
  , fireplaces :: Double
  , fireplaceQual :: Double
  , garageType :: Double
  , garageYrBlt :: Double
  , garageFinish :: Double
  , garageCars :: Double
  , garageArea :: Double
  , garageQual :: Double
  , garageCond :: Double
  , pavedDrive :: Double
  , woodDeckSF :: Double
  , openPorchSF :: Double
  , enclosedPorch :: Double
  , ssnPorch3 :: Double
  , screenPorch :: Double
  , poolArea :: Double
  , poolQC :: Double
  , fence :: Double
  , miscFeature :: Double
  , miscVal :: Double
  , moSold :: Double
  , yrSold :: Double
  , saleType :: Double
  , saleCondition :: Double
  , salePrice :: Double
} deriving (Eq, Generic, Show)

intToDouble :: Double -> C.Parser Int -> C.Parser Double
intToDouble d p = ((/d) . fromIntegral) <$> p

instance FromNamedRecord House where
    parseNamedRecord r = let
                            h :: C.Parser (Double -> House)
                            h = parseNamedRecord r
                            s :: C.Parser Double
                            s = r C..: "SalePrice"
                        in
                            h <*> s

instance FromNamedRecord (Double -> House) where
    parseNamedRecord r = House <$>
            r C..: "Id"
        <*> (msSubClassClean <$> (r C..: "MSSubClass"))
        <*> (msZoningClean <$> (r C..: "MSZoning"))
        <*> (lotFrontageClean <$> (r C..: "LotFrontage"))
        <*> (lotAreaClean <$> (r C..: "LotArea"))
        <*> (streetClean <$> (r C..: "Street"))
        <*> (alleyClean <$> (r C..: "Alley"))
        <*> (lotShapeClean <$> (r C..: "LotShape"))
        <*> (landContourClean <$> (r C..: "LandContour"))
        <*> (utilitiesClean <$> (r C..: "Utilities"))
        <*> (lotConfigClean <$> (r C..: "LotConfig"))
        <*> (landSlopeClean <$> (r C..: "LandSlope"))
        <*> (neighborhoodClean <$> (r C..: "Neighborhood"))
        <*> (conditionClean <$> (r C..: "Condition1"))
        <*> (conditionClean <$> (r C..: "Condition2"))
        <*> (bldgTypeClean <$> (r C..: "BldgType"))
        <*> (houseStyleClean <$> (r C..: "HouseStyle"))
        <*> (intToDouble 10 (r C..: "OverallQual"))
        <*> (intToDouble 10 (r C..: "OverallCond"))
        <*> (yearBuiltClean <$> (r C..: "YearBuilt"))
        <*> (yearRemodAddClean <$> (r C..: "YearRemodAdd"))
        <*> (roofStyleClean <$> (r C..: "RoofStyle"))
        <*> (roofMatClean <$> (r C..: "RoofMatl"))
        <*> (exteriorClean <$> (r C..: "Exterior1st"))
        <*> (exteriorClean <$> (r C..: "Exterior2nd"))
        <*> (masVnrTypeClean <$> (r C..: "MasVnrType"))
        <*> (masVnrAreaClean <$> (r C..: "MasVnrArea"))
        <*> (exterQualClean <$> (r C..: "ExterQual"))
        <*> (exterClean <$> (r C..: "ExterCond"))
        <*> (foundationClean <$> (r C..: "Foundation"))
        <*> (exterClean <$> (r C..: "BsmtQual"))
        <*> (exterClean <$> (r C..: "BsmtCond"))
        <*> (bsmtExpoClean <$> (r C..: "BsmtExposure"))
        <*> (bsmtFinTypeClean <$> (r C..: "BsmtFinType1"))
        <*> (bsmtFinSFXClean <$> (r C..: "BsmtFinSF1"))
        <*> (bsmtFinTypeClean <$> (r C..: "BsmtFinType2"))
        <*> (bsmtFinSFXClean <$> (r C..: "BsmtFinSF2"))
        <*> (bsmtUnfSFClean <$> (r C..: "BsmtUnfSF"))
        <*> (totalBsmtSFClean <$> (r C..: "TotalBsmtSF"))
        <*> (heatingClean <$> (r C..: "Heating"))
        <*> (exterClean <$> (r C..: "HeatingQC"))
        <*> (yesNo <$> (r C..: "CentralAir"))
        <*> (electricalClean <$> (r C..: "Electrical"))
        <*> (intToDouble 5000 (r C..: "1stFlrSF"))
        <*> (intToDouble 2100 (r C..: "2ndFlrSF"))
        <*> (intToDouble 600 (r C..: "LowQualFinSF"))
        <*> (intToDouble 6000 (r C..: "GrLivArea"))
        <*> (bsmtFullBathClean <$> (r C..: "BsmtFullBath"))
        <*> (bsmtHalfBathClean <$> (r C..: "BsmtHalfBath"))
        <*> (intToDouble 3 (r C..: "FullBath"))
        <*> (intToDouble 2 (r C..: "HalfBath"))
        <*> (intToDouble 8 (r C..: "BedroomAbvGr"))
        <*> (intToDouble 3 (r C..: "KitchenAbvGr"))
        <*> (exterClean <$> (r C..: "KitchenQual"))
        <*> (intToDouble 15 (r C..: "TotRmsAbvGrd"))
        <*> (functionalClean <$> (r C..: "Functional"))
        <*> (intToDouble 3 (r C..: "Fireplaces"))
        <*> (exterClean <$> (r C..: "FireplaceQu"))
        <*> (garageTypeClean <$> (r C..: "GarageType"))
        <*> (garageYrBuildClean <$> (r C..: "GarageYrBlt"))
        <*> (garageFinishClean <$> (r C..: "GarageFinish"))
        <*> (garageCarsClean <$> (r C..: "GarageCars"))
        <*> (garageAreaClean <$> (r C..: "GarageArea"))
        <*> (exterClean <$> (r C..: "GarageQual"))
        <*> (exterClean <$> (r C..: "GarageCond"))
        <*> (pavedDriveClean <$> (r C..: "PavedDrive"))
        <*> (intToDouble 900 (r C..: "WoodDeckSF"))
        <*> (intToDouble 550 (r C..: "OpenPorchSF"))
        <*> (intToDouble 550 (r C..: "EnclosedPorch"))
        <*> (intToDouble 550 (r C..: "3SsnPorch"))
        <*> (intToDouble 550 (r C..: "ScreenPorch"))
        <*> (intToDouble 700 (r C..: "PoolArea"))
        <*> (exterClean <$> (r C..: "PoolQC"))
        <*> (fenceClean <$> (r C..: "Fence"))
        <*> (miscFeatureClean <$> (r C..: "MiscFeature"))
        <*> (intToDouble 1600 (r C..: "MiscVal"))
        <*> (intToDouble 12 (r C..: "MoSold"))
        <*> (yearSoldClean <$> (r C..: "YrSold"))
        <*> (saleTypeClean <$> (r C..: "SaleType"))
        <*> (saleConditionClean <$> (r C..: "SaleCondition"))

msSubClassClean :: Double -> Double
msSubClassClean 20 = 0
msSubClassClean 30 = 0.066
msSubClassClean 40 = 0.133
msSubClassClean 45 = 0.199
msSubClassClean 50 = 0.265
msSubClassClean 60 = 0.331
msSubClassClean 70 = 0.397
msSubClassClean 75 = 0.463
msSubClassClean 80 = 0.529
msSubClassClean 85 = 0.595
msSubClassClean 90 = 0.661
msSubClassClean 120 = 0.727
msSubClassClean 150 = 0.793
msSubClassClean 160 = 0.859
msSubClassClean 180 = 0.925
msSubClassClean 190 = 1

msZoningClean :: String -> Double
msZoningClean "RL" = 0
msZoningClean "RM" = 0.25
msZoningClean "C (all)" = 0.5
msZoningClean "FV" = 0.75
msZoningClean "RH" = 1

lotAreaClean :: Double -> Double
lotAreaClean d = (d - 1300) / 216000

lotFrontageClean :: String -> Double
lotFrontageClean "NA" = 70 / 400
lotFrontageClean d = (fromIntegral $ read d) / 400

streetClean :: String -> Double
streetClean "Pave" = 1
streetClean "Grvl" = 0

alleyClean :: String -> Double
alleyClean "Pave" = 1
alleyClean "Grvl" = 0.5
alleyClean "NA" = 0

lotShapeClean :: String -> Double
lotShapeClean "IR1" = 0
lotShapeClean "IR2" = 0.3
lotShapeClean "IR3" = 0.6
lotShapeClean "Reg" = 1

landContourClean :: String -> Double
landContourClean "Lvl" = 0
landContourClean "Bnk" = 0.3
landContourClean "Low" = 0.6
landContourClean "HLS" = 1

utilitiesClean :: String -> Double
utilitiesClean "AllPub" = 1
utilitiesClean "NoSewr" = 0.66
utilitiesClean "NoSeWa" = 0.33
utilitiesClean "ELO" = 0

lotConfigClean :: String -> Double
lotConfigClean "Inside" = 0
lotConfigClean "Corner" = 0.25
lotConfigClean "FR2" = 0.5
lotConfigClean "FR3" = 0.75
lotConfigClean "CulDSac" = 1

landSlopeClean :: String -> Double
landSlopeClean "Gtl" = 0
landSlopeClean "Mod" = 0.5
landSlopeClean "Sev" = 1

neighborhoodClean :: String -> Double
neighborhoodClean "Blmngtn" = 0
neighborhoodClean "Blueste" = 0.04
neighborhoodClean "BrDale" = 0.08
neighborhoodClean "BrkSide" = 0.12
neighborhoodClean "ClearCr" = 0.16
neighborhoodClean "CollgCr" = 0.2
neighborhoodClean "Crawfor" = 0.24
neighborhoodClean "Edwards" = 0.28
neighborhoodClean "Gilbert" = 0.32
neighborhoodClean "IDOTRR" = 0.36
neighborhoodClean "MeadowV" = 0.4
neighborhoodClean "Mitchel" = 0.44
neighborhoodClean "NAmes" = 0.52
neighborhoodClean "NPkVill" = 0.56
neighborhoodClean "NWAmes" = 0.6
neighborhoodClean "NoRidge" = 0.64
neighborhoodClean "NridgHt" = 0.68
neighborhoodClean "OldTown" = 0.72
neighborhoodClean "SWISU" = 0.76
neighborhoodClean "Sawyer" = 0.8
neighborhoodClean "SawyerW" = 0.84
neighborhoodClean "Somerst" = 0.88
neighborhoodClean "StoneBr" = 0.92
neighborhoodClean "Timber" = 0.96
neighborhoodClean "Veenker" = 1

conditionClean :: String -> Double
conditionClean "Artery" = 0
conditionClean "Feedr" = 0.125
conditionClean "Norm" = 0.25
conditionClean "PosA" = 0.375
conditionClean "PosN" = 0.5
conditionClean "RRAe" = 0.615
conditionClean "RRAn" = 0.73
conditionClean "RRNe" = 0.845
conditionClean "RRNn" = 1

bldgTypeClean :: String -> Double
bldgTypeClean "1Fam" = 0
bldgTypeClean "Duplex" = 0.25
bldgTypeClean "Twnhs" = 0.5
bldgTypeClean "TwnhsE" = 0.75
bldgTypeClean "2fmCon" = 1

houseStyleClean :: String -> Double
houseStyleClean "1.5Fin" = 0
houseStyleClean "1.5Unf" = 0.14
houseStyleClean "1Story" = 0.28
houseStyleClean "2.5Fin" = 0.42
houseStyleClean "2.5Unf" = 0.56
houseStyleClean "2Story" = 0.7
houseStyleClean "SFoyer" = 0.84
houseStyleClean "SLvl" = 1

yearBuiltClean :: Double -> Double
yearBuiltClean d = (d - 1872) / 138

yearRemodAddClean :: Double -> Double
yearRemodAddClean d = (d - 1950) / 60

roofStyleClean :: String -> Double
roofStyleClean "Flat" = 0
roofStyleClean "Gable" = 0.2
roofStyleClean "Gambrel" = 0.4
roofStyleClean "Hip" = 0.6
roofStyleClean "Mansard" = 0.8
roofStyleClean "Shed" = 1

roofMatClean :: String -> Double
roofMatClean "ClyTile" = 0
roofMatClean "CompShg" = 0.14
roofMatClean "Membran" = 0.28
roofMatClean "Metal" = 0.42
roofMatClean "Roll" = 0.56
roofMatClean "Tar&Grv" = 0.7
roofMatClean "WdShake" = 0.84
roofMatClean "WdShngl" = 1

exteriorClean :: String -> Double
exteriorClean "AsbShng" = 0
exteriorClean "AsphShn" = 0.07
exteriorClean "BrkComm" = 0.14
exteriorClean "Brk Cmn" = 0.14
exteriorClean "BrkFace" = 0.21
exteriorClean "CBlock" = 0.28
exteriorClean "CemntBd" = 0.35
exteriorClean "CmentBd" = 0.35
exteriorClean "HdBoard" = 0.42
exteriorClean "ImStucc" = 0.49
exteriorClean "MetalSd" = 0.56
exteriorClean "Other" = 0.63
exteriorClean "Plywood" = 0.7
exteriorClean "Stone" = 0.77
exteriorClean "Stucco" = 0.84
exteriorClean "VinylSd" = 0.91
exteriorClean "Wd Sdng" = 0.98
exteriorClean "WdShing" = 1
exteriorClean "Wd Shng" = 1

masVnrTypeClean :: String -> Double
masVnrTypeClean "BrkCmn" = 0
masVnrTypeClean "BrkFace" = 0.3
masVnrTypeClean "NA" = 0.6
masVnrTypeClean "None" = 0.6
masVnrTypeClean "Stone" = 1

masVnrAreaClean :: String -> Double
masVnrAreaClean "NA" = 0
masVnrAreaClean d = (read d) / 1600

exterQualClean :: String -> Double
exterQualClean "Ex" = 1
exterQualClean "Gd" = 0.66
exterQualClean "TA" = 0.33
exterQualClean "Fa" = 0

exterClean :: String -> Double
exterClean "Ex" = 0
exterClean "Fa" = 0.2
exterClean "Gd" = 0.4
exterClean "Po" = 0.6
exterClean "TA" = 0.8
exterClean "NA" = 1

foundationClean :: String -> Double
foundationClean "BrkTil" = 0
foundationClean "CBlock" = 0.2
foundationClean "PConc" = 0.4
foundationClean "Slab" = 0.6
foundationClean "Stone" = 0.8
foundationClean "Wood" = 1

bsmtExpoClean :: String -> Double
bsmtExpoClean "Gd" = 1
bsmtExpoClean "Av" = 0.75
bsmtExpoClean "Mn" = 0.5
bsmtExpoClean "NA" = 0.25
bsmtExpoClean "No" = 0

bsmtFinTypeClean :: String -> Double
bsmtFinTypeClean "ALQ" = 0
bsmtFinTypeClean "BLQ" = 0.16
bsmtFinTypeClean "GLQ" = 0.32
bsmtFinTypeClean "LwQ" = 0.48
bsmtFinTypeClean "NA" = 0.64
bsmtFinTypeClean "Rec" = 0.8
bsmtFinTypeClean "Unf" = 1

bsmtFinSFXClean :: String -> Double
bsmtFinSFXClean "NA" = 0
bsmtFinSFXClean d = (read d) / 1500

bsmtUnfSFClean :: String -> Double
bsmtUnfSFClean "NA" = 0
bsmtUnfSFClean d = (read d) / 2500

totalBsmtSFClean :: String -> Double
totalBsmtSFClean "NA" = 0
totalBsmtSFClean d = (read d) / 6500

heatingClean :: String -> Double
heatingClean "Floor" = 0
heatingClean "GasA" = 0.2
heatingClean "GasW" = 0.4
heatingClean "Grav" = 0.6
heatingClean "OthW" = 0.8
heatingClean "Wall" = 1

yesNo :: String -> Double
yesNo "Y" = 0
yesNo "N" = 1

pavedDriveClean :: String -> Double
pavedDriveClean "Y" = 0
pavedDriveClean "P" = 0.5
pavedDriveClean "N" = 1

electricalClean :: String -> Double
electricalClean "FuseA" = 0
electricalClean "FuseF" = 0.25
electricalClean "FuseP" = 0.5
electricalClean "Mix" = 0.75
electricalClean "NA" = 1
electricalClean "SBrkr" = 1

bsmtFullBathClean :: String -> Double
bsmtFullBathClean "NA" = 0
bsmtFullBathClean d = (read d) / 3

bsmtHalfBathClean :: String -> Double
bsmtHalfBathClean "NA" = 0
bsmtHalfBathClean d = (read d) / 2

functionalClean :: String -> Double
functionalClean "Maj1" = 0
functionalClean "Maj2" = 0.16
functionalClean "Min1" = 0.32
functionalClean "Min2" = 0.48
functionalClean "Mod" = 0.66
functionalClean "Sev" = 0.82
functionalClean "Typ" = 1
functionalClean "NA" = 0

garageTypeClean :: String -> Double
garageTypeClean "2Types" = 0
garageTypeClean "Attchd" = 0.2
garageTypeClean "Basment" = 0.4
garageTypeClean "BuiltIn" = 0.6
garageTypeClean "CarPort" = 0.8
garageTypeClean "Detchd" = 1
garageTypeClean "NA" = 0.2

garageYrBuildClean :: String -> Double
garageYrBuildClean "NA" = 0 -- house has no garage, get over it!
garageYrBuildClean d = ((read d :: Double) - 1900) / 110

garageFinishClean :: String -> Double
garageFinishClean "Fin" = 0
garageFinishClean "Unf" = 0.33
garageFinishClean "RFn" = 0.66
garageFinishClean "NA" = 1

garageCarsClean :: String -> Double
garageCarsClean "NA" = 0
garageCarsClean d = (read d) / 4

garageAreaClean :: String -> Double
garageAreaClean "NA" = 0
garageAreaClean d = (read d) / 4

fenceClean :: String -> Double
fenceClean "GdPrv" = 0
fenceClean "GdWo" = 0.25
fenceClean "MnPrv" = 0.5
fenceClean "MnWw" = 0.75
fenceClean "NA" = 1

miscFeatureClean :: String -> Double
miscFeatureClean "Gar2" = 0
miscFeatureClean "Othr" = 0.25
miscFeatureClean "Shed" = 0.5
miscFeatureClean "TenC" = 0.75
miscFeatureClean "NA" = 1 -- most relevant value

yearSoldClean :: Double -> Double
yearSoldClean d = (d - 2006) / 4

saleTypeClean :: String -> Double
saleTypeClean "COD" = 0
saleTypeClean "CWD" = 0.11
saleTypeClean "Con" = 0.22
saleTypeClean "ConLD" = 0.33
saleTypeClean "ConLI" = 0.44
saleTypeClean "ConLw" = 0.55
saleTypeClean "New" = 0.66
saleTypeClean "Oth" = 0.77
saleTypeClean "SaleType" = 0.88
saleTypeClean "WD" = 1

saleConditionClean :: String -> Double
saleConditionClean "Abnorml" = 0
saleConditionClean "AdjLand" = 0.16
saleConditionClean "Alloca" = 0.32
saleConditionClean "Family" = 0.48
saleConditionClean "Normal" = 0.64
saleConditionClean "Partial" = 0.8
saleConditionClean "SaleCondition" = 1
