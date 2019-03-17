{-# LANGUAGE TupleSections #-}

module DecisionTree where

import Types

import Data.List (delete, elemIndices, nub, nubBy, maximumBy)
import qualified Data.Map.Strict as M
import Debug.Trace

data NPassenger = NPassenger {
    n_survived :: Int
  , n_pclass :: Int
  , n_title :: Int
  , n_sex :: Int
  , n_age :: Int
  , n_sibsp :: Int
  , n_parch :: Int
  , n_familySize :: Int
  , n_embarked :: Int
} deriving (Eq, Show)

passengerToNPassenger :: Passenger -> NPassenger
passengerToNPassenger p = NPassenger {
    n_survived = survivedToInt $ survived p
  , n_pclass = pclass p
  , n_title = titleToInt $ title p
  , n_sex = sexToInt $ sex p
  , n_age = ageToInt $ age p
  , n_sibsp = sibsp p
  , n_parch = parch p
  , n_familySize = familySizeToInt $ familySize p
  , n_embarked = embarkedToInt $ embarked p
}

survivedToInt :: Maybe Int -> Int
survivedToInt (Just n) = n
survivedToInt _        = 0

titleToInt :: String -> Int
titleToInt "Miss" = 0
titleToInt "Mrs" = 1
titleToInt "Master" = 2
titleToInt "Mr" = 3
titleToInt "Other" = 4

sexToInt :: String -> Int
sexToInt "male" = 0
sexToInt "female" = 1

ageToInt :: Age -> Int
ageToInt (Age n) = n

familySizeToInt :: String -> Int
familySizeToInt "Single" = 0
familySizeToInt "Small" = 1
familySizeToInt "Large" = 2

embarkedToInt :: Embarked -> Int
embarkedToInt (Embarked 'C') = 0
embarkedToInt (Embarked 'S') = 1
embarkedToInt (Embarked 'Q') = 2

entropy :: [Int] -> Double
entropy xs = sum $ map (\x -> prob x * info x) (nub xs)
    where
        prob x = (fromIntegral (length $ elemIndices x xs)) / (fromIntegral $ length xs)
        info x = negate $ logBase 2 (prob x)

splitAttribute :: [(Int, a)] -> M.Map Int [a]
splitAttribute = foldl (\m (f,c) -> M.insertWith (++) f [c] m) M.empty

entropyPerKey :: M.Map Int [Int] -> M.Map Int Double
entropyPerKey = M.map entropy

-- classes -> (value, class) -> score
informationGain :: [Int] -> [(Int, Int)] -> Double
informationGain values tt = entropy values - newInformation
    where
        m = splitAttribute tt
        entropyMap = entropyPerKey m
        sumE = M.map (\x -> (fromIntegral $ length x) / (fromIntegral $ length values)) m
        newInformation = M.foldrWithKey (\k a b -> b + a*(entropyMap M.! k)) 0 sumE

highestInformationGain :: [String] -> [NPassenger] -> String
highestInformationGain getters px =
     snd $ maximumBy (\a b -> compare (fst a) (fst b)) $ map (\f -> (informationGain classes $ valueClass $ featureToF f, f)) getters
    where
        classes :: [Int]
        classes = map n_survived px
        valueClass :: (NPassenger -> Int) -> [(Int, Int)]
        valueClass f = zip (map f px) classes -- (value, class) per feature

splitByHighInfo :: (NPassenger -> Int) -> [NPassenger] -> M.Map Int [NPassenger]
splitByHighInfo f px = M.map concat $ splitAttribute $ map (\x -> (x, px_rest x px)) values
    where
        values = nub $ map f px
        px_rest x xs = filter ((==x) . f) xs

data DTree = DTree {
    feature :: String
  , value :: Int
  , children :: [DTree]
} | Leaf Int Int deriving (Eq, Show)

featureToF :: String -> (NPassenger -> Int)
featureToF "n_pclass" = n_pclass
featureToF "n_title" = n_title
featureToF "n_sex" = n_sex
featureToF "n_age" = n_age
featureToF "n_sibsp" = n_sibsp
featureToF "n_parch" = n_parch
featureToF "n_family" = n_familySize
featureToF "n_embarked" = n_embarked

features :: [String]
features = [
             "n_pclass"
           , "n_title"
           , "n_sex"
           , "n_age"
           , "n_sibsp"
           , "n_parch"
           , "n_family"
           , "n_embarked"
           ]

mkDecisionTree :: [NPassenger] -> DTree
mkDecisionTree px = mkDecisionTree' 666 features px

mkDecisionTree' :: Int -> [String] -> [NPassenger] -> DTree
mkDecisionTree' v [] px = Leaf v 1
mkDecisionTree' v fx px
  | 0 == resultingEntropy = Leaf v $ head . snd $ M.elemAt 0 resultingClassesSet
  | otherwise = DTree {
                        feature = highest
                      , value = v
                      , children = map snd $ M.toList $ M.mapWithKey (\k p ->
                            mkDecisionTree' k others p
                          ) splitDataSet
                      }
    where
        highest = highestInformationGain fx px
        others = delete highest fx
        splitDataSet = splitByHighInfo (featureToF highest) px
        resultingClassesSet = M.map (map n_survived) splitDataSet
        resultingEntropy = entropy $ M.foldr (++) [] $ resultingClassesSet

survivalDecision :: DTree -> NPassenger -> Int
survivalDecision (Leaf _ n) _ = n
survivalDecision (DTree g _ c) p
    | [] == matching = survivalDecision (head c) p -- terrible, figure another way out
	| otherwise = survivalDecision (head matching) p
    where
        f = featureToF g
        pick (Leaf v _) = v == f p
        pick (DTree _ v _) = v == f p
        matching = filter pick c
