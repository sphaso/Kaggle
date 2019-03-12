module NaiveBayes where

import Types
import Utils

import qualified Data.Map.Strict as M

data BayesModel = BayesModel {
    prOutcome :: Float,
    prClass :: M.Map Int Int,
    prTitle :: M.Map String Int,
    prSex :: M.Map String Int,
    prAge :: M.Map Age Int,
    prSib :: M.Map Int Int,
    prPar :: M.Map Int Int,
    prTicket :: M.Map String Int,
    prFare :: M.Map Fare Int,
    prEmbarked :: M.Map Embarked Int,
    prFamilySize :: M.Map String Int,
    total :: Int
} deriving (Eq, Show)

survivors :: [Passenger] -> [Passenger]
survivors = filter (\p -> survived p == Just 1)

dead :: [Passenger] -> [Passenger]
dead = filter (\p -> survived p == Just 0)

mkBayesModel :: [Passenger] -> Bool -> BayesModel
mkBayesModel pp True = BayesModel {
                        prOutcome = survivalR,
                        prClass = genTable pclass,
                        prTitle = genTable title,
                        prSex = genTable sex,
                        prAge = genTable age,
                        prSib = genTable sibsp,
                        prPar = genTable parch,
                        prTicket = genTable ticket,
                        prFare = genTable fare,
                        prEmbarked = genTable embarked,
                        prFamilySize = genTable familySize,
                        total = length $ survivors pp
       }
    where
        genTable f = frequencyTable (map f $ survivors pp)
        survivalR = (fromIntegral $ length pp) / (fromIntegral $ length $ survivors pp)

mkBayesModel pp False = BayesModel {
                        prOutcome = deathR,
                        prClass = genTable pclass,
                        prTitle = genTable title,
                        prSex = genTable sex,
                        prAge = genTable age,
                        prSib = genTable sibsp,
                        prPar = genTable parch,
                        prTicket = genTable ticket,
                        prFare = genTable fare,
                        prEmbarked = genTable embarked,
                        prFamilySize = genTable familySize,
                        total = length $ survivors pp
       }
    where
        genTable f = frequencyTable (map f $ dead pp)
        deathR = (fromIntegral $ length pp) / (fromIntegral $ length $ dead pp)


willSurvive :: BayesModel -> BayesModel -> Passenger -> Bool
willSurvive survival death p = (categoryRate survival p) > (categoryRate death p)

categoryRate :: BayesModel -> Passenger -> Float
categoryRate m p = (prOutcome m)
                 * probability prClass pclass
                 * probability prTitle title
                 * probability prSex sex
                 * probability prAge age
--               * probability prSib sibsp
--               * probability prPar parch
--               * probability prTicket ticket
--               * probability prFare fare
--               * probability prEmbarked embarked
                 * probability prFamilySize familySize
    where
        probability f g = case M.lookup (g p) (f m) of
                               Just n -> (fromIntegral n / fromIntegral (total m))
                               _  -> 1 / (fromIntegral (total m))

