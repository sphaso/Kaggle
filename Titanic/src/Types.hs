{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Types where

import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), DefaultOrdered(..), FromField, runParser, parseField)
import qualified Data.Csv as C
import GHC.Generics

newtype Age = Age Int
    deriving (Eq, Ord, Show)

newtype Fare = Fare Float
    deriving (Eq, Ord, Show)

newtype Embarked = Embarked Char
    deriving (Eq, Ord, Show)

data Passenger = Passenger {
    pid :: Int,
    survived :: Maybe Int,
    pclass :: Int,
    name :: String,
    title :: String,
    sex :: String,
    age :: Age,
    sibsp :: Int,
    parch :: Int,
    familySize :: String,
    ticket :: String,
    fare :: Fare,
    cabin :: Maybe String,
    embarked :: Embarked
} deriving (Eq, Show, Generic)

instance FromNamedRecord Passenger where
    parseNamedRecord r =
        case length r of
            12 -> Passenger <$> r C..: "PassengerId"
                           <*> r C..: "Survived"
                           <*> r C..: "Pclass"
                           <*> r C..: "Name"
                           <*> (titleGrab <$> r C..: "Name")
                           <*> r C..: "Sex"
                           <*> r C..: "Age"
                           <*> r C..: "SibSp"
                           <*> r C..: "Parch"
                           <*> (familySizeGrab <$> r C..: "SibSp" <*> r C..: "Parch")
                           <*> r C..: "Ticket"
                           <*> r C..: "Fare"
                           <*> r C..: "Cabin"
                           <*> r C..: "Embarked"

            11 -> Passenger <$> r C..: "PassengerId"
                           <*> pure Nothing
                           <*> r C..: "Pclass"
                           <*> r C..: "Name"
                           <*> (titleGrab <$> r C..: "Name")
                           <*> r C..: "Sex"
                           <*> r C..: "Age"
                           <*> r C..: "SibSp"
                           <*> r C..: "Parch"
                           <*> (familySizeGrab <$> r C..: "SibSp" <*> r C..: "Parch")
                           <*> r C..: "Ticket"
                           <*> r C..: "Fare"
                           <*> r C..: "Cabin"
                           <*> r C..: "Embarked"

instance DefaultOrdered Passenger

instance FromField Age where
    parseField s = case runParser (parseField s) of
        Left err -> pure $ Age 666 -- needs to be cleaned later
        Right (n :: Float) -> pure $ Age $ round n

instance FromField Fare where
    parseField s = case runParser (parseField s) of
        Left err -> pure $ Fare 8.05 -- mean value
        Right n -> pure $ Fare n

instance FromField Embarked where
    parseField s = case runParser (parseField s) of
        Left err -> pure $ Embarked 'C' -- correlation with missing data
        Right n -> pure $ Embarked n

familySizeGrab :: Int -> Int -> String
familySizeGrab sib parch | sizef == 1 = "Single"
                         | sizef > 4  = "Large"
                         | otherwise  = "Small"
    where
        sizef = sib + parch + 1

titleGrab :: String -> String
titleGrab s = cleanTitle $ takeWhile (\c -> c /= '.') $ drop 2 $ dropWhile (\c -> c /= ',') s

cleanTitle :: String -> String
cleanTitle "Mlle" = "Miss"
cleanTitle "Mme"  = "Mrs"
cleanTitle "Master" = "Master"
cleanTitle "Miss" = "Miss"
cleanTitle "Mr" = "Mr"
cleanTitle "Mrs" = "Mrs"
cleanTitle _     = "Other"
