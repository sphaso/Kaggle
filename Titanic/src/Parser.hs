module Parser where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

import Types

parse :: BL.ByteString -> [Passenger]
parse s = case decodeByName s of
                 Right (_, v) -> V.toList v
                 Left err -> error err

cleanAgeData :: Passenger -> Passenger
cleanAgeData p@(Passenger _ _ 1 _ _ _ (Age 666) _ _ _ _ _ _ _) = p {age = Age 36}
cleanAgeData p@(Passenger _ _ 2 _ _ _ (Age 666) _ _ _ _ _ _ _) = p {age = Age 34}
cleanAgeData p@(Passenger _ _ 3 _ _ _ (Age 666) _ _ _ _ _ _ _) = p {age = Age 22}
cleanAgeData p = p
