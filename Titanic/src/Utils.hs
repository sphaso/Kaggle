module Utils where

import Data.List (maximumBy)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (encode)
import qualified Data.Map.Strict as M

updater :: (Ord a) => a -> M.Map a Int -> M.Map a Int
updater s m = case M.lookup s m of
                Just k -> M.adjust (+1) s m
                Nothing -> M.insert s 1 m

frequencyTable :: (Ord a) => [a] -> M.Map a Int
frequencyTable = foldr updater M.empty

mean :: (Ord a) => [a] -> (a, Int)
mean values = maximumBy (\a b -> compare (snd a) (snd b)) $ M.toList (frequencyTable values)

toCsv :: [(Int, Int)] -> BL.ByteString
toCsv = encode
