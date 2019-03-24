module Parser where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

import Types

parse :: BL.ByteString -> [Double -> House]
parse s = case decodeByName s of
                 Right (_, v) -> V.toList v
                 Left err -> error err

parseFullHouse :: BL.ByteString -> [House]
parseFullHouse s = case decodeByName s of
                     Right (_, v) -> V.toList v
                     Left err -> error err

parseValidationHouse :: BL.ByteString -> [House]
parseValidationHouse s = case decodeByName s of
                           Right (_, v) -> map (\x -> x (0 :: Double)) (V.toList v)
                           Left err -> error err

toCsv :: [(Int, Double)] -> BL.ByteString
toCsv = encode
