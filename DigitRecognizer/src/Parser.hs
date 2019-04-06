module Parser where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (readInt)
import Data.Csv
import qualified Data.Vector as V

parse :: BL.ByteString -> [(Double, [Double])]
parse s = case decode HasHeader s of
                 Right v -> V.toList $ V.map perLine v
                 Left err -> error err
    where
        perLine :: V.Vector BL.ByteString -> (Double, [Double])
        perLine l = let
                     (x:xs) = V.toList $ V.map reader l
                    in
                     (fromIntegral x, map normalize xs)
        reader b = maybe 0 fst (readInt b)

parse' :: BL.ByteString -> [[Double]]
parse' s = case decode HasHeader s of
                Right v -> V.toList $ V.map (V.toList . V.map normalize) v
                Left err -> error err

toCsv :: [(Int, Int)] -> BL.ByteString
toCsv = encode

-- shrink all pixels in the range 0.01 - 1
normalize :: Int -> Double
normalize i = (d / 255) * 0.99 + 0.01
    where
        d :: Double
        d = fromIntegral i
