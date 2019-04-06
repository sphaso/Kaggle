module NeuralNetwork where
--    (
--      initialize
--    , train
--    , query
--    , mkModel
--    , Model (..)
--    ) where

import Data.Binary
import Data.List (maximumBy)
import System.Random
import Control.Monad

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix

instance Binary Model where
    put (Model _ h o) = do
        put $ toLists h
        put $ toLists o
    get = do
        hide <- get :: Get [[Double]]
        outt <- get :: Get [[Double]]
        return $ initialize (concat hide) (concat outt)

data Model = Model {
    learningRate :: Double
  , hidden :: Matrix Double
  , output :: Matrix Double
} deriving (Eq, Show)

gauss :: Double -> IO Double
gauss scale = do
  x1 <- randomIO
  x2 <- randomIO
  return $ scale * sqrt (-2 * log x1) * cos (2 * pi * x2)

uni :: IO Double
uni = randomRIO (-1, 1)

initialize' :: IO Model
initialize' = do
    w1 <- replicateM 78400 $ uni -- 0.01
    w2 <- replicateM 1000 $ uni -- 0.01
    pure $ initialize w1 w2

initialize :: [Double] -> [Double] -> Model
initialize w1 w2 = Model {
    learningRate = 0.2
  , hidden = (><) 100 784 w1
  , output = (><) 10 100 w2
}

targetV :: Int -> Matrix Double
targetV i = (><) 1 10 $ (take i (repeat 0.01)) ++ [0.99] ++ (take (9 - i) (repeat 0.01))

mkModel :: [(Double, [Double])] -> IO Model
mkModel ii = do
    empty <- initialize'
    pure $ foldl train empty ii

train :: Model -> (Double, [Double]) -> Model
train (Model lr hid out) (y, input) = Model lr (hid + updater h_err h input') (out + updater o_err o h)
   where
       input' = (><) 784 1 input
       target = tr $ targetV (round y)
       h = layerPass hid input'
       o = layerPass out h
       o_err = target - o
       h_err = (tr out) `mul` o_err
       updater :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
       updater m n x = cmap (*lr) $ (m * n * (cmap (\e -> 1 - e) n)) `mul` (tr x)

activation :: Double -> Double
activation lx = 1.0 / (1.0 + exp (negate lx))

layerPass :: Matrix Double -> Matrix Double -> Matrix Double
layerPass m n = cmap activation $ m `mul` n

query :: Model -> [Double] -> Int
query (Model _ hid out) input = round $ fst $ maximumBy maxim $ zip ix (concat $ toLists o)
    where
        input' = (><) 784 1 input
        h = layerPass hid input'
        o = layerPass out h
        ix :: [Double]
        ix = map fromIntegral [0..9]
        maxim a b = compare (snd a) (snd b)
