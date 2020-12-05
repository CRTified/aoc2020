{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Control.Arrow ((***))
import Data.List ((\\), groupBy, intercalate, transpose)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Char (isDigit, isHexDigit)
import Lib
import Data.Bits
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T

type Seat = (Int, Int)

seatID :: Seat -> Int
seatID (row, col) = row * 8 + col

parseToSeat :: String -> Seat
parseToSeat x = (foldl (goRow) 0 $ take 7 x,
                 foldl (goCol) 0 $ drop 7 x)
  where
    goRow acc 'B' = 1 .|. (shift acc 1)
    goRow acc  _  = shift acc 1
    goCol acc 'R' = 1 .|. (shift acc 1)
    goCol acc  _  = shift acc 1

parseInput :: String -> [Seat]
parseInput i = map (parseToSeat) $ lines i
    
getSolution1 b = maximum $ map seatID b
getSolution2 b = filter (\x -> not $ (succ x `elem` seatlist) || (pred x `elem` seatlist)) seatlist
  where
    seatlist = [1..127 * 8 + 7] \\ map seatID b

main :: IO ()
main = do
  rawInput <- getContents
  let input = parseInput $ rawInput
  
  putStrLn "Part 1"
  print $ getSolution1 input

  putStrLn "Part 2"
  print $ getSolution2 input

  putStrLn "Benchmark"
  defaultMain
    [
      bgroup "Day 5" [ bench "Part 1" $ whnf getSolution1 input ]
    , bgroup "Day 5" [ bench "Part 2" $ whnf getSolution2 input ]
    ]
