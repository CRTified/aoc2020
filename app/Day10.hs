{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Main where

import Criterion.Main
import Control.Arrow ((***), (&&&))
import Control.Concurrent
import qualified Control.Monad.State.Lazy as State
import Data.List (groupBy, intercalate, transpose, inits, tails, sort)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Either (rights)
import Data.Char (isDigit, isHexDigit)
import Lib
import Data.Bits
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import "graphite" Data.Graph.Types
import "graphite" Data.Graph.DGraph
import "graphite" Data.Graph.Visualize
import "graphite" Data.Graph.Traversal
import Text.Parsec
import Text.ParserCombinators.Parsec
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map.Strict as Map

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

parseInput :: String -> [Integer]
parseInput = (map (read)) . lines

getSolution1 i = uncurry (*) $ foldr (update) (0, 1) $ allPairs . sort $ 0 : i
  where
    update :: (Integer, Integer) -> (Int, Int) -> (Int, Int)
    update (a, b) (δ1, δ2)
      | b - a == 1 = (δ1 + 1, δ2)
      | b - a == 3 = (δ1, δ2 + 1)
      | otherwise = (δ1, δ2)
      
    allPairs :: [a] -> [(a, a)]
    allPairs [] = []
    allPairs (_:[]) = []
    allPairs (x:y:[]) = [(x, y)]
    allPairs (x:y:xs) = (x, y) : (allPairs (y:xs))
                    
--getSolution2 :: [Integer] -> Integer
getSolution2 i = (flip (Map.!?)) 0 $ foldr (solve) initialMap (init input)
  where
    input = sort $ 0 : i
    initialMap = Map.fromList [(i, if i == maxIn then 1 else 0) | i <- input]
    maxIn = maximum input
    solve :: Integer -> Map Integer Integer -> Map Integer Integer
    solve x m = Map.update (const newVal) x m
      where
        newVal = Just $ sum $ catMaybes [Map.lookup (x + n) m | n <- [1..3]]


      
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
      bgroup "Day 9" [ bench "Part 1" $ whnf getSolution1 input ]
    , bgroup "Day 9" [ bench "Part 2" $ whnf getSolution2 input ]
    ]
