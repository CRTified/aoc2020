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

parseInput :: String -> [Integer]
parseInput = (map (read)) . lines

getSolution1 i = fst $ head $ dropWhile (snd) $ fst $ foldl (update) ([], initialNums) $ drop 25 i
  where
    initialNums = take 25 i
    update (retL, numL) v = (retL ++ [(v, isElem)], newNumL)
      where
        isElem = v `elem` [ a + b | a <- numL, b <- numL]
        newNumL = drop 1 $ numL ++ [v]
                    
getSolution2 :: [Integer] -> Maybe Integer
getSolution2 i = do
  result <- go 0 1
  return $ (uncurry (+)) . (head &&& last) . sort $ result
  where
    fstSol = getSolution1 i
    inputLen = length i
    go  :: Int -> Int -> Maybe [Integer]
    go l r = if r > inputLen then
               Nothing
             else
               case compare fstSol $ sum (take (r - l) $ drop l i) of
                 GT -> go l (r + 1)
                 LT -> go (l + 1) r
                 EQ -> Just $ take (r - l) $ drop l i

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
