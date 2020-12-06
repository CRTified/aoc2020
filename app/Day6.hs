{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Control.Arrow ((***))
import Data.List (groupBy, intercalate, transpose)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Char (isDigit, isHexDigit)
import Lib
import Data.Bits
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set

type Question = Char
type Person = T.Text
type Group = ( Set Question, [Person])



parseInput :: T.Text -> [Group]
parseInput i = map buildGroup $ T.splitOn "\n\n" i
  where
    buildGroup :: T.Text -> Group
    buildGroup s = (questions, persons)
      where
        questions = Set.fromList $ concat $ lines $ T.unpack s
        persons = T.lines s
    
getSolution1 :: [Group] -> Int
getSolution1 i = sum $ map (Set.size . fst) i

getSolution2 :: [Group] -> Int
getSolution2 i = sum $ map validQuestions i
  where
    validQuestions :: Group -> Int
    validQuestions (qs, ps) = Set.size $ Set.filter (\q -> all (\p -> q `elem` T.unpack p) ps) qs

main :: IO ()
main = do
  rawInput <- getContents
  let input = parseInput $ T.pack rawInput
  
  putStrLn "Part 1"
  print $ getSolution1 input

  putStrLn "Part 2"
  print $ getSolution2 input

  putStrLn "Benchmark"
  defaultMain
    [
      bgroup "Day 6" [ bench "Part 1" $ whnf getSolution1 input ]
    , bgroup "Day 6" [ bench "Part 2" $ whnf getSolution2 input ]
    ]
