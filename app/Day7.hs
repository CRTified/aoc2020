{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Main where

import Criterion.Main
import Control.Arrow ((***))
import Control.Concurrent
import Data.List (groupBy, intercalate, transpose)
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


type Bag = String
type BagGraph = DGraph Bag Int


  --lineParser :: Parser (Bag, [(Bag, Int)])
lineParser = let
  bagColor = (many1 letter) <> (string " ") <> (many1 letter)
  bagRule = do
    optional space
    num <- read <$> many1 digit
    space
    color <- bagColor
    string " bag"
    optional $ string "s"
    return $ (num :: Int, color)
  bagEndRule = string "no other bags." >> return []
  in do
  c <- bagColor
  string " bags contain "
  rules <- bagRule `sepBy` (string ",") <|> bagEndRule
  return (c, rules)

parseInput :: T.Text -> BagGraph
parseInput i = fromArcsList $ concat $ map (\(a, xs) -> map (\(w, b) -> Arc a b w) xs) parsedLines
  where
    parsedEdges = map (T.unpack) $ T.lines i
    parsedLines = rights $ map (parse lineParser "stdio") parsedEdges
    

getSolution1 :: BagGraph -> Int
getSolution1 i = (length $ dfsVertices (Data.Graph.DGraph.transpose i) root) - 1
  where
    root = "shiny gold"

getSolution2 :: BagGraph -> Int
getSolution2 i = traverse root
  where
    root = "shiny gold"
    traverse node = foldr (\(Arc a b w) acc -> acc + w + w * traverse b) 0
                    $ outboundingArcs i node

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
