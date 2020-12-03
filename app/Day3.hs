module Main where

import Criterion.Main
import Control.Arrow ((***))
import Data.List (groupBy, intercalate, transpose)
import Data.Maybe (mapMaybe, catMaybes)
import Lib
import Data.Bits

data Field = Empty | Tree deriving Eq
data Biome = Biome [[ Field ]]

readField :: Char -> Field
readField '#' = Tree
readField _ = Empty

cycleBiome :: Biome -> Biome
cycleBiome (Biome xs) = Biome $ cycle xs

traverseBiome :: Biome -> (Int, Int) -> [Field]
traverseBiome (Biome xs) (r, d) = go 0 0
  where
    fetch x y = do
      column <- xs !!? x
      column !!? y
    go x y = case fetch x y of
      Just f -> f : (go (x + r) (y + d))
      Nothing -> []

parseInput :: String -> Biome
parseInput i = Biome $ transpose $ map (map readField) $ lines i

countTrees :: Biome -> (Int, Int) -> Int
countTrees b = length . (filter (== Tree)) . traverseBiome (cycleBiome b)

getSolution1 b = countTrees b (3, 1)
getSolution2 b = foldr (\v acc -> acc * (countTrees b v)) 1 slopeList
  where
    slopeList = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
  rawInput <- getContents
  let input = parseInput rawInput
  
  putStrLn "Part 1"
  print $ getSolution1 input

  putStrLn "Part 2"
  print $ getSolution2 input

  putStrLn "Benchmark"
  defaultMain
    [
      bgroup "Day 2" [ bench "Part 1" $ whnf getSolution1 input ]
    , bgroup "Day 2" [ bench "Part 2" $ whnf getSolution2 input ]
    ]
