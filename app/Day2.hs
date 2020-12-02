module Main where

import Criterion.Main
import Control.Arrow ((***))
import Data.List (groupBy)
import Data.Maybe (mapMaybe)
import Lib
import Data.Bits

data Policy = Policy Int Int Char deriving (Show, Eq)

checkPolicy1 :: Policy -> String -> Bool
checkPolicy1 (Policy min max ch) s = c >= min && c <= max
  where
    c = length $ filter (== ch) s

checkPolicy2 :: Policy -> String -> Bool
checkPolicy2 (Policy p1 p2 ch) s = pred1 `xor` pred2
  where
    pred1 = maybe False ((==) ch) (s !!? (p1 - 1))
    pred2 = maybe False ((==) ch) (s !!? (p2 - 1))


getSolution1 x = [length $ filter (uncurry checkPolicy1) x]
getSolution2 x = [length $ filter (uncurry checkPolicy2) x]

parseInput :: String -> [(Policy, String)]
parseInput i = mapMaybe (parseLine) $ lines i
  where
    parseLine s = do
      let splitted = groupBy (\_ a -> not $ a `elem` "- :") s
      minChar  <- splitted !!? 0 
      maxChar  <- splitted !!? 1
      ch       <- splitted !!? 2
      password <- splitted !!? 4
      return ( Policy
               (read minChar)
               (read $ drop 1 maxChar)
               (last ch)
             , drop 1 password)
        
               
main :: IO ()
main = do
  rawInput <- getContents
  let input = parseInput rawInput

  putStrLn "Part 1"
  mapM_ (putStrLn . show) $ getSolution1 input

  putStrLn "Part 2"
  mapM_ (putStrLn . show) $ getSolution2 input

  putStrLn "Benchmark"
  defaultMain
    [
      bgroup "Day 2" [ bench "Part 1" $ whnf getSolution1 input ]
    , bgroup "Day 2" [ bench "Part 2" $ whnf getSolution2 input ]
    ]
