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

type Passport = HM.HashMap T.Text T.Text



parseInput :: T.Text -> [Passport]
parseInput i = map (HM.fromList . map ((id *** T.tail) . (T.break (== ':')))) passportStrings
  where
    passportStrings = map T.words $ T.splitOn "\n\n" i
    
getSolution1 :: [Passport] -> Int
getSolution1 b = length $ filter (\x -> all (\f -> f `HM.member` x) requiredFieldList) b
  where
    requiredFieldList = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ] :: [T.Text]
    
getSolution2 b = length $ filter (\x ->
                                    all (\(f, c) ->
                                           (f `HM.member` x)
                                           &&
                                           (any c $ HM.lookup f x)
                                        )
                                    requiredFieldList
                                 ) b
  where
    heightCheck x = checkRange $ T.span (isDigit) x
      where
        checkRange (x, "cm") = (read $ T.unpack x) >= 150 && (read $ T.unpack x) <= 193
        checkRange (x, "in") = (read $ T.unpack x) >= 59 && (read $ T.unpack x) <= 76
        checkRange (_, _) = False

    hairCheck ('#':xs) = (length xs == 6) && all (isHexDigit) (xs)
    hairCheck _ = False
      
    requiredFieldList = [ ("byr", \x -> (read $ T.unpack x) >= 1920 && (read $ T.unpack x) <= 2002)
                        , ("iyr", \x -> (read $ T.unpack x) >= 2010 && (read $ T.unpack x) <= 2020)
                        , ("eyr", \x -> (read $ T.unpack x) >= 2020 && (read $ T.unpack x) <= 2030)
                        , ("hgt", heightCheck)
                        , ("hcl", hairCheck . T.unpack)
                        , ("ecl", \x -> x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
                        , ("pid", \x -> 9 == (length $ filter (isDigit) $ T.unpack x))
                        ] :: [(T.Text, T.Text -> Bool)]

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
      bgroup "Day 4" [ bench "Part 1" $ whnf getSolution1 input ]
    , bgroup "Day 4" [ bench "Part 2" $ whnf getSolution2 input ]
    ]
