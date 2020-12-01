module Main where

import Criterion.Main
import Lib

chooseN :: Int -> [a] -> [[a]]
chooseN 0 _ = []
chooseN _ [] = []
chooseN 1 (x:xs) = [x] : chooseN 1 xs
chooseN n (x:xs) = (map (x:) $ chooseN (n-1) xs) ++ chooseN n xs

sum2020 :: (Num a, Eq a) => [[a]] -> [[a]]
sum2020 = filter (\x -> sum x == 2020)

getSolution :: (Num a, Eq a) => Int -> [a] -> [([a], a)]
getSolution i xs = map (\ys -> (ys, foldr (*) 1 ys)) filtered
  where
    filtered = sum2020 $ chooseN i xs

getSolution1 = getSolution 2
getSolution2 = getSolution 3

    
main :: IO ()
main = do
  rawInput <- getContents
  let input = map (read :: String -> Integer) $ lines rawInput
  
  putStrLn "Part 1"
  mapM_ (putStrLn . show) $ getSolution1 input

  putStrLn "Part 2"
  mapM_ (putStrLn . show) $ getSolution2 input

  putStrLn "Benchmark"
  defaultMain
    [
      bgroup "Part 1" [ bench "1" $ whnf getSolution1 input ]
    , bgroup "Part 2" [ bench "2" $ whnf getSolution2 input ]
    ]
