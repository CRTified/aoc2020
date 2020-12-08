{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Main where

import Criterion.Main
import Control.Arrow ((***))
import Control.Concurrent
import qualified Control.Monad.State.Lazy as State
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
import Data.Map (Map)
import qualified Data.Map.Strict as Map

data OPCode = Acc Int
            | Jmp Int
            | Nop Int
            deriving (Show)

type ProgramCode = Map Int (OPCode, Int)

type InstrPointer = Int
type Accumulator = Int

data Machine = Machine Accumulator InstrPointer ProgramCode
  deriving Show

opcodeParser :: Parser (OPCode, Int)
opcodeParser = do
  opcode <- accParse <|>
            jmpParse <|>
            nopParse 
  return (opcode, 0)
  where
    posNum :: (Num a, Read a) => Parser a
    posNum = do
      _ <- string "+"
      read <$> many1 digit
    negNum :: (Num a, Read a) => Parser a
    negNum = do
      read <$> string "-" <> many1 digit
    onePTemplate :: String -> (Int -> OPCode) -> Parser OPCode
    onePTemplate x y = do
      _ <- string $ x ++ " "
      param <- posNum <|> negNum
      return $ y param
    accParse = onePTemplate "acc" (\x -> Acc x)
    jmpParse = onePTemplate "jmp" (\x -> Jmp x)
    nopParse = onePTemplate "nop" (\x -> Nop x)
  

-- parseInput :: T.Text -> Machine
parseInput i = Machine 0 0 (Map.fromList $ zip [0..] code)
  where
    code = rights $ map (parse opcodeParser "input") $ lines $ T.unpack i

(//) ::Ord a => Map a b -> [(a, b)] -> Map a b
(//) m l = foldl (\m' (i, v) -> Map.insert i v m') m l

evalOp :: Machine -> OPCode -> Machine
evalOp (Machine acc ip code) (Acc v) = Machine (acc + v) (ip + 1) code
evalOp (Machine acc ip code) (Jmp v) = Machine (acc) (ip + v) code
evalOp (Machine acc ip code) (Nop v) = Machine (acc) (ip + 1) code

fetchOp :: Machine -> Machine
fetchOp s@(Machine acc ip code) = evalOp newMachine op
  where
    newMachine = Machine acc ip (code // [(ip, (op, visit + 1))])
    (op, visit) = case code Map.!? ip of
           Just c -> c
           Nothing -> (Nop 0, 0)

runMachineWhile :: (Machine -> Bool) -> State.State Machine Machine
runMachineWhile p = do
  s <- State.get
  if p s then do
      State.put $ fetchOp s
      runMachineWhile p
    else return s
  
  

--getSolution1 :: BagGraph -> Int
getSolution1 i = acc
  where
    (Machine acc _ _) = State.evalState (runMachineWhile p) i
    p (Machine _ ip' code') = case code' Map.!? ip' of
                                Just (_, visits) -> visits < 1
                                Nothing -> False

--getSolution2 :: BagGraph -> Int
getSolution2 (Machine _ _ basecode) = mapMaybe runSolution possibleMachines
  where
    opSwitch (Nop a) = Jmp a
    opSwitch (Jmp a) = Nop a
    opSwitch a = a
    
    possibleCodes = mapMaybe (\(i, c) -> do
                                 op <- c Map.!? i
                                 case op of
                                   (Nop a, v) -> return $ c // [(i, (Jmp a, v))]
                                   (Jmp a, v) -> return $ c // [(i, (Nop a, v))]
                                   _ -> Nothing
                             ) $ zip [0..] $ replicate (Map.size basecode) basecode 
    possibleMachines = map (Machine 0 0) possibleCodes
    runSolution i' = if ip >= Map.size code then Just acc else Nothing
      where
        Machine acc ip code = State.evalState (runMachineWhile p) i'
        p (Machine _ ip' code') = case code' Map.!? ip' of
                                    Just (_, visits) -> visits < 1
                                    Nothing -> False

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
