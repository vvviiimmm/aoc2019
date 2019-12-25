{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State.Strict as State
import Debug.Trace

type Memory = Map.Map Int Int
type Addr = Int
type MWord = Int

data Op =
    Add { a :: Int, b :: Int, dst :: Int } |
    Mult { a :: Int, b :: Int, dst :: Int } |
    Unknown deriving Show


memFetch :: Addr -> State Memory MWord
memFetch addr = gets (fromMaybe 0 . Map.lookup addr)

memStore :: Addr -> MWord -> State Memory ()
memStore addr value = do
    mem <- get
    put $ Map.insert addr value mem
    return ()

runOp :: Op -> State Memory ()
runOp op = case op of
    Add a b dst -> do
        x <- memFetch a
        y <- memFetch b
        memStore dst (x + y)

    Mult a b dst -> do
        x <- memFetch a
        y <- memFetch b
        memStore dst (x * y)

    Unknown -> pure ()

parseCommands :: [Int] -> [Op]
parseCommands (_0:_1:_2:_3:xs) = case _0 of
    1 -> (Add _1 _2 _3) : parseCommands xs
    2 -> (Mult _1 _2 _3) : parseCommands xs
    _ -> []
parseCommands _ = []

runProgram :: [Int] -> Memory -> Memory
runProgram words memory = resultMemory where
    -- memory = Map.fromList $ zip [0 .. ] words
    commands = parseCommands words
    resultMemory = State.execState (traverse runOp commands) memory

patchMemory :: Memory -> [(Int, MWord)] -> Memory
patchMemory mem patch = Map.union (Map.fromList patch) mem 

part1 :: IO ()
part1 = do
    input <- readFile "input.txt"
    let numbers = Prelude.map (\x -> read x :: Int) (splitOn "," input)
    let memory = Map.fromList $ zip [0 .. ] numbers
    let memoryPatched = patchMemory memory [(1, 12), (2, 2)]
    let memoryPatchCombinations = [[(0, 1)]]
    let resultMemory = runProgram numbers memoryPatched
    print resultMemory

part2 :: IO ()
part2 = do
    input <- readFile "input.txt"
    let numbers = Prelude.map (\x -> read x :: Int) (splitOn "," input)
    let memory = Map.fromList $ zip [0 .. ] numbers

    let memoryPatchCombinations = [[(0, x), (1, y)] | x <- [0 .. 1000], y <- [0 .. 1000]]

    let hm = findNounVerb memoryPatchCombinations numbers memory

    print hm

findNounVerb :: [ [(Int, MWord)] ] -> [Int] -> Memory -> [(Int, MWord)]
findNounVerb (patch:xs) commands mem = result where
    memory = patchMemory mem patch
    programResult = fromMaybe 0 $ Map.lookup 0 memory
    result = if (programResult > 19690720) then 
        patch 
    else
        findNounVerb xs commands mem
findNounVerb _ commands mem = []

main :: IO ()
main = do
    -- let !s = State.evalStateT play 0
    -- let s = recFunc 0
    let s = play' 0
    
    putStrLn (show s)
    -- s

---- TEST
step :: State Integer ()
step = modify (+1)

step2 :: State Integer ()
step2 = modify (\s -> s - 1)


recFunc :: Int -> Int
recFunc s = r where
    newValue = (s + 1)
    r = if (newValue < 100000000) then (recFunc newValue) else newValue

play :: StateT Integer IO ()
play = do
    s <- get
    let newValue =  (s + 1)-- trace ("hello" ++ show s)  (s + 1)
    put newValue
    if (newValue < 100000000) then play else pure ()

play' ::  Integer -> Integer
play' !i = if (i < 100000000) then play' newState else i where
    !newState = State.execState theStep i
    !theStep = do
        step
        step2
        step