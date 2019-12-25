{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad (when)
import Control.Monad.State as State
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

type Memory = Map.Map Integer Integer

type Addr = Integer

type MWord = Integer

type OpCode = Integer

data System = System
  { memory :: Memory
  , registers :: Registers
  , input :: [MWord]
  , output :: [MWord]
  , logging :: [String]
  } deriving (Show)

data Arg
  = Position Addr
  | Immediate Integer
  deriving (Show, Eq)

data Registers = Registers
  { ip :: MWord -- Instruction pointer
  , relativeBase :: MWord -- Relative addressing mode
  , halt :: Bool -- termination flag
  } deriving (Show)

data Instruction
  = Add { a :: Arg
        , b :: Arg
        , dst :: Integer }
  | Mult { a :: Arg
         , b :: Arg
         , dst :: Integer }
  | Input { dst :: Integer }
  | Output { odst :: Arg }
  | JmpIfTrue { a :: Arg
              , b :: Arg }
  | JmpIfFalse { a :: Arg
               , b :: Arg }
  | LessThan { a :: Arg
             , b :: Arg
             , c :: MWord }
  | Equals { a :: Arg
           , b :: Arg
           , c :: MWord }
  | Terminate
  | Unknown { op :: MWord }
  deriving (Show, Eq)

instructionSize :: Instruction -> Integer
instructionSize Add {} = 4
instructionSize Mult {} = 4
instructionSize Input {} = 2
instructionSize Output {} = 2
instructionSize JmpIfTrue {} = 3
instructionSize JmpIfFalse {} = 3
instructionSize LessThan {} = 4
instructionSize Equals {} = 4
instructionSize Terminate {} = 1
instructionSize Unknown {} = 1

isIpInstruction :: Instruction -> Bool
isIpInstruction JmpIfTrue {} = True
isIpInstruction JmpIfFalse {} = True
isIpInstruction _ = False

memFetch :: Addr -> State System MWord
memFetch addr = do
  s <- get
  (pure . fromMaybe 0 . Map.lookup addr) (memory s)

memStore :: Addr -> MWord -> State System ()
memStore addr value =
  modify (\s -> s {memory = (Map.insert addr value (memory s))})

acceptInput :: Addr -> State System ()
acceptInput dst = do
  s <- get
  let (x:xs) = input s
  memStore dst x
  modify (\s -> s {input = xs})

produceOutput :: MWord -> State System ()
produceOutput value = modify (\s -> s {output = output s ++ [value]})

runInstruction :: Instruction -> State System ()
runInstruction op =
  case op of
    Add (Position a) (Position b) dst -> do
      x <- memFetch a
      y <- memFetch b
      memStore dst (x + y)
    Add (Immediate a) (Position b) dst -> do
      x <- memFetch b
      memStore dst (x + a)
    Add (Position a) (Immediate b) dst -> do
      x <- memFetch a
      memStore dst (x + b)
    Add (Immediate a) (Immediate b) dst -> memStore dst (a + b)
    Mult (Position a) (Position b) dst -> do
      x <- memFetch a
      y <- memFetch b
      memStore dst (x * y)
    Mult (Immediate a) (Position b) dst -> do
      x <- memFetch b
      memStore dst (x * a)
    Mult (Position a) (Immediate b) dst -> do
      x <- memFetch a
      memStore dst (x * b)
    Mult (Immediate a) (Immediate b) dst -> memStore dst (a * b)
    Input addr -> acceptInput addr
    Output (Position addr) -> memFetch addr >>= produceOutput
    Output (Immediate value) -> produceOutput value
    JmpIfTrue (Position a) (Position b) -> do
      x <- memFetch a
      y <- memFetch b
      if x /= 0 then setIp y else addToIp 3
    JmpIfTrue (Immediate a) (Position b) -> do
      y <- memFetch b
      if a /= 0 then setIp y else addToIp 3
    JmpIfTrue (Position a) (Immediate b) -> do
      x <- memFetch a
      if x /= 0 then setIp b else addToIp 3
    JmpIfTrue (Immediate a) (Immediate b) -> if a /= 0 then setIp b else addToIp 3
    JmpIfFalse (Position a) (Position b) -> do
      x <- memFetch a
      y <- memFetch b
      if x == 0 then setIp y else addToIp 3
    JmpIfFalse (Immediate a) (Position b) -> do
      y <- memFetch b
      if a == 0 then setIp y else addToIp 3
    JmpIfFalse (Position a) (Immediate b) -> do
      x <- memFetch a
      if x == 0 then setIp b else addToIp 3
    JmpIfFalse (Immediate a) (Immediate b) -> if a == 0 then setIp b else addToIp 3

    LessThan (Position a) (Position b) dst -> do
      x <- memFetch a
      y <- memFetch b
      if x < y then memStore dst 1 else memStore dst 0
    LessThan (Immediate a) (Position b) dst -> do
      x <- memFetch b
      if a < x then memStore dst 1 else memStore dst 0
    LessThan (Position a) (Immediate b) dst -> do
      x <- memFetch a
      if x < b then memStore dst 1 else memStore dst 0
    LessThan (Immediate a) (Immediate b) dst -> if a < b then memStore dst 1 else memStore dst 0

    Equals (Position a) (Position b) dst -> do
      x <- memFetch a
      y <- memFetch b
      if x == y then memStore dst 1 else memStore dst 0
    Equals (Immediate a) (Position b) dst -> do
      x <- memFetch b
      if a == x then memStore dst 1 else memStore dst 0
    Equals (Position a) (Immediate b) dst -> do
      x <- memFetch a
      if x == b then memStore dst 1 else memStore dst 0
    Equals (Immediate a) (Immediate b) dst -> if a == b then memStore dst 1 else memStore dst 0

    Terminate -> modify (\s -> s {registers = (registers s) {halt = True}})
    Unknown _ -> pure ()

-- Lenses here!
setIp :: MWord -> State System ()
setIp newIp = modify (\s -> s {registers = (registers s) {ip = newIp}})

addToIp :: MWord -> State System ()
addToIp offset = modify (\s -> s {registers = (registers s) {ip = ip (registers s) + offset}})

runInstructionLogged :: Instruction -> State System ()
runInstructionLogged instruction = do
  modify (\s -> s {logging = logging s ++ [show instruction]})
  runInstruction instruction

parseInstructions :: [OpCode] -> Instruction
parseInstructions (1:a:b:c:xs) = Add (Position a) (Position b) c
parseInstructions (101:a:b:c:xs) = Add (Immediate a) (Position b) c
parseInstructions (1001:a:b:c:xs) = Add (Position a) (Immediate b) c
parseInstructions (1101:a:b:c:xs) = Add (Immediate a) (Immediate b) c
parseInstructions (2:a:b:c:xs) = Mult (Position a) (Position b) c
parseInstructions (102:a:b:c:xs) = Mult (Immediate a) (Position b) c
parseInstructions (1002:a:b:c:xs) = Mult (Position a) (Immediate b) c
parseInstructions (1102:a:b:c:xs) = Mult (Immediate a) (Immediate b) c
parseInstructions (3:a:xs) = Input a
parseInstructions (4:a:xs) = Output (Position a)
parseInstructions (104:a:xs) = Output (Immediate a)
parseInstructions (5:a:b:xs) = JmpIfTrue (Position a) (Position b)
parseInstructions (105:a:b:xs) = JmpIfTrue (Immediate a) (Position b)
parseInstructions (1005:a:b:xs) = JmpIfTrue (Position a) (Immediate b)
parseInstructions (1105:a:b:xs) = JmpIfTrue (Immediate a) (Immediate b)
parseInstructions (6:a:b:xs) = JmpIfFalse (Position a) (Position b)
parseInstructions (106:a:b:xs) = JmpIfFalse (Immediate a) (Position b)
parseInstructions (1006:a:b:xs) = JmpIfFalse (Position a) (Immediate b)
parseInstructions (1106:a:b:xs) = JmpIfFalse (Immediate a) (Immediate b)
parseInstructions (7:a:b:c:xs) = LessThan (Position a) (Position b) c
parseInstructions (107:a:b:c:xs) = LessThan (Immediate a) (Position b) c
parseInstructions (1007:a:b:c:xs) = LessThan (Position a) (Immediate b) c
parseInstructions (1107:a:b:c:xs) = LessThan (Immediate a) (Immediate b) c
parseInstructions (8:a:b:c:xs) = Equals (Position a) (Position b) c
parseInstructions (108:a:b:c:xs) = Equals (Immediate a) (Position b) c
parseInstructions (1008:a:b:c:xs) = Equals (Position a) (Immediate b) c
parseInstructions (1108:a:b:c:xs) = Equals (Immediate a) (Immediate b) c
parseInstructions (99:xs) = Terminate
parseInstructions (x:xs) = Unknown x
parseInstructions [] = Unknown (-1)

fetchNextOp :: State System [OpCode]
fetchNextOp = do
  s <- get
  let p = ip (registers s)
  mapM memFetch [p, p + 1, p + 2, p + 3]

step :: State System ()
step = do
  nextOp <- fetchNextOp
  let inst = parseInstructions nextOp
  -- runInstructionLogged (trace (show inst) inst)
  runInstructionLogged inst
  s <- get
    -- rewrite with lenses
  let is = instructionSize inst
      newIp = if isIpInstruction inst then ip (registers s) else ip (registers s) + is
  modify
    (\s ->
       s {registers = (registers s) {halt = inst == Terminate, ip = newIp}})

runUntilTermination :: State System ()
runUntilTermination = do
  currentState <- get
  if halt (registers currentState)
    then pure ()
    else do
      step
      runUntilTermination

part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  let mwords = Prelude.map (\x -> read x :: Integer) (splitOn "," input)
  let initialMemory = Map.fromList $ zip [0 ..] mwords
  let systemInputs = [5]
  let initRegisters = Registers {relativeBase = 0, ip = 0, halt = False}
  let initSystem = System initialMemory initRegisters systemInputs [] []
  let syst = State.execState runUntilTermination initSystem
  putStrLn "Logs: "
  forM_ (logging syst) putStrLn
  putStr "System output: "
  print (output syst)

main :: IO ()
main = pure ()
