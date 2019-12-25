{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad (when)
import Control.Monad.State as State
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.List as List
import Debug.Trace

type MWord = Integer

type Memory = Map.Map Addr MWord

newtype Addr = Addr { unAddr :: MWord } deriving (Eq, Ord)

instance Show Addr where
  show (Addr x) = "Addr " ++ show x

newtype RelAddr = RelAddr { unRelAddr :: MWord }  deriving (Eq)

instance Show RelAddr where
  show (RelAddr x) = "RelAddr " ++ show x

newtype OpCode = OpCode { unOpCode :: MWord } deriving (Eq, Show)

data System = System
  { memory :: Memory
  , registers :: Registers
  , input :: [MWord]
  , output :: [MWord]
  , logging :: [String]
  } deriving (Show)

data Arg
  = Position Addr
  | Immediate MWord
  | Relative RelAddr
  deriving (Show, Eq)

data Registers = Registers
  { ip :: MWord -- Instruction pointer
  , relativeBase :: MWord -- Relative addressing mode
  , halt :: Bool -- termination flag
  } deriving (Show)

data Instruction
  = Add { a :: Arg
        , b :: Arg
        , dst :: Arg }
  | Mult { a :: Arg
         , b :: Arg
         , dst :: Arg }
  | Input { odst :: Arg }
  | Output { odst :: Arg }
  | JmpIfTrue { a :: Arg
              , b :: Arg }
  | JmpIfFalse { a :: Arg
               , b :: Arg }
  | LessThan { a :: Arg
             , b :: Arg
             , c :: Arg }
  | Equals { a :: Arg
           , b :: Arg
           , c :: Arg }
  | ModifyRelBase { a :: Arg}
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
instructionSize ModifyRelBase {} = 2
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

memFetchRelative :: RelAddr -> State System MWord
memFetchRelative relAddr = do
  addr <- resolveAddr relAddr
  memFetch addr
  
memStore :: Addr -> MWord -> State System ()
memStore addr value =
  modify (\s -> s {memory = (Map.insert addr value (memory s))})

memStoreRelative :: RelAddr -> MWord -> State System ()
memStoreRelative relAddr value = do
  addr <- resolveAddr relAddr
  memStore addr value

resolveAddr :: RelAddr -> State System Addr
resolveAddr addr = do
  s <- get
  let base = relativeBase (registers s)
      actualAddr = Addr (base + (unRelAddr addr))
  pure actualAddr


acceptInput :: Addr -> State System ()
acceptInput dst = do
  s <- get
  let (x:xs) = input s
  memStore dst x
  modify (\s -> s {input = xs})

acceptInputRelative :: RelAddr -> State System ()
acceptInputRelative addr = do
  actualAddr <- resolveAddr addr
  acceptInput actualAddr

produceOutput :: MWord -> State System ()
produceOutput value = modify (\s -> s {output = output s ++ [value]})

runInstruction :: Instruction -> State System ()
runInstruction op =
  case op of
    Add (Position a) (Position b) (Position dst) -> do
      x <- memFetch a
      y <- memFetch b
      memStore dst (x + y)
    Add (Immediate a) (Position b) (Position dst) -> do
      x <- memFetch b
      memStore dst (x + a)
    Add (Position a) (Immediate b) (Position dst) -> do
      x <- memFetch a
      memStore dst (x + b)
    Add (Immediate a) (Immediate b) (Position dst) -> memStore dst (a + b)

    Add (Position a) (Relative b) (Position dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      memStore dst (x + y)
    Add (Relative a) (Position b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      memStore dst (x + y)
    Add (Immediate a) (Relative b) (Position dst) -> do
      y <- memFetchRelative b
      memStore dst (a + y)
    Add (Relative a) (Immediate b) (Position dst) -> do
      x <- memFetchRelative a
      memStore dst (x + b)
    Add (Relative a) (Relative b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      memStore dst (x + y)

    -- Rel
    Add (Position a) (Position b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetch b
      memStoreRelative dst (x + y)
    Add (Immediate a) (Position b) (Relative dst) -> do
      x <- memFetch b
      memStoreRelative dst (x + a)
    Add (Position a) (Immediate b) (Relative dst) -> do
      x <- memFetch a
      memStoreRelative dst (x + b)
    Add (Immediate a) (Immediate b) (Relative dst) -> memStoreRelative dst (a + b)

    Add (Position a) (Relative b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      memStoreRelative dst (x + y)
    Add (Relative a) (Position b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      memStoreRelative dst (x + y)
    Add (Immediate a) (Relative b) (Relative dst) -> do
      y <- memFetchRelative b
      memStoreRelative dst (a + y)
    Add (Relative a) (Immediate b) (Relative dst) -> do
      x <- memFetchRelative a
      memStoreRelative dst (x + b)
    Add (Relative a) (Relative b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      memStoreRelative dst (x + y)
    --


    Mult (Position a) (Position b) (Position dst) -> do
      x <- memFetch a
      y <- memFetch b
      memStore dst (x * y)
    Mult (Immediate a) (Position b) (Position dst) -> do
      x <- memFetch b
      memStore dst (x * a)
    Mult (Position a) (Immediate b) (Position dst) -> do
      x <- memFetch a
      memStore dst (x * b)
    Mult (Immediate a) (Immediate b) (Position dst) -> memStore dst (a * b)

    Mult (Position a) (Relative b) (Position dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      memStore dst (x * y)
    Mult (Relative a) (Position b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      memStore dst (x * y)
    Mult (Immediate a) (Relative b) (Position dst) -> do
      y <- memFetchRelative b
      memStore dst (a * y)
    Mult (Relative a) (Immediate b) (Position dst) -> do
      x <- memFetchRelative a
      memStore dst (x * b)
    Mult (Relative a) (Relative b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      memStore dst (x * y)

    -- Rel
    Mult (Position a) (Position b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetch b
      memStoreRelative dst (x * y)
    Mult (Immediate a) (Position b) (Relative dst) -> do
      x <- memFetch b
      memStoreRelative dst (x * a)
    Mult (Position a) (Immediate b) (Relative dst) -> do
      x <- memFetch a
      memStoreRelative dst (x * b)
    Mult (Immediate a) (Immediate b) (Relative dst) -> memStoreRelative dst (a * b)

    Mult (Position a) (Relative b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      memStoreRelative dst (x * y)
    Mult (Relative a) (Position b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      memStoreRelative dst (x * y)
    Mult (Immediate a) (Relative b) (Relative dst) -> do
      y <- memFetchRelative b
      memStoreRelative dst (a * y)
    Mult (Relative a) (Immediate b) (Relative dst) -> do
      x <- memFetchRelative a
      memStoreRelative dst (x * b)
    Mult (Relative a) (Relative b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      memStoreRelative dst (x * y)

    Input (Position addr) -> acceptInput addr
    Input (Relative addr) -> acceptInputRelative addr

    Output (Position addr) -> memFetch addr >>= produceOutput
    Output (Immediate value) -> produceOutput value
    Output (Relative addr) -> memFetchRelative addr >>= produceOutput

    JmpIfTrue (Position a) (Position b) -> do
      x <- memFetch a
      y <- memFetch b
      if x /= 0
        then setIp y
        else addToIp 3
    JmpIfTrue (Immediate a) (Position b) -> do
      y <- memFetch b
      if a /= 0
        then setIp y
        else addToIp 3
    JmpIfTrue (Position a) (Immediate b) -> do
      x <- memFetch a
      if x /= 0
        then setIp b
        else addToIp 3
    JmpIfTrue (Immediate a) (Immediate b) ->
      if a /= 0
        then setIp b
        else addToIp 3

    JmpIfTrue (Position a) (Relative b) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x /= 0
        then setIp y
        else addToIp 3
    JmpIfTrue (Relative a) (Position b) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x /= 0
        then setIp y
        else addToIp 3
    JmpIfTrue (Immediate a) (Relative b) -> do
      y <- memFetchRelative b
      if a /= 0
        then setIp y
        else addToIp 3
    JmpIfTrue (Relative a) (Immediate b) -> do
      x <- memFetchRelative a
      if x /= 0
        then setIp b
        else addToIp 3
    JmpIfTrue (Relative a) (Relative b) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x /= 0
        then setIp y
        else addToIp 3

    JmpIfFalse (Position a) (Position b) -> do
      x <- memFetch a
      y <- memFetch b
      if x == 0
        then setIp y
        else addToIp 3
    JmpIfFalse (Immediate a) (Position b) -> do
      y <- memFetch b
      if a == 0
        then setIp y
        else addToIp 3
    JmpIfFalse (Position a) (Immediate b) -> do
      x <- memFetch a
      if x == 0
        then setIp b
        else addToIp 3
    JmpIfFalse (Immediate a) (Immediate b) ->
      if a == 0
        then setIp b
        else addToIp 3

    JmpIfFalse (Position a) (Relative b) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x == 0
        then setIp y
        else addToIp 3
    JmpIfFalse (Relative a) (Position b) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x == 0
        then setIp y
        else addToIp 3
    JmpIfFalse (Immediate a) (Relative b) -> do
      y <- memFetchRelative b
      if a == 0
        then setIp y
        else addToIp 3
    JmpIfFalse (Relative a) (Immediate b) -> do 
      x <- memFetchRelative a
      if x == 0
        then setIp b
        else addToIp 3
    JmpIfFalse (Relative a) (Relative b) -> do 
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x == 0
        then setIp y
        else addToIp 3



    LessThan (Position a) (Position b) (Position dst) -> do
      x <- memFetch a
      y <- memFetch b
      if x < y
        then memStore dst 1
        else memStore dst 0
    LessThan (Immediate a) (Position b) (Position dst) -> do
      x <- memFetch b
      if a < x
        then memStore dst 1
        else memStore dst 0
    LessThan (Position a) (Immediate b) (Position dst) -> do
      x <- memFetch a
      if x < b
        then memStore dst 1
        else memStore dst 0
    LessThan (Immediate a) (Immediate b) (Position dst) ->
      if a < b
        then memStore dst 1
        else memStore dst 0

    LessThan (Position a) (Relative b) (Position dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x < y
        then memStore dst 1
        else memStore dst 0
    LessThan (Relative a) (Position b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x < y
        then memStore dst 1
        else memStore dst 0
    LessThan (Immediate a) (Relative b) (Position dst) -> do
      y <- memFetchRelative b
      if a < y
        then memStore dst 1
        else memStore dst 0
    LessThan (Relative a) (Immediate b) (Position dst) -> do 
      x <- memFetchRelative a
      if x < b
        then memStore dst 1
        else memStore dst 0
    LessThan (Relative a) (Relative b) (Position dst) -> do 
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x < y
        then memStore dst 1
        else memStore dst 0

    -- Rel
    LessThan (Position a) (Position b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetch b
      if x < y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Immediate a) (Position b) (Relative dst) -> do
      x <- memFetch b
      if a < x
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Position a) (Immediate b) (Relative dst) -> do
      x <- memFetch a
      if x < b
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Immediate a) (Immediate b) (Relative dst) ->
      if a < b
        then memStoreRelative dst 1
        else memStoreRelative dst 0

    LessThan (Position a) (Relative b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x < y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Relative a) (Position b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x < y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Immediate a) (Relative b) (Relative dst) -> do
      y <- memFetchRelative b
      if a < y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Relative a) (Immediate b) (Relative dst) -> do 
      x <- memFetchRelative a
      if x < b
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Relative a) (Relative b) (Relative dst) -> do 
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x < y
        then memStoreRelative dst 1
        else memStoreRelative dst 0



    Equals (Position a) (Position b) (Position dst) -> do
      x <- memFetch a
      y <- memFetch b
      if x == y
        then memStore dst 1
        else memStore dst 0
    Equals (Immediate a) (Position b) (Position dst) -> do
      x <- memFetch b
      if a == x
        then memStore dst 1
        else memStore dst 0
    Equals (Position a) (Immediate b) (Position dst) -> do
      x <- memFetch a
      if x == b
        then memStore dst 1
        else memStore dst 0
    Equals (Immediate a) (Immediate b) (Position dst) ->
      if a == b
        then memStore dst 1
        else memStore dst 0


    
    Equals (Position a) (Relative b) (Position dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x == y
        then memStore dst 1
        else memStore dst 0
    Equals (Relative a) (Position b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x == y
        then memStore dst 1
        else memStore dst 0
    Equals (Immediate a) (Relative b) (Position dst) -> do
      y <- memFetchRelative b
      if a == y
        then memStore dst 1
        else memStore dst 0
    Equals (Relative a) (Immediate b) (Position dst) -> do
      x <- memFetchRelative a
      if x == b
        then memStore dst 1
        else memStore dst 0
    Equals (Relative a) (Relative b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x == y
        then memStore dst 1
        else memStore dst 0

    -- Rel
    Equals (Position a) (Position b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetch b
      if x == y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Immediate a) (Position b) (Relative dst) -> do
      x <- memFetch b
      if a == x
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Position a) (Immediate b) (Relative dst) -> do
      x <- memFetch a
      if x == b
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Immediate a) (Immediate b) (Relative dst) ->
      if a == b
        then memStoreRelative dst 1
        else memStoreRelative dst 0


    
    Equals (Position a) (Relative b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x == y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Relative a) (Position b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x == y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Immediate a) (Relative b) (Relative dst) -> do
      y <- memFetchRelative b
      if a == y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Relative a) (Immediate b) (Relative dst) -> do
      x <- memFetchRelative a
      if x == b
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Relative a) (Relative b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x == y
        then memStoreRelative dst 1
        else memStoreRelative dst 0


    ModifyRelBase (Immediate a) -> addToRelativeBase a
    ModifyRelBase (Position a) -> memFetch a >>= addToRelativeBase
    ModifyRelBase (Relative a) -> memFetchRelative a >>= addToRelativeBase
    Terminate -> modify (\s -> s {registers = (registers s) {halt = True}})
    Unknown x -> pure ()

-- Lenses here!
setIp :: MWord -> State System ()
setIp newIp = modify (\s -> s {registers = (registers s) {ip = newIp}})

addToIp :: MWord -> State System ()
addToIp offset =
  modify (\s -> s {registers = (registers s) {ip = ip (registers s) + offset}})

addToRelativeBase :: MWord -> State System ()
addToRelativeBase offset =
  modify (\s -> s {registers = (registers s) {relativeBase = relativeBase (registers s) + offset}})

runInstructionLogged :: Instruction -> State System ()
runInstructionLogged instruction = do
  modify (\s -> s {logging = logging s ++ [show instruction]})
  runInstruction instruction

parseInstructions :: [OpCode] -> Instruction
parseInstructions (OpCode 1:a:b:c:xs) = Add (Position (toAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1001:a:b:c:xs) = Add (Position (toAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 1101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Position (toAddr c))

parseInstructions (OpCode 201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2001:a:b:c:xs) = Add (Position (toAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 2101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))


parseInstructions (OpCode 2:a:b:c:xs) = Mult (Position (toAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1002:a:b:c:xs) = Mult (Position (toAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 1102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Position (toAddr c))

parseInstructions (OpCode 202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2002:a:b:c:xs) = Mult (Position (toAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 2102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))

-- Relative destination
parseInstructions (OpCode 20001:a:b:c:xs) = Add (Position (toAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 20101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21001:a:b:c:xs) = Add (Position (toAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))

parseInstructions (OpCode 20201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22001:a:b:c:xs) = Add (Position (toAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))


parseInstructions (OpCode 20002:a:b:c:xs) = Mult (Position (toAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 20102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21002:a:b:c:xs) = Mult (Position (toAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))

parseInstructions (OpCode 20202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22002:a:b:c:xs) = Mult (Position (toAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
---

parseInstructions (OpCode 3:a:xs) = Input (Position (toAddr a))
parseInstructions (OpCode 203:a:xs) = Input (Relative (toRelAddr a))

parseInstructions (OpCode 4:a:xs) = Output (Position (toAddr a))
parseInstructions (OpCode 104:a:xs) = Output (Immediate (unOpCode a))
parseInstructions (OpCode 204:a:xs) = Output (Relative (toRelAddr a))

parseInstructions (OpCode 5:a:b:xs) = JmpIfTrue (Position (toAddr a)) (Position (toAddr b))
parseInstructions (OpCode 105:a:b:xs) = JmpIfTrue (Immediate (unOpCode a)) (Position (toAddr b))
parseInstructions (OpCode 1005:a:b:xs) = JmpIfTrue (Position (toAddr a)) (Immediate (unOpCode b))
parseInstructions (OpCode 1105:a:b:xs) = JmpIfTrue (Immediate (unOpCode a)) (Immediate (unOpCode b))

parseInstructions (OpCode 205:a:b:xs) = JmpIfTrue (Relative (toRelAddr a)) (Position (toAddr b))
parseInstructions (OpCode 2005:a:b:xs) = JmpIfTrue (Position (toAddr a)) (Relative (toRelAddr b))
parseInstructions (OpCode 1205:a:b:xs) = JmpIfTrue (Relative (toRelAddr a)) (Immediate (unOpCode b))
parseInstructions (OpCode 2105:a:b:xs) = JmpIfTrue (Immediate (unOpCode a)) (Relative (toRelAddr b))
parseInstructions (OpCode 2205:a:b:xs) = JmpIfTrue (Relative (toRelAddr a)) (Relative (toRelAddr b))


parseInstructions (OpCode 6:a:b:xs) = JmpIfFalse (Position (toAddr a)) (Position (toAddr b))
parseInstructions (OpCode 106:a:b:xs) = JmpIfFalse (Immediate (unOpCode a)) (Position (toAddr b))
parseInstructions (OpCode 1006:a:b:xs) = JmpIfFalse (Position (toAddr a)) (Immediate (unOpCode b))
parseInstructions (OpCode 1106:a:b:xs) = JmpIfFalse (Immediate (unOpCode a)) (Immediate (unOpCode b))

parseInstructions (OpCode 206:a:b:xs) = JmpIfFalse (Relative (toRelAddr a)) (Position (toAddr b))
parseInstructions (OpCode 2006:a:b:xs) = JmpIfFalse (Position (toAddr a)) (Relative (toRelAddr b))
parseInstructions (OpCode 1206:a:b:xs) = JmpIfFalse (Relative (toRelAddr a)) (Immediate (unOpCode b))
parseInstructions (OpCode 2106:a:b:xs) = JmpIfFalse (Immediate (unOpCode a)) (Immediate (unOpCode b))
parseInstructions (OpCode 2206:a:b:xs) = JmpIfFalse (Relative (toRelAddr a)) (Relative (toRelAddr b))

parseInstructions (OpCode 7:a:b:c:xs) = LessThan (Position (toAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1007:a:b:c:xs) = LessThan (Position (toAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 1107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Position (toAddr c))

parseInstructions (OpCode 207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2007:a:b:c:xs) = LessThan (Position (toAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 2107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))

parseInstructions (OpCode 8:a:b:c:xs) = Equals (Position (toAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1008:a:b:c:xs) = Equals (Position (toAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 1108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Position (toAddr c))

parseInstructions (OpCode 208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2008:a:b:c:xs) = Equals (Position (toAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 2108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))

-- Relative destination
parseInstructions (OpCode 20007:a:b:c:xs) = LessThan (Position (toAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 20107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21007:a:b:c:xs) = LessThan (Position (toAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))

parseInstructions (OpCode 20207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22007:a:b:c:xs) = LessThan (Position (toAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))

parseInstructions (OpCode 20008:a:b:c:xs) = Equals (Position (toAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 20108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21008:a:b:c:xs) = Equals (Position (toAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))

parseInstructions (OpCode 20208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22008:a:b:c:xs) = Equals (Position (toAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
---

parseInstructions (OpCode 9:a:xs) = ModifyRelBase (Position (toAddr a))
parseInstructions (OpCode 109:a:xs) = ModifyRelBase (Immediate (unOpCode a))
parseInstructions (OpCode 209:a:xs) = ModifyRelBase (Relative (toRelAddr a))

parseInstructions (OpCode 99:xs) = Terminate
parseInstructions (OpCode x:xs) = Unknown x
parseInstructions [] = Unknown (-1)

toAddr :: OpCode -> Addr
toAddr = Addr . unOpCode

toRelAddr :: OpCode -> RelAddr
toRelAddr = RelAddr . unOpCode

fetchNextOp :: State System [OpCode]
fetchNextOp = do
  s <- get
  let p = ip (registers s)
      offsets = [Addr p, Addr (p + 1), Addr (p + 2), Addr (p + 3)] 
  words <- mapM memFetch offsets :: State System [MWord]
  pure $ fmap OpCode words  

step :: State System ()
step = do
  nextOp <- fetchNextOp
  let inst = parseInstructions nextOp
  -- runInstructionLogged (trace (show inst) inst)
  runInstructionLogged inst
  s <- get
    -- rewrite with lenses
  let is = instructionSize inst
      newIp =
        if isIpInstruction inst
          then ip (registers s)
          else ip (registers s) + is
  modify
    (\s -> s {registers = (registers s) {halt = inst == Terminate, ip = newIp}})

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
      initSystem = initSystemForInput mwords [1]
      result = State.execState runUntilTermination initSystem
  print (logging result)
  print (output result)

initSystemForInput :: [MWord] -> [MWord] -> System
initSystemForInput program input = system
  where
    initialMemory = Map.fromList $ zip (fmap Addr [0 ..]) program
    initRegisters = Registers {relativeBase = 0, ip = 0, halt = False}
    system = System initialMemory initRegisters input [] []

runAmplifier :: [MWord] -> MWord -> MWord -> MWord
runAmplifier program previousOutput phaseSetting = out
  where
    initialMemory = Map.fromList $ zip (fmap Addr [0 ..]) program
    initRegisters = Registers {relativeBase = 0, ip = 0, halt = False}
    initSystem = System initialMemory initRegisters [phaseSetting, previousOutput] [] []
    resultSystem = State.execState runUntilTermination initSystem
    out = head $ output resultSystem

main :: IO ()
main = pure ()
