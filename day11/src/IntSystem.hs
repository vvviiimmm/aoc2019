module IntSystem where

import qualified Data.Map as Map

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
  , tick :: Integer
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