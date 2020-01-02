{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import IntExecution
import IntSystem

import Control.Monad.State.Strict as State
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Control.Lens

initSystemForInput :: [MWord] -> [MWord] -> System
initSystemForInput program input = system
  where
    initialMemory = Map.fromList $ zip (fmap Addr [0 ..]) program
    initRegisters = Registers {_relativeBase = 0, _ip = 0, _halt = False, _tick = 0}
    system = System initialMemory initRegisters input [] []

-- Day 13
data Coord = Coord
  { _x :: !Integer
  , _y :: !Integer
  } deriving (Show, Eq, Ord)

data Tile
  = EmptyTile
  | Wall 
  | Block
  | Paddle
  | Ball deriving (Show, Enum,Eq)

data Direction
  = DUp
  | DRight
  | DDown
  | DLeft
  deriving (Show, Enum)

data Turn
  = CCW
  | CW
  deriving (Show, Enum)

type Field = Map.Map Coord Tile

data SystemWithScreen = SystemWithScreen
  { _system :: System
  , _field :: Field
  , _cursor :: Coord
  , _cursorDirection :: Direction
  }

makeLenses ''SystemWithScreen

systemWithScreenStep :: State SystemWithScreen ()
systemWithScreenStep = do
  sws <- get

  let terminationPredicate sys = length (sys^.output) == 3 || sys^.registers.halt
      intMachineWithOutput = State.execState (runUntil terminationPredicate) (sws^.system)
  if null (intMachineWithOutput^.output)
    then pure ()
    else do
      let [tx, ty, tid] = intMachineWithOutput^.output
      paintPixel (Coord tx ty) (toEnum (fromInteger tid))
      system .=  (output .~ []) intMachineWithOutput
      
      systemWithScreenStep

paintPixel :: Coord -> Tile -> State SystemWithScreen ()
paintPixel coord tile = field %= Map.insert coord tile

drawScreen :: Field -> [String]
drawScreen f = grouped 101 l' where 
  indices = [Coord x y | y <- [-50 .. 50], x <- [-50 .. 50]]
  l' = fmap (\c -> colorToChar $ colorAt c f)  indices
  
colorToChar :: Tile -> Char
colorToChar EmptyTile = ' '
colorToChar Wall = '|'
colorToChar Block = '-'
colorToChar Paddle = '_'
colorToChar Ball = 'o'

colorAt :: Coord -> Field -> Tile
colorAt c f = fromMaybe EmptyTile $ Map.lookup c f

grouped :: Int -> [a] -> [[a]]
grouped 1 xs = [xs]
grouped _ [] = []
grouped n xs = take n xs : grouped n (drop n xs)


part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  let mwords = Prelude.map (\x -> read x :: Integer) (splitOn "," input)
      initSystem = initSystemForInput mwords []
      initSystemWithScreen =
        SystemWithScreen
        { _system = initSystem
        , _field = Map.singleton (Coord 0 0) EmptyTile
        , _cursor = Coord 0 0
        , _cursorDirection = DUp
        }
      result = State.execState systemWithScreenStep initSystemWithScreen
  print (length $ Map.filter (== Block) (result^.field) )
  -- forM_ (drawScreen (result^.field)) putStrLn

main :: IO ()
main = part1