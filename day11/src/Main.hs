{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import IntExecution
import IntSystem

import Control.Monad.State.Strict as State
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

initSystemForInput :: [MWord] -> [MWord] -> System
initSystemForInput program input = system
  where
    initialMemory = Map.fromList $ zip (fmap Addr [0 ..]) program
    initRegisters = Registers {relativeBase = 0, ip = 0, halt = False, tick = 0}
    system = System initialMemory initRegisters input [] []

main :: IO ()
main = part2

-- Day 11
data Coord = Coord
  { _x :: !Integer
  , _y :: !Integer
  } deriving (Show, Eq, Ord)

data Color
  = Black
  | White
  deriving (Show, Enum)

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

type Field = Map.Map Coord Color

colorAt :: Coord -> Field -> Color
colorAt c f = fromMaybe Black $ Map.lookup c f

data SystemWithScreen = SystemWithScreen
  { _system :: System
  , _field :: Field
  , _cursor :: Coord
  , _cursorDirection :: Direction
  }

systemWithScreenStep :: State SystemWithScreen ()
systemWithScreenStep = do
  sws <- get

  let currentColor = colorAt (_cursor sws) (_field sws)
      terminationPredicate sys =
        length (output sys) == 2 || halt (registers sys)
      -- Color under the cursor is the input to the machine
      provideInput = [fromIntegral (fromEnum currentColor)]
      intMachineWithInput = (_system sws) {input = provideInput, output = []}
      -- Execute the machine until it returns 2 output numbers
      intMachineWithOutput =
        State.execState (runUntil terminationPredicate) intMachineWithInput
      o = output intMachineWithOutput
  if null o
    then pure ()
    else do
      let [colorToPaint, directionToTurn] = o
          currentPosition = _cursor sws
          newDirection =
            turn (_cursorDirection sws) (toEnum (fromInteger directionToTurn))
          newCursorPosition = moveCursor currentPosition newDirection
      paintPixel currentPosition (toEnum (fromInteger colorToPaint))
      modify
        (\s ->
           s
           { _system = intMachineWithOutput
           , _cursor = newCursorPosition
           , _cursorDirection = newDirection
           })
      systemWithScreenStep

paintPixel :: Coord -> Color -> State SystemWithScreen ()
paintPixel coord color = do
  sws <- get
  let field = _field sws
      paintedField = Map.insert coord color field
  modify (\s -> s {_field = paintedField})

-- Utilize enum representation to implement turning as a modulo 4 addition
-- e.g. DUp + CW = DRight, DDown + CCW = DRight
turn :: Direction -> Turn -> Direction
turn d t = toEnum $ mod (fromEnum d + toE t) 4
  where
    toE CW = 1
    toE CCW = -1

moveCursor :: Coord -> Direction -> Coord
moveCursor (Coord x y) DRight = Coord (x + 1) y
moveCursor (Coord x y) DDown = Coord x (y + 1)
moveCursor (Coord x y) DLeft = Coord (x - 1) y
moveCursor (Coord x y) DUp = Coord x (y - 1)

drawScreen :: Field -> [String]
drawScreen f = grouped 101 l' where 
  indices = [Coord x y | y <- [-50 .. 50], x <- [-50 .. 50]]
  l' = fmap (\c -> colorToChar $ colorAt c f)  indices
  
colorToChar :: Color -> Char
colorToChar Black = '.'
colorToChar White = '#'

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
        , _field = Map.empty
        , _cursor = Coord 0 0
        , _cursorDirection = DUp
        }
      result = State.execState systemWithScreenStep initSystemWithScreen
  print (length $ _field result)

part2 :: IO ()
part2 = do
  input <- readFile "input.txt"
  let mwords = Prelude.map (\x -> read x :: Integer) (splitOn "," input)
      initSystem = initSystemForInput mwords []
      initSystemWithScreen =
        SystemWithScreen
        { _system = initSystem
        , _field = Map.singleton (Coord 0 0) White
        , _cursor = Coord 0 0
        , _cursorDirection = DUp
        }
      result = State.execState systemWithScreenStep initSystemWithScreen
  forM_ (drawScreen (_field result)) putStrLn

