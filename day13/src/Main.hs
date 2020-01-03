{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)

import qualified Data.Map as Map
import qualified Data.Set as Set
import  Control.Monad.State.Strict as State

import Control.Lens
       ((%=), (%~), (&), (+=), (.=), (.~), (^.), makeLenses)
import Data.Colour.SRGB (sRGB24)

import qualified System.Console.ANSI as ANSI

import IntExecution
import IntSystem

initSystemForInput :: [MWord] -> [MWord] -> System
initSystemForInput program input = system
  where
    initialMemory = Map.fromList $ zip (fmap Addr [0 ..]) program
    initRegisters =
      Registers {_relativeBase = 0, _ip = 0, _halt = False, _tick = 0}
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
  | Ball
  deriving (Show, Enum, Eq)

data JoyPosition
  = Neutral
  | JLeft
  | JRight
  deriving (Show, Eq)

type Field = Map.Map Coord Tile

data SystemWithScreen = SystemWithScreen
  { _system :: System
  , _field :: Field
  , _score :: Integer
  , _paddlePosition :: Integer
  , _joyPosition :: JoyPosition
  }

makeLenses ''SystemWithScreen

ballTileId = 4
scoreTileX = -1
scoreTileY = 0

systemWithScreenStep :: State.StateT SystemWithScreen IO ()
systemWithScreenStep = do
  sws <- State.get
  let terminationPredicate sys =
        length (sys ^. output) == 3 || sys ^. registers . halt
      intMachineWithOutput =
        State.execState (runUntil terminationPredicate) (sws ^. system)
  if null (intMachineWithOutput ^. output)
    then pure ()
    else do
      let [tx, ty, tid] = intMachineWithOutput ^. output
          newJoyPosition =
            if tid == ballTileId
              then joyDirection (sws ^. paddlePosition) tx
              else sws ^. joyPosition
          newInputs = [fromJoyPosition newJoyPosition]
      if tx == scoreTileX && ty == scoreTileY 
        then score .= tid
        else paintPixel (Coord tx ty) (toEnum (fromInteger tid))
      system .= (intMachineWithOutput & (output .~ []) & (input .~ newInputs))
      paddlePosition += fromJoyPosition newJoyPosition
      systemWithScreenStep

paintPixel :: Coord -> Tile -> State.StateT SystemWithScreen IO ()
paintPixel coord tile = do
  field %= Map.insert coord tile
  State.liftIO $ paintPixelAscii coord tile

paintPixelAscii :: Coord -> Tile -> IO ()
paintPixelAscii (Coord y x) tile = do
  ANSI.setCursorPosition (fromIntegral x) (fromIntegral y)
  let r = fromIntegral $ 9 * x
      g = 0
      b = 102
  ANSI.setSGR [ANSI.SetRGBColor ANSI.Background $ sRGB24 r g b]
  putChar (tileToChar tile)

tileToChar :: Tile -> Char
tileToChar EmptyTile = ' '
tileToChar Wall = '░'
tileToChar Block = '▂'
tileToChar Paddle = '▄'
tileToChar Ball = '•'

joyDirection :: Integer -> Integer -> JoyPosition
joyDirection paddleX ballX
  | (paddleX - ballX) > 0 = JLeft
  | (paddleX - ballX) < 0 = JRight
  | otherwise = Neutral

toJoyPosition :: Integer -> JoyPosition
toJoyPosition 0 = Neutral
toJoyPosition (-1) = JLeft
toJoyPosition 1 = JRight

fromJoyPosition :: JoyPosition -> Integer
fromJoyPosition Neutral = 0
fromJoyPosition JLeft = -1
fromJoyPosition JRight = 1

resetScreen :: IO ()
resetScreen =
  ANSI.setSGR [ANSI.Reset] >> ANSI.clearScreen >> ANSI.hideCursor >>
  ANSI.setCursorPosition 0 0

part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  let mwords = Prelude.map (\x -> read x :: Integer) (splitOn "," input)
      initSystem = initSystemForInput mwords []
      initSystemWithScreen =
        SystemWithScreen
        { _system = initSystem
        , _field = Map.singleton (Coord 0 0) EmptyTile
        , _paddlePosition = 0
        , _joyPosition = Neutral
        , _score = 0
        }
  result <- State.execStateT systemWithScreenStep initSystemWithScreen
  print (length $ Map.filter (== Block) (result ^. field))

part2 :: IO ()
part2 = do
  input <- readFile "input.txt"
  let mwords = Prelude.map (\x -> read x :: Integer) (splitOn "," input)
      initSystem = initSystemForInput mwords []
      initSystemWithScreen =
        SystemWithScreen
        { _system = (memory %~ Map.insert (Addr 0) 2) initSystem
        , _field = Map.singleton (Coord 0 0) EmptyTile
        , _paddlePosition = 20
        , _joyPosition = Neutral
        , _score = 0
        }
  resetScreen
  result <- State.execStateT systemWithScreenStep initSystemWithScreen
  ANSI.restoreCursor
  putStrLn ""
  putStrLn $ "Score: " <> show (result ^. score)

main :: IO ()
main = part2


