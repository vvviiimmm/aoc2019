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
import System.Console.ANSI
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)
import Data.Colour.SRGB (sRGB24)
import Debug.Trace

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

data JoyPosition
  = Neutral
  | JLeft
  | JRight deriving (Show, Eq)

type Field = Map.Map Coord Tile

data SystemWithScreen = SystemWithScreen
  { _system :: System
  , _field :: Field
  , _cursor :: Coord
  , _cursorDirection :: Direction
  , _score :: Integer
  , _paddlePosition :: Integer
  , _joyPosition :: JoyPosition
  }

makeLenses ''SystemWithScreen

systemWithScreenStep :: StateT SystemWithScreen IO ()
systemWithScreenStep = do
  sws <- get

  let terminationPredicate sys = length (sys^.output) == 3 || sys^.registers.halt
      intMachineWithOutput = State.execState (runUntil terminationPredicate) (sws^.system)
  if null (intMachineWithOutput^.output)
    then pure ()
    else do
      let [tx, ty, tid] = intMachineWithOutput^.output
          newJoyPosition = if tid == 4 then
            joyDirection (sws^.paddlePosition) tx
              else
            sws^.joyPosition
          newInputs = [fromJoyPosition newJoyPosition]
          
      if tx == -1 && ty == 0 then
        score .= tid
      else do
        if (sws^.system^.registers^.tick > 15000) then liftIO (threadDelay 5000) else pure ()
        paintPixel (Coord tx ty) (toEnum (fromInteger tid))

      system .= (intMachineWithOutput & (output .~ []) & (input .~ newInputs))
      paddlePosition += (fromJoyPosition newJoyPosition) 
      systemWithScreenStep

paintPixel :: Coord -> Tile -> StateT SystemWithScreen IO ()
paintPixel coord tile = do
  field %= Map.insert coord tile
  liftIO $ paintPixelAscii coord tile

drawScreen :: Field -> [String]
drawScreen f = grouped 51 l' where 
  indices = [Coord x y | y <- [0 .. 25], x <- [0 .. 50]]
  l' = fmap (\c -> colorToChar $ colorAt c f)  indices

drawScreenAscii :: Field -> IO ()
drawScreenAscii = undefined

paintPixelAscii :: Coord -> Tile -> IO ()
paintPixelAscii (Coord y x) tile = do
  setCursorPosition (fromIntegral x) (fromIntegral y)
  let cx = 22
      cy = 0
      r = fromIntegral $ 9 * x
      g = 0
      b = 102 -- (fromIntegral x) * 5
  setSGR [SetRGBColor Background $ sRGB24 r g b]
  putChar (colorToChar tile)
  
colorToChar :: Tile -> Char
colorToChar EmptyTile = ' '
colorToChar Wall = '░'
colorToChar Block = '▂'
colorToChar Paddle = '▄'
colorToChar Ball = '•'

colorAt :: Coord -> Field -> Tile
colorAt c f = fromMaybe EmptyTile $ Map.lookup c f

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
        , _paddlePosition = 0
        , _joyPosition = Neutral
        , _score = 0
        }
  result <- State.execStateT systemWithScreenStep initSystemWithScreen
  print (length $ Map.filter (== Block) (result^.field) )

part2 :: IO ()
part2 = do
  input <- readFile "input.txt"
  let mwords = Prelude.map (\x -> read x :: Integer) (splitOn "," input)
      initSystem = initSystemForInput mwords []
      initSystemWithScreen =
        SystemWithScreen
        { _system = (memory %~ Map.insert (Addr 0) 2) initSystem
        , _field = Map.singleton (Coord 0 0) EmptyTile
        , _cursor = Coord 0 0
        , _cursorDirection = DUp
        , _paddlePosition = 20
        , _joyPosition = Neutral
        , _score = 0
        }
  
  -- extract
  resetScreen
  hideCursor
  -- setSGR [SetRGBColor Background $ sRGB24  g b]

  result <- State.execStateT systemWithScreenStep initSystemWithScreen
  
  restoreCursor
  putStrLn ""
  putStrLn $ ("Score: " <> (show (result^.score)) )
  -- forM_ (drawScreen (result^.field)) putStrLn


main :: IO ()
main = part2

pause :: IO ()
pause = do
  hFlush stdout
  -- 1 second pause
  threadDelay 1000000

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0