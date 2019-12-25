module Main where

import Data.List.Split
import qualified Data.List as T
import qualified Data.Set as Set
import qualified Control.Monad.State as S

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Coord = Coord { x_ :: Int, y_ :: Int } deriving (Show, Eq, Ord)

data Direction = R Int | L Int | U Int | D Int deriving Show

directions = [R 42, L 34]

project (R n) (Coord x y) = ([Coord x' y | x' <- [x + 1 .. x + n]], Coord (x + n) y)
project (L n) (Coord x y) = ([Coord x' y | x' <- [x - 1, x - 2 .. x - n]], Coord (x - n) y)
project (U n) (Coord x y) = ([Coord x y' | y' <- [y + 1 .. y + n]], Coord x (y + n))
project (D n) (Coord x y) = ([Coord x y' | y' <- [y - 1, y - 2 .. y - n]], Coord x (y - n))

projectCoords :: [Direction] -> Coord -> [Coord]
projectCoords dir@(d:ds) coord@(Coord x y)  = 
    let (projected, last) = project d coord in 
    projected ++ projectCoords ds last
projectCoords _ _ = []

parseDirection :: String -> Direction
parseDirection (d:n) = case d of
    'R' -> R (read n :: Int)
    'L' -> L (read n :: Int)
    'U' -> U (read n :: Int)
    'D' -> D (read n :: Int)

coordsFromFile :: String -> IO ([Coord], [Coord])
coordsFromFile f = do
    inputFile <- readFile "input.txt"
    let (line1: line2: []) = lines inputFile
    let path1 = inputLineToPath line1
    let path2 = inputLineToPath line2
    pure (path1, path2)



inputLineToPath :: String -> [Coord]
inputLineToPath xs = path where
    tokens = splitOn "," xs 
    directions = map parseDirection tokens
    path = projectCoords directions (Coord 0 0)
    
minIntersection :: [Coord] -> [Coord] -> Int
minIntersection a b = minDistance where
    minDistance = minimum $ fmap distance (intersections a b)

intersections :: [Coord] -> [Coord] -> [Coord]
intersections a b = Set.toList $ Set.intersection (Set.fromList a) (Set.fromList b)

distance (Coord x y) = (abs x) + (abs y)

part1 :: IO ()
part1 = do
    (path1, path2) <- coordsFromFile "input.txt"
    print $ minIntersection path1 path2

part2 :: IO ()
part2 = do
    (path1, path2) <- coordsFromFile "input.txt"
    let is = intersections path1 path2
    let ds = map (\i -> (+) <$> (T.elemIndex i path1) <*> (T.elemIndex i path2)) is
    let shortest = T.minimum ds
    print shortest