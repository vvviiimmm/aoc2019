module Main where

import qualified Data.List.Split as Split
import qualified Data.List as List

main :: IO ()
main = putStrLn "Hello, Haskell!"

part1 :: IO ()
part1 = do 
    input <- readFile "input.txt"
    let numbers = fmap (\x -> read [x] :: Integer) input
        layerSize = 25 * 6
        layers = Split.chunksOf layerSize numbers

        pairs = fmap (\lst -> (numberOfX 0 lst, (numberOfX 1 lst) * (numberOfX 2 lst))) layers
        result = List.sortOn fst pairs
    
    print result

part2 :: IO ()
part2 = do
    input <- readFile "input.txt"
    let numbers = fmap (\x -> read [x] :: Int) input
        layerWidth = 25
        layerHeight = 6
        layerSize = layerWidth * layerHeight
        layers = Split.chunksOf layerSize numbers
        pixels = List.transpose layers
        decoded = fmap colorOf pixels
        image = grouped 25 $ concatMap show decoded
    
    mapM_ putStrLn image


numberOfX :: Eq a => a -> [a] -> Int
numberOfX x = length . filter (==x)

colorOf :: [Int] -> Int
colorOf pixels = case dropWhile (== 2) pixels of
    [] -> error "Shouldn't be any transparent pixels"
    xs -> head xs

grouped :: Int -> [a] -> [[a]]
grouped 1 xs = [xs]
grouped _ [] = []
grouped n xs = take n xs : grouped n (drop n xs)