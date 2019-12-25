module Main where

import Data.List

main :: IO ()
main = putStrLn "Hello, Haskell!"

input = "172851-675869"
start = 172851
end = 675869

hasDoubles' :: String -> Bool
hasDoubles' [] = False
hasDoubles' [x] = False
hasDoubles' (x:y:ys) = (x == y) || hasDoubles' (y:ys)

increasing' :: String -> Bool
increasing' [] = True
increasing' [x] = True
increasing' (x:y:ys) = (x <= y) && increasing' (y:ys)

noTripples' :: String -> Bool
noTripples' x = all (\c -> even (length c) && length c > 0) $ group x

z :: Int -> Bool
z n = let str = show n in noTripples' str && increasing' str


part1 = length $ filter (\n -> hasDoubles' (show n) && increasing' (show n)) [start .. end]
part2 = length $ filter z [start .. end]
