main :: IO ()
main = do
    numbers <- readLines "input.txt"
    let integers = fmap (\n -> read n :: Integer) numbers
        masses = fmap getMass integers
        total = sum masses
    putStrLn (show total) 

getMass :: Integer -> Integer
getMass x = if x > 0 then mass + (getMass mass) else mass where
    mass' = (x `div` 3) - 2
    mass = if mass' < 0 then 0 else mass'

readLines :: FilePath -> IO [String]
readLines f = fmap lines (readFile f)