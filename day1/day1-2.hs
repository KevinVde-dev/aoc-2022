import Data.List

findTop3HeavyElves :: FilePath -> IO ()
findTop3HeavyElves path = do
    content <- readFile path
    let parsed = splitOnNewline content []
    let ints = toInts parsed
    let elves = getElvesCarriage ints 0
    let top3 = findTop3Elves elves
    print (sum top3)

splitOnNewline :: String -> [Char] -> [String]
splitOnNewline [] prev = [prev]
splitOnNewline ('\n':r) prev = prev : splitOnNewline r []
splitOnNewline (c:r) prev = splitOnNewline r (prev++[c])

toInts :: [String] -> [Int]
toInts []     = []
toInts ("":r) = 0 : toInts r
toInts (n:r)  = read n : toInts r

getElvesCarriage :: [Int] -> Int -> [Int]
getElvesCarriage [] 0 = []
getElvesCarriage [] w = [w]
getElvesCarriage (0:r) w = w : getElvesCarriage r 0
getElvesCarriage (n:r) w = getElvesCarriage r (w+n)

findTop3Elves :: [Int] -> [Int]
findTop3Elves list = [m1,m2,m3]
    where
        m1 = maximum list
        m2 = maximum (delete m1 list)
        m3 = maximum (delete m2 (delete m1 list))