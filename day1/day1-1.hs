findHeavyElf :: FilePath -> IO ()
findHeavyElf path = do
    content <- readFile path
    let parsed = splitOnNewline content []
    let ints = toInts parsed
    let elves = getElvesCarriage ints 0
    print (maximum elves)

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