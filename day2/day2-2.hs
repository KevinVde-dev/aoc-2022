toNumber :: Char -> Integer
toNumber c = case c of
    'Y' -> -1
    'Z' -> 0
    'X' -> 1
    'A' -> 1
    'B' -> 2
    'C' -> 3
    _ -> 0

myMoveValue :: (Char,Char) -> Integer
myMoveValue (c1,c2) = ((toNumber c1 + toNumber c2) `mod` 3) + 1

matchScore :: Char -> Integer
matchScore 'Y' = 3
matchScore 'Z' = 6
matchScore _ = 0

calcOutput :: IO ()
calcOutput = do
    content <- readFile "day2/input.txt"
    let tuples = map (\ (c1:s:c2:r) -> (c1,c2) ) $ lines content
    let scores = map (\ (a,b) -> myMoveValue (a,b) + matchScore b) tuples
    print $ sum scores