import Data.List

toNumber :: Char -> Integer
toNumber s
    | s `elem` "AX" = 1
    | s `elem` "BY" = 2
    | s `elem` "CZ" = 3
    | otherwise = error "Wrong input"

calcScore :: (Integer,Integer) -> Integer
calcScore game = you + (score `mod` 9)
    where
        elf = fst game
        you = snd game
        score = (you - elf) * 3 + 3

apply :: (a -> b) -> (a,a) -> (b,b)
apply f (a,b) = (f a,f b)

calcOutput :: IO ()
calcOutput = do
    content <- readFile "day2/input.txt"
    let tuples = map (\ (c1:s:c2:r) -> (c1,c2) ) $ lines content
    let numbers = map (apply toNumber) tuples
    let scores = map calcScore numbers
    let s = sum $ map (calcScore . apply toNumber) tuples
    print s