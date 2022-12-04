import Data.Bifunctor

type Range = (Integer,Integer)

splitOn :: Char -> String -> (String,String)
splitOn sep string = (takeWhile (/=sep) string, drop 1 $ dropWhile (/=sep) string)

getPairs :: String -> [(String,String)]
getPairs content = map (splitOn ',') $ lines content

convertToInts :: (String,String) -> (Range,Range)
convertToInts (s1,s2) = (bimap read read $ splitOn '-' s1,bimap read read $ splitOn '-' s2)

getOverlap :: (Range,Range) -> Integer
getOverlap ((r11,r12),(r21,r22))
    | r11 <= r21 && r12 >= r22 || r11 >= r21 && r12 <= r22 = 1
    | otherwise = 0

calcOutput :: IO ()
calcOutput = do
    content <- readFile "day4/input.txt"
    let pairs = getPairs content
    let overlap = map (getOverlap . convertToInts) pairs
    print $ sum overlap