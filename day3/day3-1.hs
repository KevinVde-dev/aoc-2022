getBackpacks :: String -> [(String,String)]
getBackpacks content = map (\ x -> splitAt (length x `div` 2) x) $ lines content

findCommon :: (String,String) -> Char
findCommon ([],_) = error "no duplicate found"
findCommon (f:r,s2)
    | f `elem` s2 = f
    | otherwise = findCommon (r,s2)

ordList :: [(Char,Integer)]
ordList = zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52]

charValue :: Char -> Maybe Integer
charValue c = lookup c ordList

calcOutput :: IO ()
calcOutput = do
    content <- readFile "day3/input.txt"
    let backpacks = getBackpacks content
    let duplicates = [ findCommon backpack | backpack <- backpacks ]
    let values = map charValue duplicates
    let sum = foldr (\ (Just v) prev -> prev+v) 0 values
    print sum
