getBackpacks :: String -> [String]
getBackpacks = lines

makeTuple :: [String] -> (String,String,String)
makeTuple [x,y,z] = (x,y,z)
makeTuple _ = error "not three elements"

getGroups :: [String] -> [(String,String,String)]
getGroups [] = []
getGroups list = makeTuple (take 3 list) : getGroups (drop 3 list)

findCommon :: (String,String,String) -> Char
findCommon ([],_,_) = error "no duplicate found"
findCommon (f:r,s2,s3)
    | f `elem` s2 && f `elem` s3 = f
    | otherwise = findCommon (r,s2,s3)

ordList :: [(Char,Integer)]
ordList = zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52]

charValue :: Char -> Maybe Integer
charValue c = lookup c ordList

calcOutput :: IO ()
calcOutput = do
    content <- readFile "day3/input.txt"
    let backpacks = getBackpacks content
    let groups = getGroups backpacks
    let common = map findCommon groups
    let values = map charValue common
    let sum = foldr (\ (Just v) prev -> prev+v) 0 values
    print sum