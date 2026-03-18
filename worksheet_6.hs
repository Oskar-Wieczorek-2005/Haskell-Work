import Data.Char (isLower)

-- #1
mult10 :: [Int] -> [Int]
mult10 = map (* 10)

-- #2
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

-- #3
orAll :: [Bool] -> Bool
orAll = foldr (||) False

-- #4
sumSquares :: [Int] -> Int
sumSquares x = sum (map (^ 2) x)

-- #5
zeroToTen :: [Int] -> [Int]
zeroToTen = filter (>= 0) . filter (<= 10)

-- #6
squareRoots :: [Float] -> [Float]
squareRoots = filter (>= 0) . map sqrt

-- #7
countBetween :: Float -> Float -> [Float] -> Int
countBetween a b = length . filter (>= a) . filter (<= b)

-- #8
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f x = length (filter (> 0) (map f x)) == length x

-- #9
productSquareRoots :: [Float] -> Float
productSquareRoots = sum . squareRoots

-- #10
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst f (x : xs) = if f x then xs else x : removeFirst f xs

-- #11
removeLast :: (a -> Bool) -> [a] -> [a]
removeLast f = reverse . removeFirst f . reverse

-- #12
zeroToTen' :: [Int] -> [Int]
zeroToTen' = filter (\x -> x >= 0 && x <= 10)

-- #13
alwaysPositive' :: (Float -> Float) -> [Float] -> Bool
alwaysPositive' f = foldr (\x xs -> f x > 0 && xs) True

productSquareRoots' :: [Float] -> Float
productSquareRoots' = foldr (\x xs -> if x > 0 then sqrt x * xs else xs) 1

reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []