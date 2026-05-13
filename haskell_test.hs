module ExamRevision where

import Data.Char (isLower)

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Ord, Show)

-- #1 algebraic data types
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

-- #2 guards
grade :: Int -> Char
grade mark
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise  = 'F'

-- #3 guards
absolute :: Int -> Int
absolute x
    | x < 0     = -x
    | otherwise = x

-- #4 pattern matching
headPlusOne :: [Int] -> Int
headPlusOne []    = -1
headPlusOne (x:_) = x + 1

-- #5 pattern matching
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- #6 recursion
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- #7 recursion
sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

-- #8 recursion lists
listLength :: [a] -> Int
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

-- #9 recursion lists
sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

-- #10 recursion + guards
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:xs)
    | n == x    = removeAll n xs
    | otherwise = x : removeAll n xs

-- #11 recursion + guards
countEven :: [Int] -> Int
countEven [] = 0
countEven (x:xs)
    | even x    = 1 + countEven xs
    | otherwise = countEven xs

-- #12 list comprehension
firstSquares :: Int -> [Int]
firstSquares n = [x^2 | x <- [1..n]]

-- #13 list comprehension
onlyLowerCase :: String -> String
onlyLowerCase xs = [x | x <- xs, isLower x]

-- #14 higher order map
mult10 :: [Int] -> [Int]
mult10 = map (*10)

-- #15 higher order filter
onlyPositive :: [Int] -> [Int]
onlyPositive = filter (>0)

-- #16 foldr
sumUsingFold :: [Int] -> Int
sumUsingFold = foldr (+) 0

-- #17 tuples
sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

-- #18 tuples
fstValue :: (a,b) -> a
fstValue (x,_) = x

-- #19 algebraic data types
data StudentMark = Student String Int
    deriving (Show)

studentGrade :: StudentMark -> Char
studentGrade (Student _ mark)
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise  = 'F'

-- #20 tree recursion
data Tree = Null
          | Node Int Tree Tree
          deriving (Show)

height :: Tree -> Int
height Null = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- #21 tree recursion
sumTree :: Tree -> Int
sumTree Null = 0
sumTree (Node n l r) = n + sumTree l + sumTree r

-- #22 tree recursion
numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ l r) =
    1 + numberOfNodes l + numberOfNodes r

-- #23 tree recursion
isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember n (Node m l r)
    | n == m    = True
    | otherwise = isMember n l || isMember n r

-- #24 tree recursion
inOrder :: Tree -> [Int]
inOrder Null = []
inOrder (Node n l r) =
    inOrder l ++ [n] ++ inOrder r

-- #25 binary search tree
insertTree :: Int -> Tree -> Tree
insertTree n Null = Node n Null Null
insertTree n (Node m l r)
    | n < m     = Node m (insertTree n l) r
    | n > m     = Node m l (insertTree n r)
    | otherwise = Node m l r

-- #26 binary search tree
listToTree :: [Int] -> Tree
listToTree = foldr insertTree Null

-- #27 function composition
squareRootsPositive :: [Float] -> [Float]
squareRootsPositive = map sqrt . filter (>=0)

-- #28 lambda functions
zeroToTen :: [Int] -> [Int]
zeroToTen = filter (\x -> x >= 0 && x <= 10)

-- #29 where clauses
circleArea :: Float -> Float
circleArea r = pi * radiusSquared
    where
        radiusSquared = r * r

-- #30 recursion lists
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) =
    x == y && prefix xs ys

-- #31 recursion lists
sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xs) =
    x <= y && sorted (y:xs)

-- #32 recursion
power :: Int -> Int -> Int
power _ 0 = 1
power n p
    | p < 0     = error "Negative powers not supported"
    | otherwise = n * power n (p - 1)