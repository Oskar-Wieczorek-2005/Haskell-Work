```haskell
module ExamRevision where

import Data.Char (isLower)

--------------------------------------------------
-- ALGEBRAIC DATA TYPES
--------------------------------------------------

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Ord, Show)

isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

--------------------------------------------------
-- GUARDS
--------------------------------------------------

grade :: Int -> Char
grade mark
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise  = 'F'

absolute :: Int -> Int
absolute x
    | x < 0     = -x
    | otherwise = x

--------------------------------------------------
-- PATTERN MATCHING
--------------------------------------------------

headPlusOne :: [Int] -> Int
headPlusOne []    = -1
headPlusOne (x:_) = x + 1

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

--------------------------------------------------
-- RECURSION
--------------------------------------------------

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

--------------------------------------------------
-- LIST RECURSION
--------------------------------------------------

listLength :: [a] -> Int
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:xs)
    | n == x    = removeAll n xs
    | otherwise = x : removeAll n xs

countEven :: [Int] -> Int
countEven [] = 0
countEven (x:xs)
    | even x    = 1 + countEven xs
    | otherwise = countEven xs

--------------------------------------------------
-- LIST COMPREHENSIONS
--------------------------------------------------

firstSquares :: Int -> [Int]
firstSquares n = [x^2 | x <- [1..n]]

onlyLowerCase :: String -> String
onlyLowerCase xs = [x | x <- xs, isLower x]

--------------------------------------------------
-- HIGHER ORDER FUNCTIONS
--------------------------------------------------

mult10 :: [Int] -> [Int]
mult10 = map (*10)

onlyPositive :: [Int] -> [Int]
onlyPositive = filter (>0)

sumUsingFold :: [Int] -> Int
sumUsingFold = foldr (+) 0

--------------------------------------------------
-- TUPLES
--------------------------------------------------

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

fstValue :: (a,b) -> a
fstValue (x,_) = x

--------------------------------------------------
-- CUSTOM TYPES WITH DATA
--------------------------------------------------

data StudentMark = Student String Int
    deriving (Show)

studentGrade :: StudentMark -> Char
studentGrade (Student _ mark)
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise  = 'F'

--------------------------------------------------
-- TREES
--------------------------------------------------

data Tree = Null
          | Node Int Tree Tree
          deriving (Show)

exampleTree :: Tree
exampleTree =
    Node 10
        (Node 5
            (Node 2 Null Null)
            (Node 7 Null Null))
        (Node 15
            Null
            (Node 20 Null Null))

--------------------------------------------------
-- TREE RECURSION
--------------------------------------------------

height :: Tree -> Int
height Null = 0
height (Node _ l r) = 1 + max (height l) (height r)

sumTree :: Tree -> Int
sumTree Null = 0
sumTree (Node n l r) = n + sumTree l + sumTree r

numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ l r) =
    1 + numberOfNodes l + numberOfNodes r

isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember n (Node m l r)
    | n == m    = True
    | otherwise = isMember n l || isMember n r

inOrder :: Tree -> [Int]
inOrder Null = []
inOrder (Node n l r) =
    inOrder l ++ [n] ++ inOrder r

--------------------------------------------------
-- BINARY SEARCH TREE INSERTION
--------------------------------------------------

insertTree :: Int -> Tree -> Tree
insertTree n Null = Node n Null Null
insertTree n (Node m l r)
    | n < m     = Node m (insertTree n l) r
    | n > m     = Node m l (insertTree n r)
    | otherwise = Node m l r

listToTree :: [Int] -> Tree
listToTree = foldr insertTree Null

--------------------------------------------------
-- FUNCTION COMPOSITION
--------------------------------------------------

squareRootsPositive :: [Float] -> [Float]
squareRootsPositive = map sqrt . filter (>=0)

--------------------------------------------------
-- LAMBDA FUNCTIONS
--------------------------------------------------

zeroToTen :: [Int] -> [Int]
zeroToTen = filter (\x -> x >= 0 && x <= 10)

--------------------------------------------------
-- WHERE CLAUSES
--------------------------------------------------

circleArea :: Float -> Float
circleArea r = pi * radiusSquared
    where
        radiusSquared = r * r

--------------------------------------------------
-- COMMON EXAM STYLE FUNCTIONS
--------------------------------------------------

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) =
    x == y && prefix xs ys

sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xs) =
    x <= y && sorted (y:xs)

power :: Int -> Int -> Int
power _ 0 = 1
power n p
    | p < 0     = error "Negative powers not supported"
    | otherwise = n * power n (p - 1)
```
