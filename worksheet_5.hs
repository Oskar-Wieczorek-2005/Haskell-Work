import Data.Char (isLower)

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq,Ord,Show)
data Tree = Null | Node Int Tree Tree deriving (Show)

-- #1 guards
absolute :: Int -> Int
absolute x
    | x < 0 = -x
    | otherwise = x

-- #2 guards
sign :: Int -> Int
sign x
    | x < 0 = -1
    | x == 0 = 0
    | otherwise = 1

-- #3 pattern matching
headMaybe :: [Int] -> Int
headMaybe [] = -1
headMaybe (x:_) = x

-- #4 recursion lists
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- #5 recursion lists
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- #6 recursion + guards
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:xs)
    | n == x = removeAll n xs
    | otherwise = x : removeAll n xs

-- #7 higher order map
map10 :: [Int] -> [Int]
map10 = map (*10)

-- #8 higher order filter
onlyLower :: String -> String
onlyLower = filter isLower

-- #9 foldr
sumSquares :: [Int] -> Int
sumSquares = foldr (\x acc -> x*x + acc) 0

-- #10 list comprehension
firstSquares :: Int -> [Int]
firstSquares n = [x*x | x <- [1..n]]

-- #11 tuples
sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

-- #12 pattern matching ADT
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

-- #13 tree recursion
height :: Tree -> Int
height Null = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- #14 BST recursion
insert :: Int -> Tree -> Tree
insert n Null = Node n Null Null
insert n (Node m l r)
    | n < m = Node m (insert n l) r
    | n > m = Node m l (insert n r)
    | otherwise = Node m l r

-- #15 recursion lists
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys