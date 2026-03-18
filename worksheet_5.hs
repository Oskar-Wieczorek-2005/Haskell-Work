import Prelude hiding (fst, snd, tail)

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

tail :: [a] -> [a]
tail (_:xs) = xs

-- #1
headPlusOne :: [Int] -> Int
headPlusOne []    = -1
headPlusOne (x:_) = x + 1

-- #2
duplicateHead :: [a] -> [a]
duplicateHead []     = []
duplicateHead (x:xs) = x : x : xs

-- #3
rotate :: [a] -> [a]
rotate []         = []
rotate [x]        = [x]
rotate (x1:x2:xs) = x2 : x1 : xs

-- #4
listLength :: [a] -> Int
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

-- #5
multAll :: [Int] -> Int
multAll []     = 1
multAll (x:xs) = x * multAll xs

-- #6
andAll :: [Bool] -> Bool
andAll []     = True
andAll (x:xs) = x && andAll xs

-- #7
orAll :: [Bool] -> Bool
orAll []     = False
orAll (x:xs) = x || orAll xs

-- #8
countIntegers :: Int -> [Int] -> Int
countIntegers _ [] = 0
countIntegers n (x:xs)
  | n == x    = 1 + countIntegers n xs
  | otherwise = countIntegers n xs

-- #9
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:xs)
  | n == x    = removeAll n xs
  | otherwise = x : removeAll n xs

-- #10
removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst n (x:xs) = x : removeAll n xs

-- #11
type StudentMark = (String, Int)

testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

listMarks :: String -> [StudentMark] -> [Int]
listMarks _   [] = []
listMarks stu ((name,mark):xs)
  | stu == name = mark : listMarks stu xs
  | otherwise   = listMarks stu xs

-- #12
sorted :: [Int] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x:y:xs) = x < y && sorted (y:xs)

-- #13
prefix :: [Int] -> [Int] -> Bool
prefix []     _      = True
prefix _      []     = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

-- #14
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _  = True
subSequence _  [] = False
subSequence xs ys = prefix xs ys || subSequence xs (tail ys)