-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
    deriving (Eq,Ord,Show,Read)

isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- StudentMark type
data StudentMark = Student String Int
    deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)
    | m1 >= m2  = s1
    | otherwise = s2

-- Shape algebraic type
data Shape = Circle Float
           | Rectangle Float Float
           deriving (Show)

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w

-- Address types
data Address = Address Building String
    deriving (Show)

data Building = Name String | Number Int
    deriving (Show)

-- Binary tree
data Tree = Null
          | Node Int Tree Tree
          deriving (Show)

testTree =
    Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
            (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

testSearchTree =
    Node 5 (Node 1 Null Null)
           (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ l r) = 1 + max (height l) (height r)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n l r) = n + sumValues l + sumValues r

--#1
data Month = January | February | March | April | May | June
           | July | August | September | October | November | December
    deriving (Eq,Ord,Show,Read)

data Season = Spring | Summer | Autumn | Winter
    deriving (Eq,Ord,Show,Read)

--#2
season :: Month -> (Month, Season)
season March     = (March, Spring)
season April     = (April, Spring)
season May       = (May, Spring)
season June      = (June, Summer)
season July      = (July, Summer)
season August    = (August, Summer)
season September = (September, Autumn)
season October   = (October, Autumn)
season November  = (November, Autumn)
season m         = (m, Winter)

--#3
numberOfDays :: Month -> Int -> Int
numberOfDays February year = 28 + if year `mod` 4 == 0 then 1 else 0
numberOfDays April _       = 30
numberOfDays June _        = 30
numberOfDays September _   = 30
numberOfDays November _    = 30
numberOfDays _ _           = 31

--#4
data Point = Point Float Float
    deriving (Show)

--#5
data PositionedShape = PositionedShape Shape Point
    deriving (Show)

--#6
move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedShape shape (Point x y)) dx dy =
    PositionedShape shape (Point (x + dx) (y + dy))

--#7
numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ l r) = 1 + numberOfNodes l + numberOfNodes r

--#8
isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember n (Node m l r) = n == m || isMember n l || isMember n r

--#9
leaves :: Tree -> [Int]
leaves Null = []
leaves (Node n Null Null) = [n]
leaves (Node _ l r) = leaves l ++ leaves r

--#10
inOrder :: Tree -> [Int]
inOrder Null = []
inOrder (Node n l r) = inOrder l ++ [n] ++ inOrder r

--#11
insert :: Int -> Tree -> Tree
insert n Null = Node n Null Null
insert n (Node m l r)
    | n < m     = Node m (insert n l) r
    | n > m     = Node m l (insert n r)
    | otherwise = Node m l r

--#12
listToSearchTree :: [Int] -> Tree
listToSearchTree = foldr insert Null