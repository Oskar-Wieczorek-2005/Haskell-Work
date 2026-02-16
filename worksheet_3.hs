import Prelude hiding ((||), (&&), gcd)

-- #1
infixr 3 &&
(&&) :: Bool -> Bool -> Bool
False && _ = False
True  && False = False
True  && True  = True

-- #2
exOr :: Bool -> Bool -> Bool
exOr True False = True
exOr False True = True
exOr _ _ = False

-- #3
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a b = a
ifThenElse False a b = b

-- #4
daysInMonth :: Int -> Int
daysInMonth 1  = 31
daysInMonth 2  = 28
daysInMonth 3  = 31
daysInMonth 4  = 30
daysInMonth 5  = 31
daysInMonth 6  = 30
daysInMonth 7  = 31
daysInMonth 8  = 31
daysInMonth 9  = 30
daysInMonth 10 = 31
daysInMonth 11 = 30
daysInMonth 12 = 31
daysInMonth _  = error "daysInMonth: month must be 1..12"

-- simpler version requested after #4 (no guards needed here)
validDate :: Int -> Int -> Bool
validDate d m = m >= 1 && m <= 12 && d >= 1 && d <= daysInMonth m

-- #5
sumNumbers :: Int -> Int
sumNumbers n
  | n < 0 = error "sumNumbers: n must be >= 0"
  | n == 0 = 0
  | otherwise = n + sumNumbers (n - 1)

-- (optional compatibility: keep old name if your tests/reference still use it)
sumOfNum :: Int -> Int
sumOfNum = sumNumbers

-- #6
sumSquares:: Int -> Int
sumSquares n
 | n < 0 = error "sumSquares: n must be >= 0"
 | n == 0 = 0
 | otherwise = n ^ 2 + sumSquares (n - 1)

-- #7
power :: Int -> Int -> Int
power n p
  | p < 0 = error "power: exponent must be >= 0"
  | p == 0 = 1
  | otherwise = n * power n (p - 1)

-- #8
sumFromTo :: Int -> Int -> Int
sumFromTo nl nh
  | nl > nh = 0
  | nl == nh = nl
  | otherwise = nh + sumFromTo nl (nh - 1)

-- #9
gcd :: Int -> Int -> Int
gcd n p
  | n < 0 = gcd (abs n) p
  | p < 0 = gcd n (abs p)
  | n == 0 = p
  | p == 0 = n
  | n == p = p
  | n > p  = gcd (n - p) p
  | otherwise = gcd n (p - n)

-- #10
intSquareRoot :: Int -> Int
intSquareRoot n
  | n < 0     = error "intSquareRoot: n must be >= 0"
  | otherwise = findRoot n n

findRoot :: Int -> Int -> Int
findRoot n s
 | s <= 0 = 0
 | s * s <= n = s
 | otherwise = findRoot n (s - 1)


