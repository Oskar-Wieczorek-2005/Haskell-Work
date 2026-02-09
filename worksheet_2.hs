-- #1
absolute :: Int -> Int
absolute x
  | x < 0     = -x
  | otherwise = x

-- #2
sign :: Int -> Int
sign x
  | x < 0     = -1
  | x == 0    = 0
  | otherwise = 1

-- #3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && y == z = 3
  | x == y || y == z || x == z = 2
  | otherwise = 0

-- #4
sumDiagonalLengths :: Float -> Float -> Float ->Float
sumDiagonalLengths a b c = sqrt (a^2 + b^2) + sqrt (b^2 + c^2) + sqrt (a^2 + c^2)

-- #5
taxiFare :: Int -> Float
taxiFare distance
  | distance <= 10 = 2.20 + fromIntegral distance * 0.50
  | otherwise      = 2.20 + 10 * 0.50 + fromIntegral (distance - 10) * 0.30

-- #6
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
  | x > average && y > average && z > average = 3
  | (x > average && y > average) || (y > average && z > average) || (x > average && z > average) = 2
  | x > average || y > average || z > average = 1
  | otherwise = 0
  where average = round (fromIntegral (x + y + z) / 3)

-- #7
validDate :: Int -> Int -> Bool
validDate day month
  | month < 1 || month > 12 = False
  | day < 1 || day > daysInMonth month = False
  | otherwise = True
  where
    daysInMonth m
      | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
      | m `elem` [4, 6, 9, 11] = 30
      | m == 2 = 28 

-- #8
daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month < 1 || month > 12 = 0
  | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | month `elem` [4, 6, 9, 11] = 30
  | month == 2 && isLeapYear year = 29
  | month == 2 = 28
  where
    isLeapYear y
      | (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0) = True
      | otherwise = False