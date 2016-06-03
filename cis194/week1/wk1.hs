x :: Int
x = 3
-- x = 4 this doesn't work because Multiple declaration

i :: Int
i = -78

-- functions on integers by cases
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

-- choices can also be made based on arbitrary Boolean expressions using guards
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0             = 0
  | n `mod` 17 == 2   = -43
  | otherwise         = n + 3

-- pairs!
p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int, Int) -> Int
sumPair (x,y) = x + y
-- Haskell has triples, quadruples, etc, but you should never use them!

f :: Int -> Int -> Int -> Int
f x y z = x + y + z
-- you have three ints and the output of the func is an int
-- x, y, z = three Int inputs and x + y + z = output

-- function ops have higher precedence than any infix operators!

-- Lists
-- most basic data types in Haskell

nums, range, range2 :: [Integer]
nums   = [1, 2, 3, 19]
range  = [1..100]
range2 = [2, 4..100]

-- strings are just list of characters
-- so, String == [Char]
hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2

-- Simplest list is an empty list
emptyList = []
-- Other lists are built up from the empty list using the cons operator
-- (:) 

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Functions on lists using pattern matching

intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs
-- Since we don't use x in (x:xs), we could simply do (_:xs)

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []
sumEveryTwo (x:[])     = [x]
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs
-- you could simply do  (x:y:zs)

-- build more complex functions by combining many simple ones!
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) -1
