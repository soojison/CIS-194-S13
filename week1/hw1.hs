-- Validating Credit Card Numbers

-- Exercise 1

toDigits    :: Integer -> [Integer]
toDigits n
  | n < 1     = []
  | otherwise = toDigits(n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1     = []
  | otherwise = [n `mod` 10] ++ toDigitsRev(n `div` 10)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []     = []
doubleEveryOther (x:y:zs) = x : y*2 : doubleEveryOther zs
doubleEveryOther (x:xs) = x:[]

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
-- this way will only take care of integers that are two digits, but
-- in this particular problem, it wouldn't be a problem because credit
-- card digits can range from 0 ~ 9, and at most the doubled digit can
-- be 9*2 = 18. So, we don't have to worry about x being greater than 99.
sumDigits (x:xs) = (x `div` 10) + (x `mod` 10) + sumDigits(xs)

-- Exercise 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) c b a)

