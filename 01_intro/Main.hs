-------------------------------------------


lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)


-------------------------------------------


sign x	
  | x >	0 = 1
  | x <	0 = (-1)
  | otherwise =	0


-------------------------------------------


x |-| y = sqrt ((x - y) ^ 2)


-------------------------------------------


discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5


--------------------------------------------


import Data.Char
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (isDigit x && isDigit y) then digitToInt x * 10 + digitToInt y else 100


-------------------------------------------


dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = (sqrt ((fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2))


-------------------------------------------


doubleFact :: Integer -> Integer
doubleFact n = if n <= 1 then 1 else n * doubleFact(n - 2)


-------------------------------------------


fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)
            | otherwise = fibonacci (n + 2) - fibonacci (n + 1)


--------------------------------------------


fibonacciAcc :: Integer -> Integer -> Integer -> Integer
fibonacciAcc prev acc n
  | n == 0 = acc
  | otherwise = fibonacciAcc acc (acc + prev) (n - 1)

negativeSignForNegativeEven :: Integer -> Integer
negativeSignForNegativeEven v
  | v < 0 && even v = -1
  | otherwise = 1

fibonacci :: Integer -> Integer
fibonacci n = negativeSignForNegativeEven n * fibonacciAcc 1 0 (abs n)


---------------------------------------------


seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | otherwise = helper n 1 2 3
       
helper :: Integer -> Integer -> Integer -> Integer -> Integer
helper n a b c | n < 3 = c
               | otherwise = helper (n - 1) b c (c + b - 2 * a)


---------------------------------------------


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0, 1)
              | x < 0 = sum'n'count (-x)
              | otherwise = h x 0 0 where
                  h x s k | x == 0 = (s, k)
                          | otherwise = h (div x 10) (s + mod x 10) (k + 1)


--------------------------------------------


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f a + f b) / 2 + (sum $ map f xs))
    where 
        n = 1000
        h = (b - a) / n
        xs = map(\x -> a + h * x) [1..n-1]