addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y u = x : y : u



nTimes :: a -> Int -> [a]
nTimes x n = addToList [] x n
  where
    addToList list x n
      | n == 0 = list
      | otherwise = addToList (x : list) x (n - 1)



oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs)
  | odd x = x : oddsOnly xs
  | otherwise = oddsOnly xs



isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = if reverse x == x then True else False


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (x : xx) (y : yy) (z : zz) = x + y + z : sum3 xx yy zz
sum3 [] (y : yy) (z : zz) = y + z : sum3 [] yy zz
sum3 (x : xx) (y : yy) [] = x + y : sum3 xx yy []
sum3 (x : xx) [] (z : zz) = x + z : sum3 xx [] zz
sum3 [] [] (z : zz) = z : sum3 [] [] zz
sum3 [] (y : yy) [] = y : sum3 [] yy []
sum3 (x : xx) [] [] = x : sum3 xx [] []
sum3 [] [] [] = []


groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (a : as) = reverse (helper [] [a] as)
  where
    helper result [] [] = result
    helper result group [] = group : result
    helper result group (a : as) =
      if g == a
        then helper result (a : group) as
        else helper (group : result) [a] as
      where
        g = head group