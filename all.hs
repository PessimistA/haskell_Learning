numbers :: Int -> [Int]
numbers n = [multiple x | x <- [1 .. n]]
  where
    multiple x = x * x

numbers2 :: (Eq a, Enum a, Integral a) => a -> [a]
numbers2 n = [multiple x | x <- [1 .. n], x `mod` 3 == 0]
  where
    multiple x = x * x

numbers3 :: (Eq a, Enum a, Integral a) => a -> a -> [a]
numbers3 n0 n1 = [x | x <- [n0 .. n1], x `mod` 7 == 0]

numbers4 :: (Eq a, Enum a, Integral a) => a -> a -> [a]
numbers4 n0 n1 = [double x | x <- [n1, (n1 - 1) .. n0], x `mod` 2 == 0]
  where
    double x = x * 2

homemadeasalcalculate :: (Integral a, Ord a) => a -> a -> Bool
homemadeasalcalculate x i
  | x <= 1 = False
  | i * i > x = True
  | x `mod` i == 0 = False
  | otherwise = homemadeasalcalculate x (i + 1)

isPrime x = homemadeasalcalculate x 2

asalCarpanlar :: (Integral a) => a -> [a]
asalCarpanlar x = [i | i <- [2 .. x], x `mod` i == 0, isPrime i]

numbers5 :: (Integral a) => a -> [a]
numbers5 n = [i | i <- [2 .. n], all (== 3) (asalCarpanlar i)]

lambdas :: [Int] -> [Int]
lambdas x = map (\i -> i * 2) x

listest1 :: [Int] -> [Int]
listest1 x = [i * 2 | i <- x]

addthree :: [Int] -> [Int]
addthree x = map (+ 3) x

biggerthan :: [Int] -> [Int]
biggerthan x = filter (> 5) x

foldraddk :: [Int] -> Int
foldraddk x = foldr (-) 0 x -- 1 - (2 - (3 - (4 - 0)))

foldladd1 :: [Int] -> Int
foldladd1 x = foldr1 (-) x -- (1-(2-(3-4)))

foldladd2 :: [Int] -> Int
foldladd2 x = foldl (-) 0 x -- (((0 - 1) - 2) - 3) - 4

foldladd3 :: [Int] -> Int
foldladd3 x = foldl1 (-) x -- ((1-2)-3)-4
