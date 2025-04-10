calculator :: (Fractional a) => Char -> a -> a -> a
calculator z x y
  | z == '+' = x + y
  | z == '-' = x - y
  | z == '*' = x * y
  | z == '/' = x / y
  | otherwise = 0

factorial :: (Num a, Eq a) => a -> a
factorial n
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

takeask :: [a] -> Int -> a
takeask (x : xs) n
  | n == 0 = x
  | otherwise = takeask xs (n - 1)

takelist :: [a] -> Int -> [a]
takelist (x : xs) n
  | n == 0 = []
  | otherwise = [x] ++ takelist xs (n - 1)

len :: [a] -> Int
len [] = 0
len (x : xs) = 1 + len xs

sums :: (Show a, Show b, Show c) => a -> b -> c -> String
sums x y z = show x ++ show y ++ show z
