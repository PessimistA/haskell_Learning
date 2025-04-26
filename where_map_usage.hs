calculateavarage :: (Num a, Fractional a, Ord a) => a -> a -> String
calculateavarage n m
  | s > 8 = "Student is successful"
  | otherwise = "Student is not successful"
  where
    s = n * 0.3 + m * 0.7

kareToplamWhere :: (Num a, Ord a) => a -> a -> a
kareToplamWhere a b = x + y
  where
    x = a ^ 2
    y = b ^ 2


doubleList2 :: [Int] -> [Int]
doubleList2 x = map (* 2) (x)

makeString :: [Int] -> [String]
makeString x = map show (x)

squareList2 :: [Int] -> [Int]
squareList2 x = map (^ 2) (x)

doubleNumber :: [(Int, String)] -> [(Int, String)]
doubleNumber = map (\(a, b) -> (a * 2, b))

triple :: [Int] -> [Int]
triple = map (\x -> x * 3)

addsembol :: [String] -> [String]
addsembol = map (\x -> x ++ "!")

takeeven :: (Integral a, Fractional b) => [a] -> [b]
takeeven = map halveEven
  where
    halveEven x
      | mod x 2 == 0 = fromIntegral x / 2
      | otherwise = fromIntegral x

stringadder :: [(Bool, String)] -> [(Bool, String)]
stringadder = map (\(a, b) -> (a, horm a b))
  where
    horm a b
      | a == True = "ok_" ++ b
      | otherwise = "no_" ++ b

wut :: [(Int, String)] -> [(Int, String)]
wut = map (\(a, b) -> (a, horm a b))
  where
    horm a b
      | a > 0 = "positive_" ++ b
      | a < 0 = "negative_" ++ b
      | otherwise = "zero_" ++ b

zut :: [[Double]] -> [[Double]] -- 0 dan küçük olanları sil
zut = map (\xs -> map sqrt (filter (> 0) xs))

cut :: [(Int, String)] -> [(String)] -- bir elemanı çıkarmak mümkün
cut =
  map (\(a, b) -> b ++ show (a ^ 2))
    . filter (\(a, _) -> a > 0)

gut :: [(Int, String)] -> [(String)] -- bir elemanı çıkarmak mümkün
gut =
  map (\(a, b) -> norm a b)
    . filter (\(a, b) -> a > 0)
  where
    norm a b | a > 10 = "large_" ++ b | otherwise = "small_" ++ b

fut :: [String] -> [String] -- bir elemanı çıkarmak mümkün
fut =
  map (\a -> norm a)
  where
    norm a
      | length a <= 4 = "small_" ++ a
      | length a >= 5 && length a <= 8 = "medium_" ++ a
      | otherwise = "large_" ++ a
