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
