//üçgen oluşturur
triangular :: Int -> [(Int, Int, Int)]
triangular n = [(x, y, z) | x <- [1 .. n], y <- [1 .. x], z <- [1 .. y], x ^ 2 == y ^ 2 + z ^ 2]

//üçgeni dik olarak değilde mümkün olan her kombinasyonla yapar
isTriangular :: (Int, Int, Int) -> Bool
isTriangular (x, y, z)
  | x + y > z && x + z > y && y + z > x = True
  | otherwise = False

triples :: Int -> [(Int, Int, Int)]
triples n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], isTriangular (x, y, z)]

// noktalarla verilmiş üçgenin çevreyi hesaplar
distance1 :: (Float, Float) -> (Float, Float) -> Float
distance1 (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

calculatePerimeter :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float
calculatePerimeter (x1, y1) (x2, y2) (x3, y3) = sum [d | d <- [distance1 (x1, y1) (x2, y2), distance1 (x2, y2) (x3, y3), distance1 (x3, y3) (x1, y1)]]

//valid olan üçgenleri hesaplayan formul
calculatearea :: (Int, Int, Int) -> Float
calculatearea (a, b, c) = sqrt (s * (s - fromIntegral a) * (s - fromIntegral b) * (s - fromIntegral c))
  where
    s = (fromIntegral (a + b + c)) / 2

isvalid1 :: Int -> [(Int, Int, Int, Float)]
isvalid1 n =
  [ (a, b, c, calculatearea (a, b, c)) | a <- [1 .. n], b <- [1 .. a], c <- [1 .. b], a + b > c, a + c > b, b + c > a
  ]
