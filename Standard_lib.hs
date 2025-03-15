average ns= sum ns `div` length ns
sum1 n= sum [1..n]
square n= n*n
len ns= length ns

fibonacci 0=0
fibonacci 1=1// 1 de durur
fibonacci n=fibonacci (n-1)+fibonacci(n-2)

factorial 1=1// 1 de durur
factorial n= n*factorial(n-1)

polindrome:: String->Bool
polindrome x = x == reverse x 

reverse1 :: [a] -> [a]
reverse1 [] = [] 
reverse1 (x:xs) = reverse1 xs ++ [x]

evenNumbers :: [Int] -> [Int]
evenNumbers xs = [x | x <- xs, x `mod` 2 == 0]

calculator :: Int -> Int -> String ->Int
calculator x y "+"= x+y
calculator x y "-"= x-y
calculator x y "*"= x*y
calculator x y "/"= x `div` y

minİndex :: [Int] -> Int
minİndex [x]=x
minİndex (x:xs) = min x (minİndex xs)

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = not (hasDivisor n 2)

hasDivisor :: Int -> Int -> Bool
hasDivisor n i
    | i * i > n  = False    
    | n `mod` i == 0 = True 
    | otherwise   = hasDivisor n (i + 1) 

isEven ::Int ->Bool
isEven n
    | n `mod` 2==0= True //altgr , ile yap yatay tırnak işaretini
    | n `mod` 2==1= False 
