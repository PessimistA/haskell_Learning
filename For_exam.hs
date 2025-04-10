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

fibonacci ::( Num a,Eq a )=> a -> a 
fibonacci n | n==0 =0
	    | n==1 =1
	    | otherwise= fibonacci(n-1)+ fibonacci (n-2)


takeask :: [a] -> Int -> a
takeask (x : xs) n
  | n == 0 = x
  | otherwise = takeask xs (n - 1)

takelist :: [a] -> Int -> [a]
takelist (x : xs) n
  | n == 0 = []
  | otherwise = [x] ++ takelist xs (n - 1)

take1 ::(Ord a) =>[a]->Integer->[a]//takelist başka bir versiyon
take1 (x:xs) n  | n>0= [x] ++ take1 xs (n-1)
		| otherwise= []


rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

len :: [a] -> Int
len [] = 0
len (x : xs) = 1 + len xs

sums ::Num a => [a]-> a//tüm sayıları toplar dizideki
sums [] = 0
sums (x:xs)= x+ sums xs

sums :: (Show a, Show b, Show c) => a -> b -> c -> String//string toplaması
sums x y z = show x ++ show y ++ show z

find :: (Eq a)=> [a]-> a -> Integer
find (x:xs) y | x==y = 0
	      | otherwise=1+ find xs y

max1 :: Ord a => [a] -> a
max1 [x]= x
max1 (x:xs)  | x>= maxtail=x
	     |otherwise= maxtail
	     where maxtail= max1 xs

double2 ::Num a=> [a] -> [a]
double2 [] = []
double2 (x:xs)= [x*2] ++ double2 xs 

topla ::Int -> Int
topla n | n>0 = n+ topla (n-1)
	| otherwise= 0
