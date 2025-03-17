negative n | n<0 = -n
	   | n>0 = -n
	   | n==0 =  0
even1 n | n `mod` 2 ==1 =False
        | n `mod` 2 ==0 =True
        
signum1 n | n> 0 = 1
	  | n<0 = -1
	  | n==0 = 0

in_to_theDiget :: Char-> Int
in_to_theDiget n= fromEnum n

head1 :: [a] -> a
head1 (x:_) = x

tail1 :: [a] -> [a]
tail1 (_:xs) = xs

len :: [a] -> Int
len []=0
len (_:xs)=1+len xs

product1 :: [Int] -> Int
product1 []=1
product1 (x:xs) = (x)* (product1 xs)

product2 :: [Int] -> Int
product2 []=0
product2 (x:xs) = (x)+ (product2 xs)

reverse1 :: [Int] -> [Int]
reverse1 [] =[]
reverse1 (x:xs) = (reverse1 xs) ++ [x]

factorail1 :: (Eq t, Num t) => t -> t
factorail1 0=0
factorail1 1=1
factorail1 n= n* factorail1(n-1)

square :: (Num t) => t-> t
square n = n*n

triple :: (Num t) => t-> t
triple n = n*n*n

kok ::(Num t,Floating t)=> t-> t
kok n= sqrt n

add :: (Num a) => a-> a->a
add x y = x+y

multiple :: (Num a) => a-> a->a
multiple x y = x*y

divide :: Integral a => a -> a -> a
divide x y = x `div` y

extract :: (Num a) => a-> a-> a
extract x y = x-y

asal_mi n= asal n 2

asal n i | i> n-1 = True
	 | n `mod` i == 0 = False
	 | otherwise = asal n (i+1)
