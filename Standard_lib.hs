average ns= sum ns `div` length ns
sum1 n= sum [1..n]
square n= n*n
len ns= length ns

fibonacci 0=0
fibonacci 1=1// 1 de durur
fibonacci n=fibonacci (n-1)+fibonacci(n-2)

factorial 1=1// 1 de durur
factorial n= n*factorial(n-1)
