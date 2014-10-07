-- project euler - problem 3
-- https://projecteuler.net/problem=3

-- What is the largest prime factor of the number 600851475143 ?
import Data.List

divisibleBy :: (Integral a) => a -> a -> Bool
x `divisibleBy` y = x `mod` y == 0

sqrtIntegral :: (Integral a) => a -> a
sqrtIntegral = floor . sqrt . fromIntegral

isPrime :: (Integral a) => a -> Bool
isPrime 1       = False
isPrime n
    | n <= 0    = error "only natural numbers" 
    | otherwise = not . any (n `divisibleBy`) $ [2..sqrtIntegral n]


-- basic idea: if n is prime, its the largest prime factor.
-- otherwise, start at the sqrt(n) and walk backwards until we find
-- the first factor. 
largestPrimeFactor :: (Integral a) => a -> a
largestPrimeFactor n
    | n <= 0    =   error "only natural numbers"
    | isPrime n =   n 
    | otherwise =   factorWalk n (sqrtIntegral n)                    
    where   
        factorWalk x next
            | isPrime next && x `divisibleBy` next = next
            | otherwise = factorWalk x (next - 1)

main = print $ largestPrimeFactor 600851475143
