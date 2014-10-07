-- project euler: problem 10
-- https://projecteuler.net/problem=10
-- Find the sum of all the primes below two million


divisibleBy :: (Integral a) => a -> a -> Bool
x `divisibleBy` y = x `mod` y == 0

isPrime :: (Integral a) => a -> Bool
isPrime 1       = False
isPrime n  
    | n <= 0    =   error "only natural numbers" 
    | otherwise =   let sqrtIntegral = floor . sqrt . fromIntegral 
                    in  not . any (n `divisibleBy`) $ [2..sqrtIntegral n]

problemTen :: (Integral a) => a -> a
problemTen n = sum . filter isPrime $ [1..n]

main = print $ problemTen 2000000
