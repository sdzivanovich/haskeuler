-- project euler: problem 7
-- https://projecteuler.net/problem=7
-- What is the 10 001st prime number?

divisibleBy :: (Integral a) => a -> a -> Bool
x `divisibleBy` y = x `mod` y == 0

isPrime :: (Integral a) => a -> Bool
isPrime 1       = False
isPrime n  
    | n <= 0    = error "only natural numbers" 
    | otherwise =   let sqrtIntegral = floor . sqrt . fromIntegral 
                    in not . any (n `divisibleBy`) $ [2..sqrtIntegral n]

nthPrime :: (Integral a) => Int -> a
nthPrime n =    let primes = filter isPrime [1..]
                in  primes !! n

-- index by 10000 because lists are 0-indexed
main = print $ nthPrime 10000
