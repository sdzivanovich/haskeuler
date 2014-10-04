-- project euler: problem 1
-- https://projecteuler.net/problem=1

--Find the sum of all the multiples of 3 or 5 below 1000.
divisibleBy :: (Integral a) => a -> a -> Bool
x `divisibleBy` y = x `mod` y == 0

problemOne :: (Integral a) => a -> a
problemOne n = sum . filter (\x -> x `divisibleBy` 3 || x `divisibleBy` 5) $ [1..n-1]

main = print $ problemOne 1000

