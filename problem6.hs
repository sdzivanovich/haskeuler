-- project euler: problem 6
-- https://projecteuler.net/problem=6

-- Find the difference between the sum of the squares of the first one hundred 
-- natural numbers and the square of the sum.

sumOfSquares :: (Num a) => [a] -> a
sumOfSquares = sum . map (^2)

squareOfSum :: (Num a) => [a] -> a
squareOfSum xs =   sum xs ^ 2

problemSix :: (Num a) => [a] -> a
problemSix xs = squareOfSum xs - sumOfSquares xs

main = print $ problemSix [1..100]
