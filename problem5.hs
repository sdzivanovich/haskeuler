-- project euler, problem 5
-- https://projecteuler.net/problem=5
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?

divisibleBy :: (Integral a) => a -> a -> Bool
x `divisibleBy` y = x `mod` y == 0

-- generate the solution instead of searching for it!
-- think about it: starting at 20 and working down. the number has to be 
-- divisible by 20, so start there. next is 19. 20 isn't divisible by 19, so 
-- then the next smallest possible number is lcm 20 19. etc. 

smallestDivisibleByAll :: (Integral a) => [a] -> a
smallestDivisibleByAll = foldr1 (\x acc -> 
                            if acc `divisibleBy` x 
                            then acc 
                            else lcm acc x)

main = print . smallestDivisibleByAll $ [1..20]
