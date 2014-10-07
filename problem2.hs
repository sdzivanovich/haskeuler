-- project euler: problem 2
-- https://projecteuler.net/problem=2

-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

-- compute fibonacci(n) 'iteratively' by keeping state through accumulators
fibonacci :: (Integral a) => a -> a 
fibonacci n 
    | n < 0     = error "no negatives"
    | otherwise = iterfib n 0 1 
    where   
        iterfib 0 _  p  = p
        iterfib n pp p  = iterfib (n - 1) p (pp + p)

problemTwo :: (Integral a) => a -> a
problemTwo n = sum . filter even . takeWhile ( < n) . map fibonacci $ [0..]

main = print $ problemTwo 4000000
