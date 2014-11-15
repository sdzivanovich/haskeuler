import Data.List        (maximumBy)
import Data.Function    (on)

next :: (Integral a) => a -> a
next x 
    | even x    = x `div` 2
    | otherwise = 3 * x + 1

collatz :: (Integral a) => a -> [a]
collatz 1 =     [1]
collatz n 
    | n <= 0    = error "only positive numbers"
    | otherwise = n : collatz (next n)

pair :: (a -> b) -> a -> (a, b)
pair f x = (x, f x)

cmp :: (a, [b]) -> (a, [b]) -> Ordering
cmp = compare `on` length . snd 

problemFourteen :: (Integral a) => a -> a
problemFourteen l = fst . maximumBy cmp . map (pair collatz) $ [1..l]

main = print $ problemFourteen 999999
