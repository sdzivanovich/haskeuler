-- project euler: problem 9
-- https://projecteuler.net/problem=9
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

sumTriplet :: (Num a) => (a, a, a) -> a
sumTriplet (x, y, z) = x + y + z

productTriplet :: (Num a) => (a, a, a) -> a
productTriplet (x, y, z) = x * y * z

isPythagorean :: (Eq a, Num a) => (a, a, a) -> Bool
isPythagorean (a, b, c) = a^2 + b^2 == c^2

problemNine :: (Integral a) => a -> a
problemNine n =     let triplets = [(a, b, c) | c <- [1..n], b <- [1..c]
                                                , a <- [1..b]]
                    in productTriplet . head 
                        . filter (\x -> sumTriplet x == n && isPythagorean x) 
                        $ triplets

main = print $ problemNine 1000
