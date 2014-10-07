-- project euler, problem 4
-- https://projecteuler.net/problem=4
-- Find the largest palindrome made from the product of two 3-digit numbers.

isPalindrome :: (Show a, Num a) => a -> Bool
isPalindrome x =    let disp =  show x
                    in  disp == reverse disp

problemFour :: Integer
problemFour =   let threeDigitNums  = [100..999]                
                    mapMult x       = map (*x) threeDigitNums  
                in  maximum . filter isPalindrome 
                    $ concatMap mapMult threeDigitNums  

main = print problemFour
