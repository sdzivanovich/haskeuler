import Data.Char (digitToInt)

sumDigits :: (Show a) => a -> Integer
sumDigits = sum . map (toInteger . digitToInt) . show

main = print $ sumDigits (2 ^ 1000)
