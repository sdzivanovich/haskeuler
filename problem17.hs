import Data.Char (digitToInt)

asWord :: Integer -> String
asWord 1    = "one"
asWord 2    = "two"
asWord 3    = "three"
asWord 4    = "four"
asWord 5    = "five"
asWord 6    = "six"
asWord 7    = "seven"
asWord 8    = "eight"
asWord 9    = "nine"
asWord 10   = "ten"
asWord 11   = "eleven"
asWord 12   = "twelve"
asWord 13   = "thirteen"
asWord 14   = "fourteen"
asWord 15   = "fifteen"
asWord 16   = "sixteen"
asWord 17   = "seventeen"
asWord 18   = "eighteen"
asWord 19   = "nineteen"
asWord _    = []

tensAsWord :: Integer -> String
tensAsWord 2 = "twenty"
tensAsWord 3 = "thirty"
tensAsWord 4 = "forty"
tensAsWord 5 = "fifty"
tensAsWord 6 = "sixty"
tensAsWord 7 = "seventy"
tensAsWord 8 = "eighty"
tensAsWord 9 = "ninety"
tensAsWord _ = []

compound :: [a] -> [a] -> [a] -> [a]
compound [] ys _ = ys
compound xs [] _ = xs
compound xs ys s = xs ++ s ++ ys

toWords :: [Integer] -> String
toWords xs@[thou, hun, ten, one] =  let f g x   = if x > 0 then g x else [] 
                                        lastTwo = tensAsWord ten ++ asWord one
                                        thouStr = f (\x -> asWord x ++ "thousand") thou
                                        hunStr  = f (\x -> asWord x ++ "hundred") hun
                                    in  compound thouStr (compound hunStr lastTwo "and") "and"

padFront' :: Int -> Int -> a -> [a] -> [a]
padFront' l t x xs 
    | l >= t    = xs
    | otherwise = padFront' (l + 1) t x (x:xs)

padFront :: Int -> a -> [a] -> [a]
padFront l x xs = padFront' (length xs) l x xs

numberToWords :: Integer -> String
numberToWords x =   let digits = padFront 4 0 . map (fromIntegral . digitToInt) . show $ x 
                        fixDigits s@[x, y, z ,w] = if z == 1 then [x, y, 0, 10 + w] else s
                    in  toWords . fixDigits $ digits

problemSeventeen :: (Integral a) => Integer -> a
problemSeventeen x = fromIntegral . sum . map (length . numberToWords) $ [1..x]

main = print . problemSeventeen $ 1000
