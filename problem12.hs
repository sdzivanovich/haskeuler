divisibleBy :: (Integral a) => a -> a -> Bool
x `divisibleBy` y = x `mod` y == 0

triangleNum :: (Integral a) => a -> a 
triangleNum n = sum [1..n]

sqrtInt :: (Integral a) => a -> a
sqrtInt = floor . sqrt . fromIntegral

isSquare :: (Integral a) => a -> Bool
isSquare n = sqrtInt n ^ 2 == n

divisors :: (Integral a) => a -> [a]
divisors n =    let half    = filter (n `divisibleBy`) [1..sqrtInt n] 
                    f       = if isSquare n then tail else id
                in  half ++ (f . reverse . map (n `div`) $ half)

problemTwelve :: Integer -> Integer
problemTwelve n =   let sufficientDivisors x =  (toInteger . length . divisors 
                                                $ x) > n                        
                    in  head . filter sufficientDivisors . map triangleNum 
                        $ [n..]

main = print $ problemTwelve 500
