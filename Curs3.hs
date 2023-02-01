import Data.Char
ords :: [Char] -> [Int]
ords xs = [ord x | x <- xs ]

squares :: [Int] -> [Int]
squares [] = []
squares (x:xs) = x * x : squares xs

squares2 :: [Int] -> [Int]
squares2 xs = map sqr xs
    where sqr x = x*x

ords2 :: [Char] -> [Int]
ords2 xs = map ord xs

positives :: [Int] -> [Int]
positives xs = [x | x <- xs, x > 0]

digits :: [Char] -> [Char]
digits [] = []
digits (x:xs)
                |isDigit x = x : digits xs
                |otherwise = digits xs

positives2 :: [Int] -> [Int]
positives2 xs = filter (>0) xs

digits2 :: [Char] -> [Char]
digits2 xs = filter isDigit xs