import GHC.Float (divideDouble)
import Data.List
import Data.Char

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs)
                  | reverse(x) == x = length (filter (`elem` "aeiouAEIOU") x) + nrVocale xs
                  | otherwise = nrVocale xs 

listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv (x:xs) = [y | y <- [1..x], x `mod` y == 0] : listadiv xs

listadiv2 :: [Int] -> [[Int]]
listadiv2 xxs = [[y | y <- [1..xs], xs `mod` y == 0] | xs <- xxs]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec x y (xx:xs) | xx >= x && xx<=y = xx:inIntervalRec x y xs
                          | otherwise = inIntervalRec x y xs


inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp x y xs = [nr | nr <- xs, nr >= x, nr <= y]

pozitii :: [Int] -> Int -> [Int]
pozitii [] _ = []
pozitii (l:ls) poz = 
  if odd l 
    then poz : pozitii ls (poz + 1)
    else pozitii ls (poz + 1)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = pozitii l 0

--b)descrieri de liste -> pozitiiImpareComp
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [ i | (i, x) <- zip [0..] l, odd x]


--8) Scrieti o functie care calculeaza produsul tuturor cifrelor care apar în  sirul 
--de caractere dat ca intrare. Daca nu sunt cifre în sir, raspunsul functiei trebuie sa 
--fie  ̆1 .
--a)Folositi doar recursie. Denumiti functia multDigitsRec
multDigitsRec :: String -> Int 
multDigitsRec "" = 1
multDigitsRec (xs:s) = 
  if xs `elem` "0123456789"
    then (ord xs + (-1) * ord '0' )* multDigitsRec s
    else multDigitsRec s
--b)Folositi descrieri de liste.. Denumiti functia multDigitsComp
multDigitsComp :: String -> Int
multDigitsComp "" = 1
multDigitsComp l = product [digitToInt z | z<-l, isDigit z ]