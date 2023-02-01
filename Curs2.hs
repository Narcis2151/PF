
lista = [x*2 | x <- [1..10]]

lista2 = [ x | x <- [50..100], x `mod` 7 == 3]  

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   

lista3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]  

lista4 = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50] 

length' xs = sum [1 | _ <- xs]   

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  

filtered_list = [ [ x | x <- xs, even x ] | xs <- xxs]

t1 = fst ("Wow", False)  
t2 = snd("WoW", False)

zip_f = zip [1 .. 5] ["one", "two", "three", "four", "five"]  
comp_f = [(a,b) | a <- [1,2,3], b <- ['a', 'b', 'c']]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact(n-1)

reverse' [] = []
reverse' ( x : xs ) = ( reverse xs ) ++ [ x ]