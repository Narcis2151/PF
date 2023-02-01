import Data.List

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer
double x = x+x


--maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
            else y

max3 x y z = let  
              u = maxim x y
             in (maxim  u z)

max31 x y z = if x>=y && x>=z
               then x
             else if y>=x && y>=z
               then y
             else z

max4 x y z t = let
               u = max3 x y z
               in maxim u t

verif x y z t = if (max4 x y z t) >= x && (max4 x y z t) >= y && (max4 x y z t) >= z && (max4 x y z t) >=t
                then True
                else False

suma_patrate x y = x * x + y * y

paritate x = if mod x 2 == 0 then "Par" else "Impar"

factorial x =
          if x <= 1
          then 1
          else x * factorial (x - 1)

factorial2 x = product[1..x]

verifica x y = if x > 2*y
               then "DA"
               else "NU"

