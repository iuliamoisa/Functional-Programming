fibo :: Integer -> Integer 
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-1) + fibo(n-2)
{-model conceptual de evaluare expresii 
1. se parcurge fisierul .hs de sus in jos pana la prima ecuatie care se potriveste
2. se inlocuieste partea stanga cu partea dreapta
-}

{-guard = conditie booleana 
fibo ' x == pattern
| x == 0  | x == 1  == constrangere
= x == rezultat
-}

fibo' :: Integer -> Integer
fibo' x | x == 0  || x == 1      = x
fibo' n                          = fibo'(n-1) + fibo'(n-2)

--screenshot

cmmdc :: Integer -> Integer -> Integer
cmmdc x 0 = x
cmmdc 0 y = y
cmmdc x y = if x > y then 
                cmmdc y (x-y)
            else 
                cmmdc x (y-x)

cmmdc' :: Integer -> Integer -> Integer
cmmdc' x 0 = x
cmmdc' 0 y = y
cmmdc' x y  | x > y = cmmdc' y (x-y)
            | otherwise = cmmdc' x (y-x)

