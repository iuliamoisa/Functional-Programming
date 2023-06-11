import Prelude hiding (div) 

and :: Bool -> Bool -> Bool
and False _ = False
and _ False = False
and _ _ = True

or :: Bool -> Bool -> Bool
or True _ = True
or _ True = True
or False False = False

not:: Bool -> Bool
not True = False
not False = True

implication :: Bool -> Bool -> Bool
implication False _ = True
implication True False = False
implication True True = True

{-
nand :: Bool -> Bool -> Bool
nand (not False) _ = True
nand _ (not False) = True
nand (not True) (not True) = True
nand (not False) (not False) = False
-}

--hasDivisor n a b => nr n are divizori intre a si b

hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b   | a > b = False --nu exista valori intre a si b  
                    | mod n a == 0 = True 
                    | otherwise =  hasDivisors n (a+1) b


isPrime :: Integer -> Bool
isPrime 1 = False 
isPrime n = Main.not (hasDivisors n 2 (n-1)) --daca exista divizori intre 2 si n-1, nu e prim
--floor(sqrt n)
cmmdc :: Integer -> Integer -> Integer
-- cmmdc primeste doua numere naturale, nu ambele 0
cmmdc x 0 = x
cmmdc 0 y = y
cmmdc x y | x > y    = cmmdc (x - y) y
          | otherwise = cmmdc x (y - x)

cmmdc' :: Integer -> Integer -> Integer
-- cmmdc primeste doua numere naturale, nu ambele 0
cmmdc' a b = if b == 0 then a else cmmdc' b (mod a b)

--cmmdc folosing algoritmul binar???


{-
    4. este posibila aplicarea unei optimizari pentru aducerea apelurilor recursive ın pozitie de
    coada?
    Nu, pentru ca nu putem adauga un acumulator, din cauza faptului 
    x poate fi uneori mai mare iar alteori mai mic 
    decat y=>e nu ne permite sa contorizam numarul de pasi
-}


fibo :: Integer -> Integer

fibo 0 = 0 -- (1)
fibo 1 = 1 -- (2)
fibo n = fibo (n - 1) + fibo (n - 2) -- (3)
fiboaux :: Integer -> Integer -> Integer -> Integer
fiboaux 0 a _ = a  -- (A)
fiboaux n a b = fiboaux (n-1) b (a+b)  -- (B)
{-

--pt orice nr natural n: fibo' n = fiboaux n 0 1

cazul de baza (n=0): fibo' 0 = fiboaux 0 0 1
        fiboaux 0 0 1 
                = (A)
        0 
                = fibo 0 (1)
=> fibo' 0 = fibo 0

cazul inductiv (n > 0): 
        presupun fibo' (n-1) = fibo(n-2) + fibo(n-3)     (3)
arat    fibo' n = fibo(n-1) + fibo(n-2)
fie n ales atbitrar, n nr natural.
fibo' n =


        ----------------------------------------------------------------
-- caz pricipal: fiboaux n a b = 
        fiboaux (n-1) (a+b) a = 
                fibo (n-1) + a + a + b =  

-}
fibo' :: Integer -> Integer
fibo' n = fiboaux n 0 1

--alg extins al lui euclid
{-
euclidExtins :: Integer -> Integer -> (Integer, Integer)
euclidExtins a 0 = (1, 1)
euclidExtins a b = euclidExtins (mod b a) a
-}

succ :: Integer -> Integer 
succ a = a+1

sumaSucc :: Integer -> Integer -> Integer --adunare a 2 nr folosind succ
sumaSucc a 0 = a 
sumaSucc a b = sumaSucc (Main.succ a) (b-1)

mod' :: Integer -> Integer -> Integer
mod' a b    | a < b = a
            | otherwise = mod' (a-b) b

div' :: Integer -> Integer -> Integer
div' a b    | a < b = 0
            | otherwise = 1+ div' (a-b) b

inmultire :: Integer -> Integer -> Integer
 --inmultire 2 nr folosind adunare
inmultire 0 b = 0
inmultire a 0 = 0
inmultire 1 a = a
inmultire b 1 = b 
inmultire a b = sumaSucc a (inmultire a (b-1))

putere :: Integer -> Integer -> Integer
putere _ 0 = 1
putere a b = inmultire a (putere a (b-1))


{-
    9. implementati functiile mod si div pentru numere naturale.
-}

modNat :: Integer -> Integer -> Integer
modNat a b | a < b = a 
modNat a b | a <=0 = a 
modNat a b = modNat (a-b) b 


divNatAux :: Integer -> Integer -> Integer-> Integer
divNatAux r a b | a < b = r
divNatAux r a b = divNatAux (r+1) (a-b) b 
divNat :: Integer -> Integer -> Integer
divNat a b | a < b = 0
divNat a b | a == b = 1
divNat a b = divNatAux 0 a b 
