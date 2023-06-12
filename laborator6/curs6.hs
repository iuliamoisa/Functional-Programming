--lista de elemente de tip a
data Lista a = Vida | Cons a (Lista a) deriving (Eq, Show)

-- polimorfism parametric = valorile tipului parametru sunt tratate identic indiferent de tipul concret
{-
MARCA:  lipsa constrangerilor de clasa asupra variabilelor de tip.
    TIPURILE DE DATE suporta doar acest tip de polimorfism
    indiferent ca am lista de elemente de ip int, bool, etc..
    functia lungime returneaza acelasi lucru
-}
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

lungime :: Lista a -> Int
lungime Vida = 0
lungime (Cons x tl) = 1 + lungime tl
{-
    exemplu:
    ghci> lungime(Cons 1 (Cons 2 (Cons 4 (Vida))))
    3
    ghci> lungime(Cons True (Cons False (Cons False (Vida))))
    3
-}
----------------------------------------------------
-- polimorfism ad-hoc = comportamentul functiei este adaptat in functie de tipul care instantiaza variabila de tip
{-
MARCA:
    prezenta constrangerilor!
-}
qs :: Ord a => [a] -> [a]
-- ^^^^^
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [hd] ++ qs (filter (>hd) tl)
{-
 qs concateneaza elementele ordonate in functie de hd; 
 elementele <=hd sunt primele, urmate de hd si elementele >hd
Exemplu::::
 ghci> qs [1,2,3] => [1,2,3]
ghci> qs [3,2,1] => [1,2,3]
ghci> qs [4,2,1,7] =>  [1,2,4,7]
ghci> qs [True, True] => [True,True]
ghci> qs [True, False, False, True] => [False,False,True,True]
-}

ex :: Ord a => a -> a -> b -> b -> b 
ex x y u v = if x < y then u else v 
{-
ghci> ex 1 2 3 4
3
ghci> ex 6 5 0 1 
1
1
ghci> ex True False False True
True
-}

ex1 :: a -> b 
ex1 x = ex1 x
{-
--Exista o sg optiune pt functia cu aceasta signatura:
bucla infinita pt orice parametru ptc nu stie cum sa produca b
(b nu se afla printre parametri)
-}
ex2 :: a -> a
--Optiuni pt functii cu aceasta signatura
-- ex2 x = ex2 x -> functia care bucleaza la infinit
ex2 x = x -- -> functia identitate

{-
Avantaj polimorfism parametric = teoremele gratuite
---
majoritatea limbajelor: evaluare "eager"
haskell: evaluare "lazy"
    -> nicio expresie (ex: x=expresie; f(exp1, ...expn) )
        nu e evaluata decat daca rez evaluarii este NECESAR
        calculului final

-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)
-- Experimentul 1
a :: Int
a = let x = fib 100 in 2
-- Experimentul 2
b :: Int
b = let x = fib 100 in
    let y = 13 in
    if y > 10 then 2 else x
-- Experimentul 3
f :: Int -> Int -> Int
f x y = if x > 2 then y else x


-- if then else :
ite :: Bool -> a -> a -> a
ite True x y = x
ite False x y = y
{- Exemplu:
ghci> ite True 5 4
5
ghci> ite False 5 4
4
-------
fiecare var Haskell nu contine o valoare, ci un pointer spre o 
bucata de cod (thunk); acea bucata de cod produce valoarea in cauza 
atunci cand e executata
ex: let x = fib 100 in 2 
    var x contine un pointer spre o bucata de cod care
    calculeaza fib 100 
******
When you see let A in B you can think of B as the value of the statement.
 In an imperative language, you might write it as A; return B.
**********

strategie lazy : call-by-need; 
            evita evaluarea repetata a aceluiasi argument 
Avantaj:
        permite definirea de structuri de date aparent infinite;
        Exemplu:
listaNat, la nivel de concept, e lista tuturor nr naturale
in practica, listaNat e o functie care calculeaza aceasta lista
daca este necesar, cand este necesar
-}
listaAux :: Int -> [Int]
listaAux i = i : (listaAux (i + 1)) -- (E6)
listaNat :: [Int]
listaNat = listaAux 0 -- (E5)

-- (!!) :: [a] -> Int -> a
-- (!!) (x:xs) 0 = x (E1)
-- (!!) (x:xs) n = (!!) xs (n-1) (E2)
-- map :: (a -> b) -> [a] -> [b]
-- map f [] = [] (E3)
-- map f (x:xs) = (f x) : map f xs (E4)
--map (+13) [1,2,3] ==> [14,15,16]
-- (map (+13) listaNat) !! 0 se opreste doar datorita evaluarii lenese;
-- in sine, fara !! 0, ar rula la infinit 
{-
evaluare lenesa pt  (map (+13) listaNat) !! 0
(map (+13) listaNat) !! 0 =
                                (E5)
(map (+13) (listaAux 0)) !! 0 =
                                (E6)
(map (+13) (0 : (listaAux (0 + 1)))) !! 0 =
                                (E4)
((+13) 0 : map (+13) (listaAux (0 + 1))) !! 0 =
                                (E1)
(+13) 0 =
                                (def. +)
13.

-}