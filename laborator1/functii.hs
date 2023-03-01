-- primul pas: ghci in terminal
-- :comanda
--daca vreau sa incarc un fisier folosesc :l fisier.hs
-- orice modificare facuta in fisierul hs trebuie loaded => 
-- apelez in terminal :r
sumThree :: Int -> Int -> Int -> Int -- specific explicit tipul functiei sumThree
myMax :: Int -> Int -> Int
mySum :: Int -> Int 
id x = x --exista id predefinit; aici l am redeclarat=>
--cand apelez id 9 in terminal e posibil sa nu recunoasca id
--deci trebe specificat clar Main.id 9, sa stie pe care sa l foloseasca

sumThree x y z = x + y + z

myMax x y = if x <= y then y else x

myMaxThree x y z = maximum [x,y,z]

mySum x = if x <= 0  then 0 else x + mySum(x-1)

fibonacci n = if n==0 then 0 else (if n==1 then 1 else fibonacci(n-1)+fibonacci(n-2))

cmmdc a b = if a==b then a else if a>b then cmmdc (a-b) b else cmmdc a (b-a)
