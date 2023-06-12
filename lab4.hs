{-
add :: Int -> Int -> Int ((ii dau lui add un argument de tip int, returneaza int->int))
    echivalent cu
(add 2)  ::   Int -> Int



forma curried a functiilor e preferata ptc permite aplicarea partiala a functiilor
in Haskell, toate fctiile sunt considerate  ca fiind in forma curried
-}
f :: (Int, Int) -> Int --f(2,3)
f (x,y) = x + y 
--forma curry a lui f:
g :: Int -> Int -> Int --g 2 3
g x y = x + y

------(((1.1)))
addThree :: (Int, Int, Int) -> Int --addThree(2,3,4)
addThree (x, y, z) = x + y + z

addThreeCurry :: Int -> Int -> Int -> Int--addThreeCurry 1 2 3
addThreeCurry x y z = x + y + z

--functii de ordin superior
-- = functii care au ca argument alte functii

process :: (Int -> Int) -> Int -> Int --argumente=functie si un nr intreg
process f x = f x --process (+(-2)) 4; process (+2) 4

process2 :: (a -> a) -> a -> a --a = variabila de orice tip
process2 f x = f x --process2 (&& True) True


--------(((2.1)))
suma :: (Int -> Int) -> Int -> Int -> Int
suma f x y = if x <= y then (f x) + (suma f (x+1) y)
            else 0
-- suma (+0) 2 5
-- suma (+(-1)) 1 2 == 1-1 + 2-1 = 1

------ (((2.2)))

composeFunctions :: (b -> c) -> (a -> b) -> a -> c
composeFunctions f g x = f (g x)

addOne :: Int -> Int
addOne x = x + 1

double :: Int -> Int
double x = 2 * x

composedFunction :: Int -> Int
composedFunction = composeFunctions double addOne


{-
ghci> composedFunction 1  
4

add3 :: Integer -> Integer
add3 x = x + 3

dubleaza x = x * 2
-}


--------(((2.3)))

compunereLista :: [a -> a] -> a -> a
compunereLista (hd:tl) = if (length tl) == 0 then hd 
                    else hd . (compunereLista tl)

--------(((2.4)))

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

--------(((2.5)))
dubleaza x = x * 2

aplicaFunctieLista :: (a -> b) -> [a] -> [b]
aplicaFunctieLista _ [] = []
aplicaFunctieLista f (hd:tl) = f hd : aplicaFunctieLista f tl -- ++ = concatenare liste
--aplicaFunctieLista dubleaza [1,2,3]


--------(((2.6)))

elementeTrue :: (a -> Bool) -> [a] -> [a]
elementeTrue _ [] = []
elementeTrue f (hd:tl) = if f hd
                        then hd : elementeTrue f tl
                        else elementeTrue f tl


even :: Int -> Bool 
even x = if mod x 2 == 0 then True else False
--elementeTrue Main.even [1,2,3]



-------------(((2.7)))
{-
foldr (+) 4 [0, 1, 2, 3] <=> 0 + (1 + (2 + (3 + 4)))
-}
implementareFoldl :: (a -> b -> b) -> b -> [a] -> b
implementareFoldl _ x [] = x
implementareFoldl f x (hd:tl) = implementareFoldl f (f hd x) tl


implementareFoldr :: (a -> b -> b) -> b -> [a] -> b
implementareFoldr f x (hd:tl) = f hd (implementareFoldr f x tl)
implementareFoldr f x _ = x
{-
ghci> implementareFoldr (+) 4 [1,2] 
7
-}

--------(((2.8)))

data Arb = Frunza | Nod Integer Arb Arb deriving (Show, Eq)

--RSD
preOrderApply :: (Integer -> Integer) -> Arb -> [Integer]
preOrderApply _ Frunza = []
preOrderApply f (Nod x st dr) = [f x] ++ preOrderApply f st ++ preOrderApply f dr

--SRD
inOrderApply :: (Integer -> Integer) -> Arb -> [Integer]
inOrderApply _ Frunza = []
inOrderApply f (Nod x st dr) = inOrderApply f st ++ [f x] ++ inOrderApply f dr

--SDR
postOrderApply :: (Integer -> Integer) -> Arb -> [Integer]
postOrderApply f Frunza = []
postOrderApply f (Nod x st dr) = postOrderApply f st ++ postOrderApply f dr ++ [f x]

preOrder :: Arb -> [Integer]
preOrder Frunza = []
preOrder (Nod x st dr) = [x] ++ preOrder st ++ preOrder dr

inOrder :: Arb -> [Integer]
inOrder Frunza = []
inOrder (Nod x st dr) = inOrder st ++ [x] ++ inOrder dr

postOrder :: Arb -> [Integer]
postOrder Frunza = []
postOrder (Nod x st dr) = postOrder st ++ postOrder dr ++ [x]

--------(((3.1)))

--------(((3.2)))

data Either a b = Left a 
            | Right b
            deriving (Ord, Eq)

{-
4.1
fromend [1, 7, 5] 0 --ia elementul de pe pozitia 0 din lista inversata
    => Just 5
4.2
convolute [1,7,5] (reverse[1,2,3])
echivalenta cu functia zip existenta7

binding: let (x, y) <=> rezultatul va fi de forma (x, y)

--convolute :: [a] -> [a] -> Maybe ([(a,a)], [a])
--convolute l1 l2 = let (result, l)

convolute_aux :: [a] -> [a] -> ([(a,a)], [a])
convolute_aux [] ys = ([], ys) --daca am ajuns la o lista vida returenz restul
convolute_aux (x:xs) ys = let (rest, ys') = convolute_aux xs ys in
        case ys' of 
            [] -> (rest, ys)
            (y:ys'') -> ((x,y):rest, ys'')

-}


fromendaux :: [a] -> Int -> (a, Int) -- (a, Int) pastreaza informatia colectata la intoarcere (indicele)
fromendaux [x] index = (x, 0)
fromendaux (x:xs) index = let (x', index') = fromendaux xs index in
    if index' == index then(x', index') else (x, index' + 1)
fromend :: [a] -> Int -> Maybe a --Maybe a== returneaza un rezultat, daca el exista
fromend [] _ = Nothing
fromend (x:xs) index = let (x', index') = fromendaux (x:xs) index in
    if index == index' then Just x' else Nothing