import Data.List (minimum, delete)

--1. fct care intoarce minimul unei liste, calculand primul element din lista sortata
qs :: Ord a => [a] -> [a]
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [hd] ++ qs (filter (>hd) tl)

minimLista :: Ord a => [a] -> a
minimLista [] = error "lista vida"
minimLista (hd:tl) = (head (qs (hd:tl)))

minimLista2 :: Ord a => [a] -> a
minimLista2 xs = let (y:ys) = qs xs in y
{-
2. proiectati un experiment computational prin care sa comp
complex-timp a functiei minim cu complex-timp a fct de sortare

- tuple, quicksort, reverse => snd(head quicksort)
ghci> zip [1 .. 5] ["one", "two", "three", "four", "five"]  => [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
-}
qsCalcul :: Ord a => ([a], Int) -> ([a], Int)
qsCalcul ([], 0) = ([], 0)
--qsCalcul ((hd:tl), i) = (qsCalcul (filter (<=hd) tl, 1) ++ [hd] ++ qsCalcul (filter (>hd) tl, 1), i + 1) 
--qsCalcul (filter (<=hd) tl) ++ [hd] ++ qsCalcul (filter (>hd) tl)
{-
3. repeta experimentul pt mai multe functii de sort(insertie selectie mergesort quicksort)
-}
--selection sort
ssort :: Ord t => [t] -> [t]
ssort [] = []
ssort xs = let { x = minimum xs } 
           in  x : ssort (delete x xs)

{-
4.repeta exp pt o implementare a functiei max prin calculul ultimului
element al listei sortate
ghci> :t max
max :: Ord a => a -> a -> a
-}
maxLista :: Ord a => [a] -> a
maxLista [] = error "lista vida"
maxLista (hd:tl) = (head (reverse(qs (hd:tl))))


--5. defin lista infinita a nr fibonacci



-- sauuu:

fibonacciaux :: Integer -> Integer -> [Integer]
fibonacciaux x y = x : fibonacciaux y (x+y)

fibonacci :: [Integer]
fibonacci = fibonacciaux 1 1
{---incepe cu 0 si 1; adun coada la lista curenta
take 10 fibInfinit => [0,1,1,2,3,5,8,13,21,34]
(0.00 secs, 54,000 bytes)
-}



--6. def o lista infinita de true/false in care nr prime sa aiba asociata val True

isPrimeList :: [Bool]
isPrimeList = map isPrime [0..]

hasDivisors :: Int -> Int -> Int -> Bool
hasDivisors n a b   | a > b = False --nu exista valori intre a si b  
                    | mod n a == 0 = True 
                    | otherwise =  hasDivisors n (a+1) b


isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = not (hasDivisors n 2 (n-1))
{--
ghci> take 15 isPrimeList => [False,False,True,True,False,True,False,True,False,False,False,True,False,True,False]
-}

--7. def o lista infinita a nr prime

infinitePrimes :: [Int]
infinitePrimes = filter isPrime [0..]
