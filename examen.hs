import Data.Char (ord)
--  PROBLEMA 1

char2Integer :: Char -> Integer
char2Integer c = fromIntegral (ord c) - 48

string2Integer :: String -> Integer
string2Integer [] = 0
string2Integer (x:xs) = 10 ^ length xs * char2Integer x + string2Integer xs


-- PROBLEMA  2
countCapitals :: String -> Int 
countCapitals [] = 0
countCapitals (x:xs)  | x >= 'A' && x <= 'Z' = 1 + countCapitals xs
                      | otherwise = countCapitals xs

-- PROBLEMA 3 

data Vehicle = Car String String Integer 
              | Ship String Integer 
              | Bicycle String String  
              | Truck String String Float 
 deriving Show
vehicleBrand :: Vehicle -> String 
vehicleBrand (Car marca _ _ ) = marca
vehicleBrand (Ship marca _  )= marca
vehicleBrand (Bicycle marca _  )= marca
vehicleBrand (Truck marca _ _ )= marca
-- ghci> vehicleBrand (Car "Matiz" "Model" 1991) ==> "Matiz"

--  PROBLEMA 4 

-- data Exp = Val Int
--           | Var String --variabila 
--           | Add Exp Exp
--           | Mul Exp Exp
--           | Minus Exp Exp
--           | Div Exp Exp
--   deriving (Eq, Show)
--  (x * 7 + 10) - 23 => Minus (Add (Mul (Var "x") (Val 7)) (Val 10)) (Val 23)


--  PROBLEMA 5 

isDigit :: Char -> Bool 
isDigit c =  c >= '0' && c <= '9'

countDigits :: String -> Int
countDigits str = length (filter isDigit str)

--  PROBLEMA 6

decrement :: Int -> [Int]
decrement x = [x - 1]

appFOverList :: [a] -> (a -> [a]) -> [a]
appFOverList [] _ = [] 
appFOverList (x:xs) f = f x ++ appFOverList xs f



-------------- EXAMEN 2023

--1) FACTORIAL  recursiv dupa n primit ca param

factorial :: Int -> Int 
factorial 0 = 1 
factorial n = n*factorial (n-1)

--2) FACTROIAL tail recursive

factorial2 :: Int -> Int -> Int
factorial2 0 n = n
factorial2 n aux = factorial2 (n-1) (aux * n)

-- 3) scrie o functie care executa o actiune IO de un nr de ori dat ca argument 
--ex:  executeNTimes(putStrLn "yay!") 3 => yay! yay! yay!
executeNTimes :: IO () -> Int -> IO()
executeNTimes _ 0 = return()
executeNTimes actiune n = do actiune 
                             executeNTimes actiune (n-1)

-- 4) ce tip are expresia map (+) [1,3,4] ?? 
--R: (map (+) [1,3,4]) :: Num a => [a -> a]
{-
for any type a that is an instance of the Num typeclass,
we can use these functions to perform addition on values of type a.
=> (map (+) [1,3,4]) returns a list of functions that
 can perform addition on values of a specific numeric type (Num a => a -> a)
-}

--5) fol map si filter, scrie o functie care pastreaza doar catul
--impartirii la 2 a elementelor pare dintr o lista ddata ca arg
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

functie :: [Int] -> [Int]
functie [] = []
functie lista = map (`div` 2) (filter isEven lista)

-- 6. un tip de date care permite codificarea unor expresii
--aritm precum (2^x+7)*y; scrie expr data ca valoare de tipul creat

data Exp = Val Int 
        | Var String 
        | Sum Exp Exp
        | Mul Exp Exp 
        | Pow Exp Exp
        deriving Show
-- Mul (Sum (Pow (Val 2) (Var "x")) (Val 7)) (Var "y")

-- 7) proiecteaza un tip de date ListaNevida si scrie o functie recursiva structural
-- care calculeaza lungimea unei liste de tip ListaNevida 
--val de tip ListaNevida trebuie sa reprezinte liste de nr intregi de cel putin
--un element (lista vida sa nu poata fi reprezentata ca listaNevida)

data ListaNevida = Cons Integer | Constructor Integer ListaNevida
len :: ListaNevida -> Int
len (Cons a) = 1
len (Constructor a lista) = 1 + len lista
--ghci> len (Constructor 7 (Constructor 9 (Cons 5))) => 3


{-
Se da functia half ; scrie o functie cu signatura 
(==>) :: (Maybe Int) -> (Int -> Maybe Int) -> Maybe Int
care sa permita inlantuirea aplicarilor de functii precum half. de ex:
Just 20 ==> half ==> half  ~~~> Just 5 
Just 20 ==> half ==> half ==> half  ~~~>  Nothing
-}

half :: Int -> Maybe Int 
half x | even x = Just $ div x 2 
half _ = Nothing 

(==>) :: (Maybe Int) -> (Int -> Maybe Int) -> Maybe Int
(==>) Nothing _ = Nothing 
(==>) (Just x) f = case f x of
                  Nothing -> Nothing 
                  Just y -> Just y 