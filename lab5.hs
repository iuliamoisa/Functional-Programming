--clasa polimorfica: are ca arg un tip a
--MINIMAL == nr minim de fct si care snt ele 

--data Nat = Cons [Bool] -- lista de valori de tip boolean; reprezentari binare
-- infix 4 => precdenta in asociere e mai mare cu cat nr e mai mare

data Exp = Val Int
          | Add Exp Exp
          | Mul Exp Exp
instance Show Exp where
  show (Val x) = show x
  show (Add e1 e2) = "(" ++ (show e1) ++ "+"  ++ (show e2) ++ ")"
-- :t ((Val 3)(Val 4))

--in cls pot scrie si definitii temporare
{-
class Eval a where
  eval :: a -> Int 

-}

data Nat = Zero | Double Nat | DoubleAddOne Nat

instance Eq Nat where
    (==) Zero Zero                          = True
    (==) (Double Zero) (Double Zero)        = True
    (==) (Double Zero) Zero                 = True
    (==) Zero (Double Zero)                 = True
    (==) Zero _                             = False
    (==) _ Zero                             = False
    (==) (Double Zero) _                    = False
    (==) (DoubleAddOne a) (Double b)        = False
    (==) (Double a) (DoubleAddOne b)        = False
    (==) (Double a) (Double b)              = (==) a b
    (==) (DoubleAddOne a) (DoubleAddOne b)  = (==) a b
--(DoubleAddOne (Double Zero))  == (DoubleAddOne Zero)

instance Ord Nat where
  (<=) Zero _ = True
  (<=) _ Zero = False
  (<=) (Double x) (Double y) = (<=) x y
  (<=) (DoubleAddOne x) (DoubleAddOne y) = (<=) x y
  (<=) (Double x) (DoubleAddOne y) = (<=) x y
  (<=) (DoubleAddOne x) (Double y) = (<=) x y

--instance Integral Nat where


{-
type MyEq :: * -> Constraint
class MyEq a where
  egal :: a -> a -> Bool
  egal x y = not (neegal x y)
  neegal :: a -> a -> Bool
  neegal x y = not (egal x y)
-}
class MyOrd a where
  maimic :: a -> a -> Bool
  maimic x y = not (maimare x y)
  maimare :: a -> a -> Bool
  maimare x y  = not (maimic x y)

instance MyOrd Int where
  maimic a b = (<=) a b
  maimare a b = (<=) b a 



----------------- RANDOM DE PRIN CURS :D :/ :P </3
data Zi = Lun | Mar | Mie | Joi | Vin | Sam | Dum deriving (Show, Enum, Bounded)

{-
 tipul () (pronunțat "unit") reprezintă un tip care are o singură valoare,
  cunoscută sub numele de valoare unitate. 
  Este similar cu conceptul de "void" în alte limbaje de programare, 
  dar în Haskell () este un tip valid și are o valoare validă.

  util cand: o funcție sau un tip de date nu întoarce sau nu primește nicio informație utilă, 
  ci doar semnalează faptul că acțiunea s-a terminat cu succes sau că nu se întâmplă nimic important. 
  ex:
    greet :: String -> ()
    greet name = putStrLn ("Hello, " ++ name)

-}
    
data Exc = DivByZero | NotDefined deriving Show

impartire :: Int -> Int -> Either Exc Int
impartire _ 0 = Left DivByZero
impartire x y = Right (x `div` y)
{-
ghci> impartire 4 0
  Left DivByZero
ghci> impartire 2 2
  Right 1
-}
cmmdc :: Int -> Int -> Either Exc Int
cmmdc 0 0 = Left NotDefined
cmmdc x 0 = Right x
cmmdc x y = if x <= y then
              cmmdc x (y - x)
            else
              cmmdc y (x - y)
{-
ghci> cmmdc 6 4 => Right 2
ghci> cmmdc 8 0 => Right 8
ghci> cmmdc 0 0 => Left NotDefined
-}

{-
understanding FUNCTOR ptc fucking examen peste 2 zile

FUNCTOR este o clasa care defineste op pt a MAPA o functie
sau
FUNCTOR este clasa tipurilor peste care pot aplica functii 

practic permite transf valorilor dintr un context in alt context,
aplicand o functie asupra valorilor interne din acest context

fmap este metoda principala a clasei asteia 
! cand definesc o instanta de tip Functor, practic tre sa implementez
functia fmap pt tipu de date respectiv (arborele de mai jos
)
la ce ajuta??? ptc generalizeaza operatii de mapare si transf a valorilor
in liste, arbori, tipuri de date personaliate etc etc 

class Functor f where
  fmap :: (a -> b) -> f a -> f b-- preia o fct+un context=>
  --un nou context creat prin aplicarea fct pe fiecare valoare din contextul original

ex: lista e un tip care e Functor; pt fmap aplica o functie pe fiecare
element al acesteia => o lista cu elementele transformate
-}
increment :: Int -> Int
increment x = x + 1

originalList :: [Int]
originalList = [1, 2, 3, 4, 5]

transformedList :: [Int]
transformedList = Prelude.fmap increment originalList-- => [2, 3, 4, 5, 6]

x = fmap (+7) (impartire 10 2)
{-
ghci> x => Right 12
-}
x' = case (impartire 10 2) of
      Left err -> Left err
      Right r -> Right ((+7) r)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

--fmap aplica functia f pe fiecare valoare din arbore
instance Functor Tree where
  fmap f Leaf = Leaf -- arborele nu contine nimic => n am ce aplica 
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r) --altfel aplic functia f pe fiecare valoare din arborele respectiv
  -- mai intai pe radacina, apoi recursiv pe subarb stang si apoi pe cel drept
a1 :: Tree Integer
a1 = Node 10 Leaf Leaf
a2 :: Tree Integer
a2 = Node 5 a1 Leaf

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f x Leaf = x --arb contine un sng nod deci n avem pe ce aplica fnctia f
  foldr f x (Node i l r) = foldr f (f i (foldr f x l)) r --aplic functia pe nodul i,
  --apoi continui recursiv sa aplic pe subarborele stang