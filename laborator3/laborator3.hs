
data MobileDevice1   =   Smartphone1
                    |   Laptop1
                    |   Tablet1 
                        deriving (Show)

data Culori =   Albastru
            |   Rosu 
            |   Verde 
            |   Galben 
                deriving (Show)
data MobileDevice2   =   Smartphone2 Culori
                    |   Laptop2 Culori
                    |   Tablet2 Int
                        deriving (Show)

--am adaugat Int la Tablet pt a putea construi mai multe valori de acest tip
{-ghci> Tablet 12
Tablet 12
ghci> :t (Tablet 2112)
(Tablet 2112) :: MobileDevice
-}

data MobileDevice3   =   Smartphone3 Culori
                    |   Laptop3 Culori
                    |   Tablet3 Int String Culori
                        deriving (Show) --pt ca ghci sa asocieze o reprez ca sir de caractere a valorii constructorilor
                --astfel va fi recunoscut "Laptop" ca valoare

--definire functii peste tip de date 
--ex: definire functie "descriere" peste tipul "MobileDevice"
    --se def pt fiecare constructor in parte!!!
--exemplu pe prima versiune a datei MobileDevice
data MobileDevice4   =   SmartphonePrim
                        |   LaptopPrim
                        |   TabletPrim
descriere :: MobileDevice4 -> String
descriere SmartphonePrim    =   "Acesta este un telefon mobil."
descriere LaptopPrim        =   "Acesta este un laptop de culoare roz."
descriere TabletPrim        =   "Aceasta este o tableta mov."

afisareCuloare :: MobileDevice3 -> Culori 
afisareCuloare (Smartphone3 culoare)    = culoare
afisareCuloare (Laptop3 culoare)    = culoare
afisareCuloare (Tablet3 _ _ culoare)    = culoare

--tip de date pt arbori binari cu noduri nr intregi
data Arb    =   Frunza
            |   Nod Integer Arb Arb deriving (Show, Eq)

--functie care verif daca un arbore binar e arb binar de cautare
isBST :: Arb -> Bool
isBST Frunza    =       True --Frunza = arbore vid=> ABC
 --verif subarbore stang; valoare==rad; stg = valoarea stanga
isBST (Nod valoare (Nod stg fiuStang fiuDrept) _ ) | valoare < stg = False --trebuie ca parintele>fiu stang
 --verif subarbore drept
isBST (Nod valoare _ (Nod right fiuStang fiuDrept)) | valoare > right = False --fiu drept treb sa fie mai mare decat parintele
isBST (Nod valoare stg right) = isBST stg && isBST right

--Scrieti o functie care cauta o valoare de tip ıntreg ıntr-un arbore binar de cautare.
search :: Arb -> Integer -> Bool
search Frunza valoareCautata = False --in arb vid nu exista valori
search (Nod valoare Frunza Frunza) valoareCautata = valoare == valoareCautata
search (Nod valoare stg dr) valoareCautata | valoare == valoareCautata = True
--daca valoarea curenta e mai mica decat valoarea cautata, caut in subarb drept
search (Nod valoare stg dr) valoareCautata | valoare < valoareCautata = search dr valoareCautata 
search (Nod valoare stg dr) valoareCautata = search stg valoareCautata


 --inserare valoare de tip intreg in ABC
insert :: Arb -> Integer -> Arb
insert Frunza valoareInserata = (Nod valoareInserata Frunza Frunza) --daca arb e vid, va exista doar valoarea inserata
--daca exista o singura valoare, adaug val in dreapta(daca e >) sau stanga (daca e <=)
insert (Nod valoare Frunza Frunza) valoareInserata | valoareInserata > valoare  = (Nod valoare Frunza (Nod valoareInserata Frunza Frunza))
insert (Nod valoare Frunza Frunza) valoareInserata | valoareInserata <= valoare = (Nod valoare (Nod valoareInserata Frunza Frunza) Frunza)
--daca val inserata>valoare o introduc in subarb drept
insert (Nod valoare stg dr) valoareInserata |   valoareInserata > valoare  = Nod valoare stg (insert dr valoareInserata)
insert (Nod valoare stg dr) valoareInserata |   valoareInserata <= valoare  = Nod valoare (insert stg valoareInserata) dr

-- functii care calculeaza maximul/minimul dintr-un ABC 
--(alegeti un comportament rezonabil pentru cazul ın care ABC-ul nu are elemente)
maxim :: Arb -> Integer
maxim Frunza = -1
maxim (Nod valoare _ Frunza) = valoare 
maxim (Nod valoare st dr) = maxim dr 

minim :: Arb -> Integer
minim Frunza = -1
minim (Nod valoare Frunza _) = valoare 
minim (Nod valoare stg _) = minim stg 

--fct care sterge cel mai mare element

removeMax :: Arb -> Arb
removeMax Frunza = Frunza --nu am ce sterge; arb este vid
removeMax (Nod valoare Frunza Frunza) = Frunza --daca am doar radacina, arb ramane vid
removeMax (Nod valoare stg Frunza) = stg --daca nu exista subarbore drept, cea mai mare val e radacina; o sterg=> ramane subarb stang
removeMax (Nod valoare stg dr) = Nod valoare stg (removeMax dr) --daca am si subarb stang si dr, sigur treb sa sterg din subarb drept

--fct care sterge o valoare din arb

remove :: Arb -> Integer -> Arb
remove Frunza valoareDeSters  = Frunza
remove (Nod x st Frunza) valoareDeSters | x == valoareDeSters  = st
remove (Nod x Frunza dr) valoareDeSters | x == valoareDeSters  = dr
remove (Nod x st dr) valoareDeSters | x == valoareDeSters = Nod (maxim st) (removeMax st) dr
remove (Nod x (Nod x1 st1 dr1) dr) valoareDeSters | valoareDeSters <= x1  = Nod x (remove (Nod x1 st1 dr1) valoareDeSters) dr
remove (Nod x st dr) valoareDeSters = Nod x st (remove dr valoareDeSters)

preOrder :: Arb -> [Integer]
preOrder Frunza = []
preOrder (Nod x st dr) = [x] ++ preOrder st ++ preOrder dr

inOrder :: Arb -> [Integer]
inOrder Frunza = []
inOrder (Nod x st dr) = inOrder st ++ [x] ++ inOrder dr

postOrder :: Arb -> [Integer]
postOrder Frunza = []
postOrder (Nod x st dr) = postOrder st ++ postOrder dr ++ [x]

{-
Definiti un tip de date pentru expresii booleene (variabile boolene, constante true/false, operatori (s, i, sau, not)).
Definiti o functie pentru simplificarea expresiilor booleene.
Definitti o functie pentru aducerea unei expresii booleene in forma normala conjunctiva.

-}
data ExpresieBooleana = Var String 
                        | Adevarat
                        | Fals 
                        | Si ExpresieBooleana ExpresieBooleana 
                        | Sau ExpresieBooleana ExpresieBooleana
                        | Not ExpresieBooleana
                        deriving Show;

simplifica :: ExpresieBooleana -> ExpresieBooleana
simplifica (Var x) = Var x
simplifica Adevarat = Adevarat
simplifica Fals = Fals 
simplifica (Si Fals _) = Fals
simplifica (Si _ Fals) = Fals
simplifica (Si Adevarat x) = simplifica x
simplifica (Si x Adevarat) = simplifica x
simplifica (Si x y) = Si (simplifica x) (simplifica y)
simplifica (Sau Fals x) = simplifica x
simplifica (Sau x Fals) = simplifica x
simplifica (Sau Adevarat _) = Adevarat
simplifica (Sau _ Adevarat) = Adevarat
simplifica (Sau x y) = Sau (simplifica x) (simplifica y)
simplifica (Not Adevarat) = Fals
simplifica (Not Fals) = Adevarat
simplifica (Not x) = Not (simplifica x)

