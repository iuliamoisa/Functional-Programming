import Data.Char
import System.Environment
import System.IO
import Control.Exception
{-
*****************1.care e singura valoare de tip()?
Valoarea Unit: ()

*******************2. folositi >> pt a crea o secventa de 3 actiuni
>> este implicit la stanga

--REZOLVARE:
main = (putStrLn "Buna ziua!") >> (putStrLn "Buna seara!") >> (putStrLn "Noapte buna!")

*******************3.scrie un prog care citeste de la tastatura
numele userului, apoi prenumele si afiseaza un mesaj de intampinare

--REZOLVARE:
main = putStrLn "What is your first name?" >>
    getLine >>= \firstName ->
    putStrLn "What is your last name?" >>
    getLine >>= \lastName ->
    putStrLn ("Hello, " ++ firstName ++ " " ++ lastName ++ "!")

*******************4.rescrie programul folosin notatia "do"

--REZOLVARE
main = do
    putStrLn "What is your first name?"
    firstName <- getLine
    putStrLn "What is your last name?"
    lastName <- getLine
    putStrLn $ "Hello, " ++ firstName ++ " " ++ lastName ++ "!"

*******************5. afisare nume la infinit, fara "do"
--REZOLVARE
main = putStrLn "What is your name?" >>
    getLine >>= \name ->
    putStrLn ("Hello, " ++ name ++ "!") >>
    main

*******************6. afisare nume si prenume la infinit
--REZOLVARE
main = putStrLn "What is your first name?" >>
    getLine >>= \firstName ->
    putStrLn "What is your last name?" >>
    getLine >>= \lastName ->
    putStrLn ("Hello, " ++ firstName ++ " " ++ lastName ++ "!") >>
    main

*******************7.citeste nume si prenume pana cand un nume SAU
un prenume e vid
--REZOLVARE
main = do
    putStrLn "What is your first name?"
    firstName <- getLine
    if firstName == "" then
        return()
    else do
       putStrLn "What is your last name?"
       lastName <- getLine
       if lastName == "" then 
            return ()
        else do
            putStrLn ("Hello, " ++ firstName ++ " " ++ lastName ++ "!")
            main

*******************8. citeste siruri si le afiseaza uppercase
--REZOLVARE
main = do  
    putStrLn "Write something"  
    something <- getLine 
    let bigSomething = map toUpper something  
    putStrLn (bigSomething) 
    main
*******************9.
openFile :: FilePath -> IOMode -> IO Handle
module GHC.IO.StdHandles

hGetContents :: Handle -> IO String
hGetLine :: Handle -> IO String
module GHC.IO.Handle.Text 

hClose :: Handle -> IO ()
module GHC.IO.Handle

getProgName :: IO String
getArgs :: IO [String]
module System.Environment

putStr          :: String -> IO ()  -- | Write a string to the standard output device
putStr s        =  hPutStr stdout s



*******************10. afiseaza continutului fisierului ''exemplu.txt''
--REZOLVARE
main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Dati un fisier ca argument!"
            (hd:_) -> do 
                        handle <- openFile hd ReadMode
                        contents <- hGetContents handle
                        putStrLn contents
            
main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Dati un fisier ca argument!"
            (hd:_) -> do 
                        let fileName = head args
                        handle <- openFile fileName ReadMode
                        contents <- hGetContents handle
                        putStrLn contents



*******************11. modifica programul a.i primul argument
 sa fie numele fisierului
--REZOLVARE

*******************12.
--REZOLVARE

*******************13. continutul fisierului uppercase
--REZOLVARE

main = do args <- getArgs
          case args of
            [] -> putStrLn "Dati un fisier ca argument!"
            (hd:_) -> do 
                        handle <- openFile hd ReadMode
                        contents <- hGetContents handle
                        putStrLn $ map toUpper contents


********* 15. 
main = do
  contents <- readFile "input.txt"
  putStrLn contents

  datorita evaluarii lenese, readFile nu se executa decat atunci cand 
  contents sunt necesare, adica la putStrLn; fara evaluare lenesa,
  readFile s-ar executa imediat => flexibilitate in lucrul cu actiuni I/O

  16.

  main = do args <- getArgs
          case args of
            [] -> putStrLn "Dati un fisier ca argument!"
            (hd:_) -> do 
                        let fileName = head args
                        handle <- openFile fileName ReadMode
                        contents1 <- hGetLine handle
                        contents2 <- hGetContents handle
                        putStrLn $ "Cu handle: " ++ contents1 
                        putStrLn $ "Cu hGetContents: " ++ contents2
-}







