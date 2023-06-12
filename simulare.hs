
--corectat
-- 1.
sumOfSquares :: Int -> Int 
-- sumOfSquares 0 = 0 Nu e necesarrrr necesar dansu senzual
sumOfSquares 1 = 1
sumOfSquares n = n*n + sumOfSquares(n-1)

-- 2.
countOccurrences :: Eq a => [a] -> a -> Int
countOccurrences [] _ = 0
countOccurrences (x:xs) b = let index' = countOccurrences xs b in
                                if (==) b x then index'+ 1 else index'

-- 3.  ??????????
longestIncreasingSubsequence :: [Int] -> Int
longestIncreasingSubsequence (x:xs) = max (1 + longestIncreasingSubsequence (filter (>x) xs)) (longestIncreasingSubsequence xs)
longestIncreasingSubsequence _ = 0

-- 4.  
data Vehicle = Car String String Int
          | Ship String Int
          | Bycicle String String
          | Truck String String Int
-- 5.  
data Exp = Val Int
          | Var String --variabila 
          | Add Exp Exp
          | Mul Exp Exp
          | Minus Exp Exp
          | Div Exp Exp
  deriving (Eq, Show)
-- Div (Add (Mul (Var "x") (Val 7)) (Val 10)) (Val 2)
-- 6. 
data Shape = Cerc Float
            | Dreptunghi Float Float
            | Triunghi Float Float Float
            | Patrat Float
aria :: Shape -> Float
aria (Cerc a) = pi * a * a 
aria (Dreptunghi a b) = a * b
aria (Patrat a) = a * a
aria (Triunghi a b c) = sqrt ((a+b+c)*(a+b)*(b+c)*(c+a))

-- 7. cu map, foldl, foldr, filter
sumOfEvenSquares :: [Int] -> Int
sumOfEvenSquares = foldl (+) 0 . map (^2) . filter even


-- 8.
averageGrade :: [(String, [Float])] -> [(String, Float)]
averageGrade = map (\(name, grades) -> (name, foldr (+) 0 grades / fromIntegral (length grades)))

-- 9.
{-
The foldr function is used here to fold the list from the right, 
with an anonymous function that takes the current element x and a second argument _ 
(which is not used), and returns a Just x value. 
The Nothing value is used as the initial accumulator for the fold.

If the list is empty, the fold will return Nothing, otherwise it will return Just x, 
where x is the first element of the list.
-}
myhead :: [a] -> Maybe a
myhead = foldr (\x _ -> Just x) Nothing

--sauuuuu::
primul :: a -> a -> a
primul x _ = x

myHead :: [a] -> Maybe a
myHead (x:xs) = Just (foldr primul x (x : xs))
myHead _ = Nothing