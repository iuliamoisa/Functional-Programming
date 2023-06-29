type Id = String

data Term = Var Id -- x, y, z
     | App Term Term -- (t1 t2)
     | Lambda Id Term deriving (Show, Eq) -- (λx.x)


subst :: Id -> Term -> Term -> Term
subst id term (Var id') | id == id' = term -- daca intalnesc id, il inlocuiesc cu term: x[x/t'] = t';
                        | True = (Var id') -- daca nu este id, ramane neschimbat: y[x/t'] = y (daca x!= y);
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2) --(t1 t2)[x/t'] = (t1[x/t']) (t2[x/t']);
subst id term (Lambda id' term') |  id == id' = (Lambda id' term') --(λx.t)[x/t'] = λx.t;
                                 |  True      = (Lambda id' (subst id term term')) --(λy.t)[x/t'] = λy.(t[x/t']) (daca x!= y).

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) | id == hd    = remove id tl --daca id e oprimul element din lista, il scot si continui sa elimin din restul listei
                  | True        = [hd] ++ (remove id tl) --altfel, il adaug in lista noua si continui sa sterg din restul listei

free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = (free term1) ++ (free term2)
free (Lambda id term) = remove id (free term)

vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = (vars term1) ++ (vars term2)
vars (Lambda id term) = [id] ++ vars(term)

fresh' :: [Id] -> Int -> Id
fresh' ids index =  if ("n" ++ (show index)) `elem` ids then fresh' ids (index + 1)
                    else "n" ++ (show index)

fresh :: [Id] -> Id
fresh ids = fresh' ids 0

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _ | id == id' = term --  x[[x/t']] = t'
                            | True      = (Var id') --y[[x/t']] = y
casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid) -- (t1 t2) [[x/t']] = (t1[[x/t']])(t2[[x/t']])
casubst id term (Lambda id' term') avoid    | id == id' = (Lambda id' term') -- (lambda x.t)[[x/t']] = lambda x.t
                                            | id' `elem` (free term) = let 
                                                                        id'' = fresh avoid
                                                                        varUsed = (avoid ++ [id''])
                                                                   in (casubst id term (Lambda id'' (casubst id' (Var id'') term' varUsed)) varUsed)
                                            | True                  = Lambda id' (casubst id term term' (avoid ++ [id']))
                                        

reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var id') _ = Nothing
reduce1' (App (Lambda id term) term') avoid = 
    Just (casubst id term' term avoid)
reduce1' (App term1 term2) avoid = case reduce1' term1 avoid of
    Nothing -> case reduce1' term2 avoid of
                Nothing -> Nothing
                Just term2' -> Just (App term1 term2')
    Just term1' -> Just (App term1' term2)
reduce1' (Lambda id term) avoid = case reduce1' term avoid of
    Nothing -> Nothing
    Just term' -> Just (Lambda id term')

reduce1 :: Term -> Maybe Term
reduce1 t = reduce1' t (vars t)

x = Var "x"
y = Var "y"
z = Var "z"
u = Var "u"
v = Var "v"
b = Var "b"
m = Var "m"
f = Var "f"
s = Var "s"
n = Var "n"
t = Var "t"
p = Var "p"
term1 = Lambda "x" x
term2 = App term1 term1
term3 = Lambda "y" (Lambda "x" term2)
term4 = App term3 term1


ex1 = reduce1 term1 -- Nothing
ex2 = reduce1 term2 -- Just (\x.x)
ex3 = reduce1 term3 -- Just \y.\x.(\x.x)
ex4 = reduce1 term4 -- Just (Lambda "x" (App (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))))


reduce :: Term -> Term
reduce term = case reduce1 term of
    Nothing -> term
    Just term' -> reduce term'

term5 = App (Lambda "x" (Var "x")) (App (Lambda "z" (Var "z")) (Var "y")) -- (λx.x)((λz.z)y)
ex5 = reduce term5 --Var "y"

tTRUE = Lambda "x" (Lambda "y" x) 
tFALSE = Lambda "x" (Lambda "y" y) 
tAND = Lambda "u" (Lambda "v" (App (App u v) u ))
tOR = Lambda "u" (Lambda "v" (App (App u u) v ))
tITE = Lambda "b" (Lambda "u" (Lambda "v" (App (App b u) v)))-- ITE =λb.λu.λv.b u v 
tNOT = Lambda "u" (App (App u tFALSE) tTRUE)

ex6 = reduce (App (App tAND tTRUE) tFALSE) -- Lambda "x" (Lambda "y" (Var "y")) =   false 
ex7 = reduce (App (App tAND tTRUE) tTRUE) -- Lambda "x" (Lambda "y" (Var "x")) =    true
ex8 = reduce (App (App tOR tFALSE) tFALSE) -- Lambda "x" (Lambda "y" (Var "y")) =   false
ex9 = reduce (App (App tOR tFALSE) tTRUE) -- Lambda "x" (Lambda "y" (Var "x")) = true 
ex10 = reduce (App tNOT tFALSE) -- Lambda "x" (Lambda "y" (Var "x")) = True


zero :: Term
zero = Lambda "s" (Lambda "z" (Var "z"))
--1 = λf.λx.f x
one = Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))


succesor =
  Lambda "n" $
    Lambda "f" $
      Lambda "x" $
        App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))
{-
ghci> reduce (App succesor zero)
Lambda "f" (Lambda "x" (App (Var "f") (Var "x"))) == 1

ghci> reduce (App succesor (App succesor zero))
Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (Var "x")))) == 2
-}

plus = Lambda "m" (Lambda "n" (Lambda "s" (Lambda "z" (App (App (Var "m") (Var "s")) (App (App (Var "n") (Var "s")) (Var "z"))))))
{- 0 + 1 = 1
ghci> reduce(App (App plus zero) one)
Lambda "s" (Lambda "z" (App (Var "s") (Var "z"))) 
-}
mult = Lambda "m" (Lambda "n" (Lambda "s" (Lambda "z" (App (App (Var "m") (App (Var "n") (Var "s"))) (Var "z")))))
{- 
ghci> reduce(App (App mult zero) one) ===== 0 * 1 = 0
Lambda "s" (Lambda "z" (Var "z"))

ghci> reduce(App (App mult one) one)  ===== 1 * 1 = 1
Lambda "s" (Lambda "z" (App (Var "s") (Var "z")))

--- 2* 2 = 4:
ghci> reduce(App (App mult (Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (Var "x")))))) (Lambda "f" (Lambda "x" (App (Var "f") (App (Var "f") (Var "x"))))))
Lambda "s" (Lambda "z" (App (Var "s") (App (Var "s") (App (Var "s") (App (Var "s") (Var "z"))))))
-}
isZero = Lambda "n" (App (App (Var "n") (Lambda "x" (tFALSE))) (tTRUE))
{-
ghci> reduce (App isZero zero)
Lambda "x" (Lambda "y" (Var "x"))  -- true

ghci> reduce (App isZero one)
Lambda "x" (Lambda "y" (Var "y")) -- false 
-}

pair = Lambda "x" (Lambda "y" (Lambda "f" (App ( App (Var "f") (Var "x")) (Var "y"))))
fst = Lambda "p" (App p tTRUE)
snd = Lambda "p" (App p tFALSE)
