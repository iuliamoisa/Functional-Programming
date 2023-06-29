type Id = String

data Term = Var Id -- x, y, z
     | App Term Term -- (t1 t2)
     | Lambda Id Term deriving (Show, Eq) -- (λx.x)


-- ex1. ----> reprezinta lambda x.lambda y.x 
var1 = Lambda "x" (Lambda "y" (Var "x"))

--ex2. -----> functie care implementeaza substitutia:  t[x/t'] <=> subst x t' t 

subst :: Id -> Term -> Term -> Term
subst id term (Var id') | id == id' = term -- daca intalnesc id, il inlocuiesc cu term: x[x/t'] = t';
                        | True = (Var id') -- daca nu este id, ramane neschimbat: y[x/t'] = y (daca x!= y);
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2) --(t1 t2)[x/t'] = (t1[x/t']) (t2[x/t']);
subst id term (Lambda id' term') |  id == id' = (Lambda id' term') --(λx.t)[x/t'] = λx.t;
                                 |  True      = (Lambda id' (subst id term term')) --(λy.t)[x/t'] = λy.(t[x/t']) (daca x!= y).

-- 1. x[x/y] = y;
test1 = subst "x" (Var "y") (Var "x")

-- 2. x[y/z] = x;
test2 = subst "y" (Var "z") (Var "x")
-- 3. (x y)[y/z] = x z;
test3 = subst "y" (Var "z") (App (Var "x") (Var "z"))
-- 4. (y x)[y/z] = z x;
test4 = subst "y" (Var "z") (App (Var "y") (Var "x")) 
-- 5. (λx.(y x))[x/(λz.z)] = λx.(y x);
test5 = subst "x" (Lambda "z" (Var "z")) (Lambda "x" (App (Var "y") (Var "x"))) 
-- 6. (λy.(y x))[x/(λz.z)] = λy.(y (λz.z)).
test6 = subst "x" (Lambda "z" (Var "z")) (Lambda "y" (App (Var "y") (Var "x")))


-- ex3: elimina toate apritiile unui element dat ca argument dintr o lista
remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) | id == hd    = remove id tl --daca id e oprimul element din lista, il scot si continui sa elimin din restul listei
                  | True        = [hd] ++ (remove id tl) --altfel, il adaug in lista noua si continui sa sterg din restul listei
{-
testarea functiei pe niste exemple:
     ghci> remove "x" ["y","z"]
          ["y","z"]
     ghci> remove "x" ["y","z", "x"]
          ["y","z"]
     ghci> remove "z" ["z","x","y","z","z","a"]
          ["x","y","a"]
-}

--ex4: free= calc toate variabilele libere ale unui lambda-term
{-
1. free(x) = {x} (pentru orice identificator x ∈ Id);
2. free(λx.t) = free(t) \ {x} (pentru orice identificator x si orice termen t);
3. free(t1 t2) = free(t1) ∪ free(t2) (pentru orice termeni t1, t2).
-}

free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = (free term1) ++ (free term2)
free (Lambda id term) = remove id (free term)
{-
ghci> free (Lambda "5" (App (Var "3")(Var "2")))
     ["3","2"]
ghci> free (Lambda "x" (Lambda "y" (Var "x")))
     []
-}
--ex5: functia vars= calc toate var unui lambda-term, libere sau nu
vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = (vars term1) ++ (vars term2)
vars (Lambda id term) = [id] ++ vars(term)
{-
ghci> vars (Lambda "x" (Lambda "y" (Var "x")))  
     ["x","y","x"]
-}

--ex 6: fresh= calc identificatorii noi/proaspeti
fresh' :: [Id] -> Int -> Id
fresh' ids index =  if ("n" ++ (show index)) `elem` ids then fresh' ids (index + 1)
                    else "n" ++ (show index)
{- fresh' creeaza id nou de tipul "ni"
ghci> fresh' ["1","2","3"] 3 => "n3"
ghci> fresh' ["1","2","n3"] 3 =>"n4"
-}
fresh :: [Id] -> Id
fresh ids = fresh' ids 0

--ex7: casubst = substitutie tip capture-avoiding

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _ | id == id' = term --  x[[x/t']] = t'
                            | True      = (Var id') --y[[x/t']] = y
casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid) -- (t1 t2) [[x/t']] = (t1[[x/t']])(t2[[x/t']])
casubst id term (Lambda id' term') avoid | id == id' = (Lambda id' term') -- (lambda x.t)[[x/t']] = lambda x.t
                                         | id' `elem` (free term) = let 
                                                                        id'' = fresh avoid
                                                                        varUsed = (avoid ++ [id''])
                                                                   in (casubst id term (Lambda id'' (casubst id' (Var id'') term' varUsed)) varUsed)
                                        | True                   = Lambda id' (casubst id term term' (avoid ++ [id']))


--ex8: reduce1= aplica beta-reducere in term dat si intoarce Just term sau Nothing daca nu se poate aplica reducere
term1 = Lambda "x" x
term2 = App term1 term1
term3 = Lambda "y" (Lambda "x" term2)
term4 = App term3 term1
x = Var "x"
y = Var "y"
z = Var "z"
u = Var "u"
v = Var "v"
b = Var "b"
m = Var "m"
f = Var "f"
n = Var "n"

--ex11: TRUE, FALSE, AND, OR, NOT, ITE

tTRUE = Lambda "x" (Lambda "y" x) 
tFALSE = Lambda "x" (Lambda "y" y) 
tAND = Lambda "u" (Lambda "v" (App (App u v) u ))
tOR = Lambda "u" (Lambda "v" (App (App u u) v ))
tITE = Lambda "b" (Lambda "u" (Lambda "v" (App (App b u) v)))-- ITE =λb.λu.λv.b u v 

-- PLUS =  λn.λm.λf.λx.m f (n f x)
--tPLUS = Lambda "n" (Lambda "m" (Lambda "f" (Lambda "x" (App (App (m f) (App (App n f) x))))))



