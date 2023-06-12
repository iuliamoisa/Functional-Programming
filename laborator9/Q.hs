module Q
    (
        simpl, 
        Q(..)
    ) where 

--numerator / denominator   
data Q = Q Int Int 

instance Show Q where 
    show (Q n d) = show n ++ "/" ++ show d 

simpl :: Q -> Q 
simpl (Q n d) = Q (n `div` c)(d `div` c)
    where c = gcd n d