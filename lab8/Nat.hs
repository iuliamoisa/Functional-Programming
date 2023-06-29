{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Nat(add, 
           Nat(Zero, Succ),
           two,
           three,
           succesor,
           sub,          -- exports a function for subtraction
           mul,
           ) where

data Nat = Zero | Succ Nat deriving (Show, Eq)

addAux :: Nat -> Nat -> Nat
addAux Zero x = x
addAux (Succ y) x = addAux y (Succ x)

add :: Nat -> Nat -> Nat
add x y = addAux x y

two :: Nat
two = Succ (Succ Zero)

three :: Nat
three = Succ (Succ (Succ Zero))

succesor :: Nat -> Nat
succesor n = Succ n

sub :: Nat -> Nat -> Nat
sub m Zero = m
sub Zero _ = Zero
sub (Succ m) (Succ n) = sub m n

mul :: Nat -> Nat -> Nat
mul _ Zero = Zero
mul m (Succ n) = add m (mul m n)

