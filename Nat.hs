module Nat where

import Prelude
    hiding ((+), (*), (^), (<), quot, min, gcd, lcm, div, max, pred, rem)

data Nat = O | S Nat
    deriving ( Eq , Show )

(+) :: Nat -> (Nat -> Nat)
n + O = n
n + (S m) = S (n + m)

(*) :: Nat -> Nat -> Nat
n * O = O
n * (S m) = n + (n * m)

(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ (S m) = n * (n ^ m)

(∸) :: Nat -> Nat -> Nat
(S n) ∸ (S m) = n ∸ m
n ∸ m = n

(<) :: Nat -> Nat -> Bool
O < (S _) = True
_ < O = False
(S n) < (S m) = n < m

(≤) :: Nat -> Nat -> Bool
O ≤ _ = True
_ ≤ O = False
S n ≤ S m = n ≤ m

ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S (S n)) = ev n

od :: Nat -> Bool
od O = False
od (S O) = True
od (S (S n)) = ev n

double :: Nat -> Nat
double O = O
double (S n) = S (S (double n))

pred :: Nat -> Nat
pred O = O
pred (S n) = n

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib (S (S n)) = fib (S n) + fib n
fib n = n

min :: (Nat, Nat) -> Nat
min (S n, S m) = S (min (n, m))
min (n, m) = O

max :: (Nat, Nat) -> Nat
max (n, O) = n
max (O, m) = m

div :: (Nat, Nat) -> (Nat, Nat)
div (_, O) = error "Zero cannot go there, your computer might break."
div (n, m)
    | n < m     = (O, n)
    | otherwise = let (q', r') = div (n ∸ m, m)
                  in (S q', r')

quot :: (Nat, Nat) -> Nat
quot (n, m) = fst (div (n, m))

rem :: (Nat, Nat) -> Nat
rem (n, m) = snd (div (n, m))

gcd :: (Nat, Nat) -> Nat
gcd (n, m) 
    | n == O    = error "Sike, no zeros" 
    | m == O    = error "Zero is still not allowed"
    | n == m    = n
    | n < m     = gcd (n, m ∸ n)
    | otherwise = gcd (n ∸ m, m)

lcm :: (Nat, Nat) -> Nat
lcm (O, O) = error "Zero cannot go both places, choose"
lcm (n, m) = quot (n * m, gcd (n, m))