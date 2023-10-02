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

quot :: (Nat, Nat) -> Nat
quot (O, m) = O
quot (n, m) = if m ≤ n then S (quot (n ∸ m, m)) else quot (n ∸ m, m)

rem :: (Nat, Nat) -> Nat
rem (O, m) = O
rem (n, m) = if m ≤ n then rem (n ∸ m, m) else n

div :: (Nat, Nat) -> (Nat, Nat)
div (n, m) = (quot (n, m), rem (n, m))