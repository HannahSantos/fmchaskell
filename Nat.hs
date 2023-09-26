module Nat where

import Prelude
    hiding ((+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem)

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
n ∸ O = n
O ∸ m = O
(S n) ∸ (S m) = n ∸ m

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
min (n, O) = O
min (O, m) = O
min (S n, S m) = S (min (n, m))

max :: (Nat, Nat) -> Nat
max (n, O) = n
max (O, m) = m
max (S n, S m) = S (max (n, m))