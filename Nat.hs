module Nat where

data Nat = O | S Nat
    deriving ( Eq , Show )

double :: Nat -> Nat
double O = O
double (S n) = (S (S (double n)))