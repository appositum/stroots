module Stroots.Nat where

import Data.Kind (Type)

data Nat = Z | S Nat deriving Eq

instance Show Nat where
  show :: Nat -> String
  show = show . fromNat

type family a + b where
  'Z + n = n
  'S n + m = 'S (n + m)

data SNat :: Nat -> Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

deriving instance Show (SNat n)

fromNat :: Integral a => Nat -> a
fromNat Z = 0
fromNat (S n) = 1 + fromNat n

toNat :: Integral a => a -> Nat
toNat 0 = Z
toNat n = S (toNat (n-1))
