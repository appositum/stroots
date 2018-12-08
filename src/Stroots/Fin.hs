{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Stroots.Fin
  ( Fin(..)
  ) where

import Data.Kind
import Stroots.Nat

data Fin :: Nat -> Type where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

deriving instance Show (Fin n)
