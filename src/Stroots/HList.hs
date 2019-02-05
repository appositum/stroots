module Stroots.HList
  ( HList(..)
  ) where

import Data.Kind
import Stroots.Nat
import Stroots.Vector

infixr 6 :::
data HList :: Vector n Type -> Type where
  HNil  :: HList 'Nil
  (:::) :: a -> HList as -> HList (a ':> as)

instance Show (HList 'Nil) where
  show :: HList 'Nil -> String
  show HNil = "HNil"

instance (Show (HList as), Show a) => Show (HList (a ':> as)) where
  show :: HList (a ':> as) -> String
  show (a:::as) = show a ++ " ::: " ++ show as
