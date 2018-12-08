module Stroots.HList
  ( HList(..)
  ) where

import Data.Kind
import Stroots.Nat

infixr 6 :::
data HList :: [Type] -> Type where
  HNil  :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

instance Show (HList '[]) where
  show :: HList '[] -> String
  show HNil = "HNil"

instance (Show (HList as), Show a) => Show (HList (a ': as)) where
  show :: HList (a ': as) -> String
  show (a:::as) = show a ++ " ::: " ++ show as
