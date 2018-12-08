module Stroots.List (List(..)) where

import GHC.Exts (fromList, toList, IsList, Item)
import Control.Applicative (empty, Alternative, (<|>))

infixr 5 :|
data List a = Empty
            | a :| List a
            deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  Empty <> ys = ys
  (x:|xs) <> ys = x :| (xs <> ys)

instance Monoid (List a) where
  mempty :: List a
  mempty = Empty

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Empty = Empty
  fmap f (x:|xs) = f x :| fmap f xs

instance Applicative List where
  pure :: a -> List a
  pure a = a :| Empty

  (<*>) :: List (a -> b) -> List a -> List b
  Empty <*> _ = Empty
  (f:|fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Alternative List where
  empty :: List a
  empty = mempty

  (<|>) :: List a -> List a -> List a
  (<|>) = mappend

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  Empty >>= _ = Empty
  xs >>= f = foldr (<>) Empty (f <$> xs)

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ z Empty = z
  foldr f z (x:|xs) = f x (foldr f z xs)

instance Traversable List where
  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Empty = pure Empty
  sequenceA (x:|xs) = (:|) <$> x <*> sequenceA xs

instance IsList (List a) where
  type Item (List a) = a
  fromList :: [a] -> List a
  fromList [] = Empty
  fromList (x:xs) = x :| fromList xs

  toList :: List a -> [a]
  toList Empty = []
  toList (x:|xs) = x : toList xs
