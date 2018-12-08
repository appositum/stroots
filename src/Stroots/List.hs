{-# LANGUAGE TypeFamilies #-}

module Stroots.List where

import GHC.Exts (fromList, toList, IsList, Item)
import Control.Applicative (empty, Alternative, (<|>))

infixr 5 :|
data List a = Nil
            | a :| List a
            deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  Nil <> ys = ys
  (x:|xs) <> ys = x :| (xs <> ys)

instance Monoid (List a) where
  mempty :: List a
  mempty = Nil

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (x:|xs) = f x :| fmap f xs

instance Applicative List where
  pure :: a -> List a
  pure a = a :| Nil

  (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  (f:|fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Alternative List where
  empty :: List a
  empty = mempty

  (<|>) :: List a -> List a -> List a
  (<|>) = mappend

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  Nil >>= _ = Nil
  xs >>= f = foldr (<>) Nil (f <$> xs)

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ z Nil = z
  foldr f z (x:|xs) = f x (foldr f z xs)

instance Traversable List where
  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Nil = pure Nil
  sequenceA (x:|xs) = (:|) <$> x <*> sequenceA xs

instance IsList (List a) where
  type Item (List a) = a
  fromList :: [a] -> List a
  fromList [] = Nil
  fromList (x:xs) = x :| fromList xs

  toList :: List a -> [a]
  toList Nil = []
  toList (x:|xs) = x : toList xs

empty :: List a
empty = Nil
