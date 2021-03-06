module Stroots.Vector
  ( append
  , head
  , index
  , replicate
  , tail
  , zip
  , zipWith
  , Vector(..)
  ) where

import Prelude hiding (zip, zipWith, head, tail, replicate)
import Data.Kind (Type)
import Stroots.Fin
import Stroots.Nat

infixr 5 :>
data Vector :: Nat -> Type -> Type where
  Nil  :: Vector 'Z a
  (:>) :: a -> Vector n a -> Vector ('S n) a

instance Show a => Show (Vector n a) where
  show :: Show a => Vector n a -> String
  show xs = "[" ++ show' xs ++ "]" where
    show' :: Show a => Vector n a -> String
    show' Nil = ""
    show' (x:>xs) =
      case xs of
        Nil -> show x
        _ -> show x ++ ", " ++ show' xs

instance Functor (Vector n) where
  fmap :: (a -> b) -> Vector n a -> Vector n b
  fmap _ Nil = Nil
  fmap f (x:>xs) = f x :> fmap f xs

instance Applicative (Vector 'Z) where
  pure :: a -> Vector 'Z a
  pure _ = Nil

  (<*>) :: Vector 'Z (a -> b) -> Vector 'Z a -> Vector 'Z b
  _ <*> _ = Nil

instance Applicative (Vector n) => Applicative (Vector ('S n)) where
  pure :: a -> Vector ('S n) a
  pure a = a :> pure a

  (<*>) :: Vector ('S n) (a -> b) -> Vector ('S n) a -> Vector ('S n) b
  (f:>fs) <*> (x:>xs) = f x :> (fs <*> xs)

instance Monad (Vector 'Z) where
  (>>=) :: Vector 'Z a -> (a -> Vector 'Z b) -> Vector 'Z b
  Nil >>= _ = Nil

instance Monad (Vector n) => Monad (Vector ('S n)) where
  (>>=) :: Vector ('S n) a -> (a -> Vector ('S n) b) -> Vector ('S n) b
  xs >>= f = diagonal $ fmap f xs

instance Foldable (Vector n) where
  foldr :: (a -> b -> b) -> b -> Vector n a -> b
  foldr _ z Nil = z
  foldr f z (x:>xs) = f x (foldr f z xs)

instance Traversable (Vector n) where
  sequenceA :: Applicative f => Vector n (f a) -> f (Vector n a)
  sequenceA Nil = pure Nil
  sequenceA (x:>xs) = (:>) <$> x <*> sequenceA xs

append :: Vector n a -> Vector m a -> Vector (n + m) a
append Nil ys = ys
append (x:>xs) ys = x :> append xs ys

zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith _ Nil Nil = Nil
zipWith f (x:>xs) (y:>ys) = f x y :> zipWith f xs ys

zip :: Vector n a -> Vector n b -> Vector n (a, b)
zip Nil Nil = Nil
zip (x:>xs) (y:>ys) = (x, y) :> zip xs ys

head :: Vector ('S n) a -> a
head (x:>_) = x

tail :: Vector ('S n) a -> Vector n a
tail (_:>xs) = xs

diagonal :: Vector n (Vector n a) -> Vector n a
diagonal Nil = Nil
diagonal ((x:>xs):>xss) = x :> diagonal (tail <$> xss)

index :: Fin n -> Vector n a -> a
index FZ     (x:>xs) = x
index (FS n) (x:>xs) = index n xs

replicate :: SNat n -> a -> Vector n a
replicate SZ _ = Nil
replicate (SS n) a = a :> replicate n a
