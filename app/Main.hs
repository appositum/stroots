{-# LANGUAGE DataKinds #-}

module Main where

import Control.Applicative (liftA2)
import Stroots.Nat
import Stroots.Vector (Vector(..))
import qualified Stroots.Vector as V

import Stroots.List (List(..))
import qualified Stroots.List as L

main :: IO ()
main = do
  print $ 5
  print $ V.zip v1 v2
  print $ V.zipWith (+) v1 v2

v1 :: Vector ('S ('S ('S 'Z))) Int
v1 = 1:-2:-3:- V.empty

v2 :: Vector ('S ('S ('S 'Z))) Int
v2 = 6:-7:-8:- V.empty

{-
v3 = 1:>2:>3:> L.empty
v4 = 6:>7:>8:> L.empty-}
