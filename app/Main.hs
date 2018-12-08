module Main where

import Stroots.Nat

import Stroots.Fin (Fin(..))
import qualified Stroots.Fin as F

import Stroots.HList (HList(..))
import qualified Stroots.HList as HL

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
v1 = 1 :> 2 :> 3 :> Nil

v2 :: Vector ('S ('S ('S 'Z))) Int
v2 = 6 :> 7 :> 8 :> Nil

v3 :: List Int
v3 = 1 :| 2 :| 3 :| Empty

v4 :: List Int
v4 = 6 :| 7 :| 8 :| Empty

hlist :: HList '[Int, Char, [Int], Vector ('S ('S ('S 'Z))) Int]
hlist = 1 ::: 'e' ::: [1,2,3] ::: (6 :> 7 :> 8 :> V.empty) ::: HNil
