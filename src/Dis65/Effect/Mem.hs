module Dis65.Effect.Mem where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Dis65.Effect.Class

{-
* Memory effects

For each memory location, register, and status flag:
  - R: the code reads it
  - W: the code overwrites it without reading it
  - RW: the code reads it then writes it

For sequential composition, these side-effects compose like this:
  - R;R = R
  - W;W = W
  - W;R = W

What if we branch to one of two instructions, each of which has a
different set of side-effects? In this case, we want to use parallel
composition of side-effects, which is commutative:
  - R|R = R
  - R|W = RW
  - R|RW = RW
  - W|W = W
  - W|RW = RW

The absence of W means that the state component is always preserved.
So parallel composition should include W if either argument does.

The absence of R means that the state component cannot affect any
instructions. So parallel composition should include R if either
argument does.


0: ignores = yes, preserves = yes
R: ignores = no, preserves = yes
W: ignores = yes, preserves = no
RW: ignores = no, preserves = no

-}

--------------------------------------------------------------------------------
-- * Registers


--------------------------------------------------------------------------------
-- * Memory

-- | Read and write effects for sets of addresses.
data MemEffect = MemEffect !IntSet !IntSet
  deriving (Eq, Show)

instance Effect MemEffect where

  MemEffect r1 w1 +++ MemEffect r2 w2 =
    MemEffect (IntSet.union r1 r2) (IntSet.union w1 w2)

  MemEffect r1 w1 >>> MemEffect r2 w2 =
    MemEffect (IntSet.union r1 (IntSet.difference r2 w1)) (IntSet.union w1 w2)

instance NoEffect MemEffect where
  noEffect = MemEffect IntSet.empty IntSet.empty

readAddr :: Int -> MemEffect
readAddr a = MemEffect (IntSet.singleton a) IntSet.empty

writeAddr :: Int -> MemEffect
writeAddr a = MemEffect IntSet.empty (IntSet.singleton a)

modifyAddr :: Int -> MemEffect
modifyAddr a = MemEffect (IntSet.singleton a) (IntSet.singleton a)
