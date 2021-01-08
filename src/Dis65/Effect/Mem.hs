module Dis65.Effect.Mem
  ( MemEffect(..)
  , ArgSet(..)
  , readAddr
  , writeAddr
  , modifyAddr
  , canRead
  , canWrite
  , ppMemEffect
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word

import Dis65.Instruction (AddrArg, ppAddrArg)
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
-- Sets of AddrArg

newtype ArgSet = ArgSet (Set AddrArg)
  deriving (Eq, Show)

empty :: ArgSet
empty = ArgSet Set.empty

single :: AddrArg -> ArgSet
single x = ArgSet (Set.singleton x)

union :: ArgSet -> ArgSet -> ArgSet
union (ArgSet xs) (ArgSet ys) = ArgSet (Set.union xs ys)

inter :: ArgSet -> ArgSet -> ArgSet
inter (ArgSet xs) (ArgSet ys) = ArgSet (Set.intersection xs ys)

diff :: ArgSet -> ArgSet -> ArgSet
diff (ArgSet xs) (ArgSet ys) = ArgSet (Set.difference xs ys)

elems :: ArgSet -> [AddrArg]
elems (ArgSet xs) = Set.elems xs

--------------------------------------------------------------------------------
-- Possibly-infinite set type

data ArgSet' = Univ | Fin !ArgSet
  deriving (Eq, Show)

union' :: ArgSet' -> ArgSet' -> ArgSet'
union' (Fin xs) (Fin ys) = Fin (union xs ys)
union' _ _ = Univ

inter' :: ArgSet' -> ArgSet' -> ArgSet'
inter' Univ y = y
inter' x Univ = x
inter' (Fin xs) (Fin ys) = Fin (inter xs ys)

diff' :: ArgSet -> ArgSet' -> ArgSet
diff' _ Univ = empty
diff' xs (Fin ys) = diff xs ys

--------------------------------------------------------------------------------
-- MemEffect

-- | Read and write effects for sets of addresses.
data MemEffect
  = MemEffect
  { loads :: !ArgSet
  , stores :: !ArgSet
  , overwrites :: !ArgSet'
  }
  deriving (Eq, Show)

instance Choice MemEffect where
  e1 +++ e2 =
    MemEffect
    { loads = union (loads e1) (loads e2)
    , stores = union (stores e1) (stores e2)
    , overwrites = inter' (overwrites e1) (overwrites e2)
    }

instance Effect MemEffect where
  e1 >>> e2 =
    MemEffect
    { loads = union (loads e1) (diff' (loads e2) (overwrites e1))
    , stores = union (stores e1) (stores e2)
    , overwrites = union' (overwrites e1) (overwrites e2)
    }

instance NoEffect MemEffect where
  noEffect = MemEffect empty empty (Fin empty)

instance Bottom MemEffect where
  bottom = MemEffect empty empty Univ

--------------------------------------------------------------------------------
-- Effect constructors

readAddr :: AddrArg -> MemEffect
readAddr a = MemEffect (single a) empty (Fin empty)

writeAddr :: AddrArg -> MemEffect
writeAddr a = MemEffect empty (single a) (Fin (single a))

modifyAddr :: AddrArg -> MemEffect
modifyAddr a = MemEffect (single a) (single a) (Fin (single a))

--------------------------------------------------------------------------------
-- Effect queries

canRead :: AddrArg -> MemEffect -> Bool
canRead a (MemEffect (ArgSet r) _ _) = Set.member a r

canWrite :: AddrArg -> MemEffect -> Bool
canWrite a (MemEffect _ (ArgSet w) _) = Set.member a w

--------------------------------------------------------------------------------
-- Pretty printing

-- | Parameterized by a printer for labels.
ppMemEffect :: (Word16 -> String) -> MemEffect -> [String]
ppMemEffect label (MemEffect r w _) =
  heading "READS" (diff r w) ++
  heading "WRITES" (diff w r) ++
  heading "MODIFIES" (inter r w)
  where
    heading str set
      | null (elems set) = []
      | otherwise = [unwords (str : map (ppAddrArg label) (elems set))]
