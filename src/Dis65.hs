{-# LANGUAGE LambdaCase #-}

module Dis65
  ( Type(..)
  , markType
  , markTypes
  -- * Overrides
  , alwaysBranch
  , jumpTable
  -- * Utilities
  , getVecs
  ) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Word

import Dis65.Addr
import Dis65.Effect
import Dis65.Effect.Class
import Dis65.Backward
import Dis65.Forward
import Dis65.Instruction

--------------------------------------------------------------------------------
-- Pointer structures in memory

data Type = TByte | TWord | TPtr [Type] | TDataPtr | TCodePtr

markType :: IntMap Word8 -> Addr -> Type -> IntMap AddrUsage
markType mem addr =
  \case
    TByte -> IntMap.singleton addr mempty
    TWord -> IntMap.singleton addr addrWord
    TPtr ts ->
      IntMap.insertWith (<>) addr addrWord $
      case getWord of
        Nothing -> IntMap.empty
        Just addr' ->
          IntMap.insertWith (<>) (addr16 addr') addrLabel $
          markTypes mem (addr16 addr') ts
    TDataPtr ->
      IntMap.insertWith (<>) addr addrWord $
      case getWord of
        Nothing -> IntMap.empty
        Just addr' ->
          IntMap.singleton (addr16 addr') addrLabel
    TCodePtr ->
      IntMap.insertWith (<>) addr addrWord $
      case getWord of
        Nothing -> IntMap.empty
        Just addr' ->
          IntMap.singleton (addr16 addr') addrJumpInd
  where
    getWord :: Maybe Word16
    getWord =
      do lo <- IntMap.lookup addr mem
         hi <- IntMap.lookup (addr + 1) mem
         Just (word lo hi)

markTypes :: IntMap Word8 -> Addr -> [Type] -> IntMap AddrUsage
markTypes _ _ [] = mempty
markTypes mem addr (TByte : ts) = markTypes mem (addr + 1) ts
markTypes mem addr (t : ts) =
  IntMap.unionWith (<>) (markType mem addr t) (markTypes mem (addr + 2) ts)

--------------------------------------------------------------------------------
-- Common overrides

-- | Define an override for a branch instruction so that it will always be taken.
alwaysBranch :: IntMap Instruction -> Addr -> (Addr, Override)
alwaysBranch instrs pc =
  case IntMap.lookup pc instrs of
    Just (Branch op target) -> (pc, Override [addr16 target] (branchEffect op))
    _ -> error "alwaysBranch: not a valid branch instruction"
  where
    branchEffect op es =
      let effect = noEffect { registers = doOpBranch op, branch = True }
      in thenFinalEffect effect (mconcat es)

-- | Override for a subroutine that implements a jump table. The table
-- of vectors is expected to start just after the JSR instruction.
jumpTable ::
  -- | Address of jump table subroutine
  Addr ->
  -- | Memory contents
  IntMap Word8 ->
  -- | Address of JSR instruction
  Addr ->
  -- | Number of vectors in jump table
  Int ->
  (Addr, Override)
jumpTable sr mem base len =
  case getVecs mem (base+1) (len+1) of
    Just (t : ts) | t == sr -> (base, Override (t : ts) jumpTableEffect)
    _ -> error "jumpTable: invalid memory contents"
  where
    jumpTableEffect [] = bottom
    jumpTableEffect (_ : es) =
      let effect = noEffect { subroutines = IntSet.singleton sr }
      in thenFinalEffect effect $ mconcat es

--------------------------------------------------------------------------------
-- Utility functions

getVecs :: IntMap Word8 -> Addr -> Int -> Maybe [Addr]
getVecs _ _ 0 = Just []
getVecs mem v n =
  do lo <- IntMap.lookup v mem
     hi <- IntMap.lookup (v+1) mem
     let t = addr16 (word lo hi)
     ts <- getVecs mem (v+2) (n-1)
     pure (t : ts)
