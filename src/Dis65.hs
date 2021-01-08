{-# LANGUAGE LambdaCase #-}

module Dis65
  ( Type(..)
  , markType
  , markTypes
  -- * Overrides
  , alwaysBranch
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
