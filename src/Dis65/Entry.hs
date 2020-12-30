{- |
Module: Dis65.Entry
Description: Computing entry points of all instructions.
-}

module Dis65.Entry where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.List (partition)
import           Data.Maybe

import Dis65.Addr

-- | Arguments include the control flow graph (encoded as an 'IntMap'
-- of successor addresses) and an initial set of known entry points.
-- The output consists of the extended set of region entry points, the
-- mapping from program addresses to entry points, and the quotient
-- graph of control-flow edges between regions.
entryPoints :: IntMap [Addr] -> IntSet -> (IntSet, IntMap Addr, IntMap IntSet)
entryPoints successors roots = loop IntMap.empty IntMap.empty [(r, r) | r <- IntSet.elems roots]
  where
    loop :: IntMap Addr -> IntMap IntSet -> [(Addr, Addr)] -> (IntSet, IntMap Addr, IntMap IntSet)
    loop state edges [] = (roots, state, edges)
    loop state edges ((r, pc) : rpcs) =
      case IntMap.lookup pc state of
        Nothing -> loop state' edges' ([ (r, pc') | pc' <- ins ] ++ rpcs)
          where
            succs = fromMaybe [] (IntMap.lookup pc successors)
            (outs, ins) = partition (flip IntSet.member (IntSet.delete r roots)) succs
            state' = IntMap.insert pc r state
            edges' = IntMap.insertWith (<>) r (IntSet.fromList outs) edges
        Just r'
          | r == r' -> loop state edges rpcs
          | otherwise -> entryPoints successors (IntSet.insert pc roots)
