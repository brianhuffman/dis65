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
  , allJSRs
  -- * Output
  , ppCallGraph
  , ppCombined
  , ppMemEffectsByPC
  ) where

import           Control.Monad
import           Data.Char (toUpper)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.List
import           Data.Maybe
import           Data.Word

import Dis65.Addr
import Dis65.Effect
import Dis65.Effect.Class
import Dis65.Effect.Reg
import Dis65.Effect.Stack (ppFinalStackEffect')
import Dis65.Backward
import Dis65.Forward
import Dis65.Instruction
import Dis65.Statement

import qualified Dis65.Effect.Mem as Mem

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

allJSRs :: IntMap Instruction -> IntSet
allJSRs instrs =
  IntSet.fromList
  [ fromIntegral target | JSR target <- IntMap.elems instrs ]

--------------------------------------------------------------------------------
-- Fancy output

-- | Output the call graph in graphviz .dot format.
ppCallGraph :: IntMap (Instruction, FinalEffect) -> IntMap AddrUsage -> IntMap IntSet -> [String]
ppCallGraph effs usage edges =
  "digraph calls {" :
  "\tgraph [rankdir=\"LR\"];" :
  "\tgraph [ranksep=2.0];" :
  map ppNode subs ++
  map ppEdge edges' ++
  [ "}" ]
  where
    subs = IntMap.assocs $ IntMap.intersection effs edges
    edges' = [ (a, b) | (a, bs) <- IntMap.assocs edges, b <- IntSet.elems bs ]
    instrs = fmap fst effs
    jsrs = allJSRs $ IntMap.filterWithKey (\k _ -> alreadyExecuted usage k) instrs
    ppEdge (a, b) =
      "\tj_" ++ ppAddr a ++ " -> j_" ++ ppAddr b ++ ";"
    ppNode (a, (_, e)) =
      "\tj_" ++ ppAddr a ++
      " [label=\"" ++ map toUpper (ppAddr a) ++
      "\", shape=" ++ (if normalSubroutine e then "box" else "ellipse") ++
      (if IntSet.notMember a jsrs then ", style=filled" else "") ++
      "];"

-- | Print disassembly together with effect information for all
-- subroutines, as well as usage information for all labels.
ppCombined :: IntMap FinalEffect -> MemUsage -> IntMap Addr -> [Statement] -> IO ()
ppCombined effs usage regions = go Nothing
  where
    getEff a = fromMaybe mempty (IntMap.lookup a effs)

    label :: Word16 -> String
    label w = ppLabel usage (addr16 w)

    pad :: Int -> String -> String
    pad w s
      | n <= w = s ++ replicate (w - n) ' '
      | otherwise = s ++ '\n' : replicate n ' '
      where n = length s

    takeBytes :: Int -> [Statement] -> ([Word8], [Statement])
    takeBytes i (StmtByte _ b : stmts)
      | i > 0 = let (bs, stmts') = takeBytes (i - 1) stmts in (b : bs, stmts')
    takeBytes _ stmts = ([], stmts)

    stmtAddr :: Statement -> Addr
    stmtAddr =
      \case
        StmtLabel a -> a
        StmtDefine a -> a
        StmtCode a _ -> a
        StmtByte a _ -> a
        StmtWord a _ -> a
        StmtSection a -> a

    go :: Maybe Addr -> [Statement] -> IO ()
    go _ [] = pure ()
    go r (stmt : stmts) =
      do let r' = IntMap.lookup (stmtAddr stmt) regions
         case stmt of
           StmtLabel a ->
             do when (r /= r') $ putStrLn $ "\n; " ++ replicate 76 '-'
                when (IntMap.lookup a regions == Just a) $
                  mapM_ putStrLn $ map ("; " ++) (ppFinalEffect label (getEff a))
                putStrLn $
                  pad 48 (ppLabel usage a ++ ":") ++ "; " ++ ppAddrUsage (lookupUsage usage a)
                go r' stmts
           StmtDefine a ->
             do putStrLn $
                  pad 48 (ppLabel usage a ++ " := $" ++ ppAddr a) ++
                  "; " ++ ppAddrUsage (lookupUsage usage a)
                go r' stmts
           StmtCode a i ->
             do let e = getEff a
                putStrLn $
                  pad 48 ("    " ++ ppInstr usage i) ++
                  unwords [";", ppAddr a, ppRegEffect' (registers' e), ppFinalStackEffect' (stack' e)]
                go r' stmts
           StmtByte a b0 ->
             do let (bs, stmts') = takeBytes 7 stmts
                putStrLn $
                  pad 48 ("    .byte " ++ intercalate "," ["$" ++ ppWord8 b | b <- b0 : bs]) ++
                  "; " ++ ppAddr a
                go r' stmts'
           StmtWord a w ->
             do putStrLn $
                  pad 48 ("    .word " ++ ppLabel usage w) ++
                  "; " ++ ppAddr a
                go r' stmts
           StmtSection a ->
             do putStrLn $ "\n.org $" ++ ppAddr a
                go r' stmts

ppLabel :: MemUsage -> Addr -> String
ppLabel usage addr =
  fromMaybe "$" (IntMap.lookup addr usage >>= labelPrefix) ++ ppAddr addr

ppInstr :: MemUsage -> Instruction -> String
ppInstr usage = ppInstruction (ppLabel usage . addr16)

ppMemEffectsByPC :: (Addr -> String) -> IntMap Mem.MemEffect -> IO ()
ppMemEffectsByPC label es =
  mapM_ ppEntry (Map.assocs es')
  where
    rs :: IntMap (Set.Set AddrArg)
    rs = fmap (\(Mem.MemEffect (Mem.ArgSet x) _ _) -> x) es

    ws :: IntMap (Set.Set AddrArg)
    ws = fmap (\(Mem.MemEffect _ (Mem.ArgSet x) _) -> x) es

    es' :: Map.Map AddrArg (IntSet, IntSet)
    es' =
      Map.unionWith (<>)
      (fmap (\x -> (x, mempty)) (trans rs))
      (fmap (\x -> (mempty, x)) (trans ws))

    trans :: Ord a => IntMap (Set.Set a) -> Map.Map a IntSet
    trans xss =
      Map.fromListWith (<>)
      [ (a, IntSet.singleton x) | (x, as) <- IntMap.assocs xss, a <- Set.elems as ]

    ppEntry :: (AddrArg, (IntSet, IntSet)) -> IO ()
    ppEntry (arg, (r, w)) =
      do putStrLn $ ppAddrArg (label . addr16) arg
         putStrLn $ "    " ++ unwords ("READ by:" : map label (IntSet.elems r))
         putStrLn $ "    " ++ unwords ("WRITTEN by:" : map label (IntSet.elems w))
