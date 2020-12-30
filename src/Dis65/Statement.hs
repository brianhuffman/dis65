module Dis65.Statement where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Word

import Dis65.Addr
import Dis65.Instruction
import Dis65.Forward

labelPrefix :: AddrUsage -> Maybe String
labelPrefix u
  | has addrJsr u            = Just "R"
  | has addrJump u           = Just "J"
  | has addrJumpInd u        = Just "K"
  | has addrBranchBoth u     = Just "W"
  | has addrBranchBackward u = Just "U"
  | has addrBranchForward u  = Just "V"
  | has addrReadIx u         = Just "L"
  | has addrWriteIx u        = Just "L"
  | has addrRead u           = Just "L"
  | has addrWrite u          = Just "L"
  | has addrLabel u          = Just "L"
  | otherwise                = Nothing

needsLabel :: AddrUsage -> Bool
needsLabel u = not (has u addrWord)

data Statement
  = StmtLabel Addr
  | StmtDefine Addr
  | StmtCode Addr Instruction
  | StmtByte Addr Word8
  | StmtWord Addr Addr
  | StmtSection Addr

disassemble :: IntMap Word8 -> MemUsage -> [Statement]
disassemble mem usage = go False 0x0000
  where
    go :: Bool -> Int -> [Statement]
    go _rom addr | addr > 0xffff = []
    go rom addr =
      case ordinaryInstruction mem usage addr of
        Just instr ->
          sections ++ map StmtLabel labels ++
          StmtCode addr instr :
          go True (addr + sizeInstruction instr)
        Nothing ->
          case ordinaryWord mem usage addr of
            Just w ->
              sections ++ map StmtLabel labels ++
              StmtWord addr w :
              go True (addr + 2)
            Nothing ->
              case IntMap.lookup addr mem of
                Just b ->
                  sections ++ map StmtLabel labels ++
                  StmtByte addr b :
                  go True (addr + 1)
                Nothing ->
                  map (if rom then StmtLabel else StmtDefine) labels ++
                  go False (addr + 1)
      where
        sections = if rom then [] else [StmtSection addr]
        labels =
          case labelPrefix (lookupUsage usage addr) of
            Just _ -> [addr]
            Nothing -> []

ordinaryInstruction :: IntMap Word8 -> IntMap AddrUsage -> Addr -> Maybe Instruction
ordinaryInstruction mem usage addr =
  do instr <- decodeInstruction mem addr
     let size = sizeInstruction instr
     let addrs = tail (take size [addr..])
     let tags = map (lookupUsage usage) addrs
     if alreadyExecuted usage addr && mconcat tags == mempty
       then Just instr
       else Nothing

ordinaryWord :: IntMap Word8 -> IntMap AddrUsage -> Addr -> Maybe Addr
ordinaryWord mem usage addr =
  do lo <- IntMap.lookup addr mem
     hi <- IntMap.lookup (addr + 1) mem
     let tag = lookupUsage usage addr
     let tag' = lookupUsage usage (addr + 1)
     if has addrWord tag && tag' == mempty
       then Just (addr16 (word lo hi))
       else Nothing
