{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Dis65.Forward where

import           Data.Bits
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Maybe
import           Data.Word

import Dis65.Instruction
import Dis65.Addr

--------------------------------------------------------------------------------
-- * AddrUsage

-- | A set of bit flags that indicate how a particular address is
-- used.
newtype AddrUsage = AddrUsage Word16
  deriving (Eq)

instance Semigroup AddrUsage where
  AddrUsage x <> AddrUsage y = AddrUsage (x .|. y)

instance Monoid AddrUsage where
  mempty = AddrUsage 0
  mappend = (<>)

has :: AddrUsage -> AddrUsage -> Bool
has x y = mappend x y == y

addrExec :: AddrUsage
addrExec = AddrUsage (bit 0)

addrJsr :: AddrUsage
addrJsr = AddrUsage (bit 1)

addrJump :: AddrUsage
addrJump = AddrUsage (bit 2)

addrJumpInd :: AddrUsage
addrJumpInd = AddrUsage (bit 3)

addrBranchBoth :: AddrUsage
addrBranchBoth = addrBranchBackward <> addrBranchForward

addrBranchBackward :: AddrUsage
addrBranchBackward = AddrUsage (bit 4)

addrBranchForward :: AddrUsage
addrBranchForward = AddrUsage (bit 5)

addrRead :: AddrUsage
addrRead = AddrUsage (bit 6)

addrWrite :: AddrUsage
addrWrite = AddrUsage (bit 7)

addrReadIx :: AddrUsage
addrReadIx = AddrUsage (bit 8)

addrWriteIx :: AddrUsage
addrWriteIx = AddrUsage (bit 9)

addrReadInd :: AddrUsage
addrReadInd = AddrUsage (bit 10)

addrWriteInd :: AddrUsage
addrWriteInd = AddrUsage (bit 11)

addrWord :: AddrUsage
addrWord = AddrUsage (bit 12)

addrLabel :: AddrUsage
addrLabel = AddrUsage (bit 13)

--------------------------------------------------------------------------------
-- * Forward analysis

type Memory = IntMap Word8

type MemUsage = IntMap AddrUsage

mark :: Addr -> AddrUsage -> MemUsage -> MemUsage
mark = IntMap.insertWith mappend

markRead :: AddrArg -> MemUsage -> MemUsage
markRead =
  \case
    IndirectX z -> mark (fromIntegral z) addrReadInd
    ZeroPage  z -> mark (fromIntegral z) addrRead
    Immediate _ -> id
    Absolute  w -> mark (fromIntegral w) addrRead
    IndirectY z -> mark (fromIntegral z) addrReadInd
    ZeroPageX z -> mark (fromIntegral z) addrReadIx
    ZeroPageY z -> mark (fromIntegral z) addrReadIx
    AbsoluteY w -> mark (fromIntegral w) addrReadIx
    AbsoluteX w -> mark (fromIntegral w) addrReadIx

markWrite :: AddrArg -> MemUsage -> MemUsage
markWrite =
  \case
    IndirectX z -> mark (fromIntegral z) addrWriteInd
    ZeroPage  z -> mark (fromIntegral z) addrWrite
    Immediate _ -> id
    Absolute  w -> mark (fromIntegral w) addrWrite
    IndirectY z -> mark (fromIntegral z) addrWriteInd
    ZeroPageX z -> mark (fromIntegral z) addrWriteIx
    ZeroPageY z -> mark (fromIntegral z) addrWriteIx
    AbsoluteY w -> mark (fromIntegral w) addrWriteIx
    AbsoluteX w -> mark (fromIntegral w) addrWriteIx

{-
Memory map:

As we process the file, we update another map of addresses.


AddrUsage has various values, based on how that address has been used
in the program:
- execute
- read
- write
- read with index
- write with index
-}

data OpType
  = OpRel -- ^ Branch instructions
  | OpJmp
  | OpJsr
  | OpEnd -- ^ RTS, RTI, BRK
  | Op1 -- ^ 1-byte instructions
  | Op2 -- ^ 2-byte instructions
  | OpAbs AddrUsage
  | OpUndoc

{-
    00  20  40  60  80  a0  c0  e0
0c  nop BIT JMP JMP'STY LDY CPY CPX    $0000      absolute (6c is indirect JMP)
0d  ORA AND EOR ADC STA LDA CMP SBC    $0000      absolute
0e  ASL ROL LSR ROR STX LDX DEC INC    $0000      absolute (read-modify-write)
19  ORA AND EOR ADC STA LDA CMP SBC    $0000,Y    absolute,Y
1c  nop nop nop nop shy LDY nop nop    $0000,X    absolute,X
1d  ORA AND EOR ADC STA LDA CMP SBC    $0000,X    absolute,X
1e  ASL ROL LSR ROR shx LDX DEC INC    $0000,X    absolute,X (r-m-w) (absolute,Y shx/LDX)
    1f  3f  5f  7f  9f  bf  df  ff

Write:
RMW: xxxxx11x

-}


{-
opType :: Word8 -> OpType
opType 0x00 = OpEnd
opType 0x20 = OpJsr
opType 0x40 = OpEnd
opType 0x60 = OpEnd
opType 0x4c = OpJmp
opType x =
  case opTable ! x of
    Imp _ -> Op1
    Imm _ -> Op2
    Zpg _ -> Op2
    Zpx _ -> Op2
    Zpy _ -> Op2
    Abs _ -> OpAbs a
    Abx _ -> OpAbs a
    Aby _ -> OpAbs a
    Ind _ -> OpEnd
    Inx _ -> Op2
    Iny _ -> Op2
    Rel _ -> OpRel
    Undoc -> OpUndoc
  where
    a | x .&. 0xf0 == 0x80 = addrWrite
      | x .&. 0xf0 == 0x90 = addrWriteIx
      | x .&. 0x0f == 0x0e = mappend addrRead addrWrite
      | x .&. 0x1f == 0x1e = mappend addrReadIx addrWriteIx
      | x .&. 0x10 == 0x10 = addrReadIx
      | otherwise          = addrRead
-}

lookupUsage :: MemUsage -> Addr -> AddrUsage
lookupUsage usage addr = fromMaybe mempty (IntMap.lookup addr usage)

alreadyExecuted :: MemUsage -> Addr -> Bool
alreadyExecuted s pc =
  case IntMap.lookup pc s of
    Just a -> has addrExec a
    Nothing -> False

computeUsage ::
  IntMap Instruction ->
  MemUsage ->
  [Addr] ->
  MemUsage
computeUsage _mem s [] = s
computeUsage mem s (pc : pcs)
  | alreadyExecuted s pc = computeUsage mem s pcs
  | otherwise =
      case IntMap.lookup pc mem of
        Nothing -> computeUsage mem s pcs -- jump to external code/crash
        Just instr ->
          case instr of
            Reg _ ->
              computeUsage mem s' (pc + 1 : pcs)
            Stack _ ->
              computeUsage mem s' (pc + 1 : pcs)
            Read _ arg ->
              computeUsage mem (markRead arg s') (pc + 1 + sizeAddrArg arg : pcs)
            Write _ arg ->
              computeUsage mem (markWrite arg s') (pc + 1 + sizeAddrArg arg : pcs)
            Modify _ arg ->
              computeUsage mem (markWrite arg (markRead arg s')) (pc + 1 + sizeAddrArg arg : pcs)
            Accumulator _ ->
              computeUsage mem s' (pc + 1 : pcs)
            Branch _ (fromIntegral -> target) ->
              computeUsage mem (mark target tag s') (pc + 2 : target : pcs)
              where tag = if target <= pc then addrBranchBackward else addrBranchForward
            BRK ->
              computeUsage mem s' pcs
            JSR (fromIntegral -> target) ->
              computeUsage mem (mark target addrJsr s') (pc + 3 : target : pcs)
            RTI ->
              computeUsage mem s' pcs
            RTS ->
              computeUsage mem s' pcs
            AbsJMP (fromIntegral -> target) ->
              computeUsage mem (mark target addrJump s') (target : pcs)
            IndJMP (fromIntegral -> target) ->
              computeUsage mem (mark target addrWord s') pcs
            Undoc _ ->
              computeUsage mem s' pcs

          where
            s' = mark pc addrExec s

computeUsage' ::
  IntMap (Instruction, [Addr]) ->
  MemUsage ->
  [Addr] ->
  MemUsage
computeUsage' _mem s [] = s
computeUsage' mem s (pc : pcs)
  | alreadyExecuted s pc = computeUsage' mem s pcs
  | otherwise =
      case IntMap.lookup pc mem of
        Nothing -> computeUsage' mem s pcs -- jump to external code/crash
        Just (instr, next) ->
          case instr of
            Reg _ ->
              computeUsage' mem s' (next ++ pcs)
            Stack _ ->
              computeUsage' mem s' (next ++ pcs)
            Read _ arg ->
              computeUsage' mem (markRead arg s') (next ++ pcs)
            Write _ arg ->
              computeUsage' mem (markWrite arg s') (next ++ pcs)
            Modify _ arg ->
              computeUsage' mem (markWrite arg (markRead arg s')) (next ++ pcs)
            Accumulator _ ->
              computeUsage' mem s' (pc + 1 : pcs)
            Branch _ (fromIntegral -> target) ->
              computeUsage' mem (mark target tag s') (next ++ pcs)
              where tag = if target <= pc then addrBranchBackward else addrBranchForward
            BRK ->
              computeUsage' mem s' (next ++ pcs)
            JSR (fromIntegral -> target) ->
              computeUsage' mem (mark target addrJsr s') (next ++ pcs)
            RTI ->
              computeUsage' mem s' pcs
            RTS ->
              computeUsage' mem s' pcs
            AbsJMP (fromIntegral -> target) ->
              computeUsage' mem (mark target addrJump s') (next ++ pcs)
            IndJMP (fromIntegral -> target) ->
              computeUsage' mem (mark target addrWord s') (next ++ pcs)
            Undoc _ ->
              computeUsage' mem s' (next ++ pcs)
          where
            s' = mark pc addrExec s

--------------------------------------------------------------------------------
-- * Pretty printing

ppAddrUsage :: AddrUsage -> String
ppAddrUsage a =
  unwords $
  mapMaybe f
  [ (addrExec, "PC")
  , (addrJsr, "JSR")
  , (addrJump, "JMP")
  , (addrJumpInd, "JMP()")
  , (addrBranchBackward, "B-")
  , (addrBranchForward, "B+")
  , (addrRead, "R")
  , (addrWrite, "W")
  , (addrReadIx, "R+")
  , (addrWriteIx, "W+")
  , (addrReadInd, "(R)")
  , (addrWriteInd, "(W)")
  , (addrLabel, "L")
  ]
  where
    f (x, s) = if has x a then Just s else Nothing
