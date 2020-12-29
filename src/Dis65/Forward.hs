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

addrLabel :: AddrUsage
addrLabel = AddrUsage (bit 0)

addrExec :: AddrUsage
addrExec = AddrUsage (bit 1)

addrJsr :: AddrUsage
addrJsr = AddrUsage (bit 2)

addrJump :: AddrUsage
addrJump = AddrUsage (bit 3)

addrBranch :: AddrUsage
addrBranch = AddrUsage (bit 4)

addrVector :: AddrUsage
addrVector = AddrUsage (bit 5)

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

loop ::
  IntMap Instruction ->
  MemUsage ->
  [Addr] ->
  MemUsage
loop _mem s [] = s
loop mem s (pc : pcs)
  | alreadyExecuted s pc = loop mem s pcs
  | otherwise =
      case IntMap.lookup pc mem of
        Nothing -> loop mem s pcs -- jump to external code/crash
        Just instr ->
          case instr of
            Reg _ ->
              loop mem s' (pc + 1 : pcs)
            Stack _ ->
              loop mem s' (pc + 1 : pcs)
            Read _ arg ->
              loop mem (markRead arg s') (pc + 1 + sizeAddrArg arg : pcs)
            Write _ arg ->
              loop mem (markWrite arg s') (pc + 1 + sizeAddrArg arg : pcs)
            Modify _ arg ->
              loop mem (markWrite arg (markRead arg s')) (pc + 1 + sizeAddrArg arg : pcs)
            Accumulator _ ->
              loop mem s' (pc + 1 : pcs)
            Branch _ (fromIntegral -> target) ->
              loop mem (mark target (addrLabel <> addrBranch) s') (pc + 2 : target : pcs)
            BRK ->
              loop mem s' pcs
            JSR (fromIntegral -> target) ->
              loop mem (mark target (addrLabel <> addrJsr) s') (pc + 3 : target : pcs)
            RTI ->
              loop mem s' pcs
            RTS ->
              loop mem s' pcs
            AbsJMP (fromIntegral -> target) ->
              loop mem (mark target (addrLabel <> addrJump) s') (target : pcs)
            IndJMP (fromIntegral -> target) ->
              loop mem (mark target addrVector s') pcs
            Undoc _ ->
              loop mem s' pcs

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
  , (addrBranch, "B")
  , (addrVector, "JMP()")
  , (addrRead, "R")
  , (addrWrite, "W")
  , (addrReadIx, "R+")
  , (addrWriteIx, "W+")
  , (addrReadInd, "(R)")
  , (addrWriteInd, "(W)")
  ]
  where
    f (x, s) = if has x a then Just s else Nothing
