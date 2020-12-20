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
-- * AddrState

-- | A set of bit flags that indicate how a particular address is
-- used.
newtype AddrState = AddrState Word8
  deriving (Eq)

instance Semigroup AddrState where
  AddrState x <> AddrState y = AddrState (x .|. y)

instance Monoid AddrState where
  mempty = AddrState 0
  mappend = (<>)

has :: AddrState -> AddrState -> Bool
has x y = mappend x y == y

addrJump :: AddrState
addrJump = AddrState (bit 0)

addrExec :: AddrState
addrExec = AddrState (bit 1)

addrRead :: AddrState
addrRead = AddrState (bit 2)

addrWrite :: AddrState
addrWrite = AddrState (bit 3)

addrReadIx :: AddrState
addrReadIx = AddrState (bit 4)

addrWriteIx :: AddrState
addrWriteIx = AddrState (bit 5)

addrReadInd :: AddrState
addrReadInd = AddrState (bit 6)

addrWriteInd :: AddrState
addrWriteInd = AddrState (bit 7)

--------------------------------------------------------------------------------
-- * Forward analysis

type Memory = IntMap Word8

type MemState = IntMap AddrState

mark :: Addr -> AddrState -> MemState -> MemState
mark = IntMap.insertWith mappend

markRead :: AddrArg -> MemState -> MemState
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

markWrite :: AddrArg -> MemState -> MemState
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


AddrState has various values, based on how that address has been used
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
  | OpAbs AddrState
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

alreadyExecuted :: MemState -> Addr -> Bool
alreadyExecuted s pc =
  case IntMap.lookup pc s of
    Just a -> has addrExec a
    Nothing -> False

loop ::
  IntMap Instruction ->
  MemState ->
  [Addr] ->
  MemState
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
              loop mem (mark target addrJump s') (pc + 2 : target : pcs)
            BRK ->
              loop mem s' pcs
            JSR (fromIntegral -> target) ->
              loop mem (mark target addrJump s') (pc + 3 : target : pcs)
            RTI ->
              loop mem s' pcs
            RTS ->
              loop mem s' pcs
            AbsJMP (fromIntegral -> target) ->
              loop mem (mark target addrJump s') (target : pcs)
            IndJMP _ ->
              loop mem s' pcs
            Undoc _ ->
              loop mem s' pcs

          where
            s' = mark pc addrExec s

--------------------------------------------------------------------------------
-- * Pretty printing

ppAddrState :: AddrState -> String
ppAddrState a =
  unwords $
  mapMaybe f
  [ (addrJump, "Label")
  , (addrExec, "PC")
  , (addrRead, "R")
  , (addrWrite, "W")
  , (addrReadIx, "R+")
  , (addrWriteIx, "W+")
  , (addrReadInd, "(R)")
  , (addrWriteInd, "(W)")
  ]
  where
    f (x, s) = if has x a then Just s else Nothing
