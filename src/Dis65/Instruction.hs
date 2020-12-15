{-# LANGUAGE LambdaCase #-}

module Dis65.Instruction where

import           Data.Array
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Word

-- | Representation of a decoded opcode. This identifies the
-- instruction, but does not include the instruction's argument.
data Op
  = OpReg OpReg
  | OpStack OpStack
  | OpRd OpR AddrMode
  | OpWr OpW AddrMode
  | OpRW OpRW AddrMode
  | OpAcc OpRW
  | OpBranch OpBranch
  | OpBRK
  | OpJSR
  | OpRTI
  | OpRTS
  | OpAbsJMP
  | OpIndJMP
  | OpUndoc Word8

-- | Representation of an instruction, including the 1- or 2-byte
-- argument, if any.
data Instruction
  = Reg OpReg
  | Stack OpStack
  | Read OpR AddrArg
  | Write OpW AddrArg
  | Modify OpRW AddrArg
  | Accumulator OpRW
  | Branch OpBranch Word16
  | BRK
  | JSR Word16
  | RTI
  | RTS
  | AbsJMP Word16
  | IndJMP Word16
  | Undoc Word8

-- | Operations that use the stack.
data OpStack = PHP | PLP | PHA | PLA
  deriving (Eq, Show)

-- | Operations that affect only the register state.
data OpReg
  = CLC | SEC | CLI | SEI | CLV | CLD | SED
  | DEX | TXA | TAX | INX
  | DEY | TYA | TAY | INY
  | TXS | TSX | NOP
  deriving (Eq, Show)

-- | Branch operations.
data OpBranch = BPL | BMI | BVC | BVS | BCC | BCS | BNE | BEQ
  deriving (Eq, Show)

-- | Operations that read from memory.
data OpR = ORA | AND | EOR | ADC | SBC | BIT | CMP | CPX | CPY | LDA | LDX | LDY
  deriving (Eq, Show)

-- | Operations that write to memory.
data OpW = STA | STX | STY
  deriving (Eq, Show)

-- | Memory read/write operations.
data OpRW = ASL | ROL | LSR | ROR | DEC | INC
  deriving (Eq, Show)

-- | Addressing modes.
data AddrMode = InX | Zpg | Imm | Abs | InY | ZpX | ZpY | AbY | AbX
  deriving (Eq, Ord, Show)

-- | Addressing modes with arguments.
data AddrArg
  = IndirectX Word8
  | ZeroPage Word8
  | Immediate Word8
  | Absolute Word16
  | IndirectY Word8
  | ZeroPageX Word8
  | ZeroPageY Word8
  | AbsoluteY Word16
  | AbsoluteX Word16
  deriving (Eq, Ord, Show)

sizeAddrArg :: AddrArg -> Int
sizeAddrArg =
  \case
    IndirectX _ -> 1
    ZeroPage  _ -> 1
    Immediate _ -> 1
    Absolute  _ -> 2
    IndirectY _ -> 1
    ZeroPageX _ -> 1
    ZeroPageY _ -> 1
    AbsoluteY _ -> 2
    AbsoluteX _ -> 2

--------------------------------------------------------------------------------
-- * Pretty printing

ppWord8 :: Word8 -> String
ppWord8 b = [ds !! hi, ds !! lo]
  where (hi, lo) = divMod (fromIntegral b) 16
        ds = "0123456789abcdef"

ppWord16 :: Word16 -> String
ppWord16 w = ppWord8 hi ++ ppWord8 lo
  where hi = fromIntegral (w `div` 0x100)
        lo = fromIntegral w

ppAddrArg :: AddrArg -> String
ppAddrArg =
  \case
    IndirectX z -> "($" ++ ppWord8 z ++ ",X)"
    ZeroPage z -> "$" ++ ppWord8 z
    Immediate z -> "#$" ++ ppWord8 z
    Absolute w -> "$" ++ ppWord16 w
    IndirectY z -> "($" ++ ppWord8 z ++"),Y"
    ZeroPageX z -> "$" ++ ppWord8 z ++ ",X"
    ZeroPageY z -> "$" ++ ppWord8 z ++ ",Y"
    AbsoluteY w -> "$" ++ ppWord16 w ++ ",Y"
    AbsoluteX w -> "$" ++ ppWord16 w ++ ",X"

ppInstruction :: Instruction -> String
ppInstruction =
  \case
    Reg op -> show op
    Stack op -> show op
    Read op arg -> unwords [show op, ppAddrArg arg]
    Write op arg -> unwords [show op, ppAddrArg arg]
    Modify op arg -> unwords [show op, ppAddrArg arg]
    Accumulator op -> show op
    Branch op w -> unwords [show op, "$" ++ ppWord16 w]
    BRK -> "BRK"
    JSR w -> "JSR $" ++ ppWord16 w
    RTI -> "RTI"
    RTS -> "RTS"
    AbsJMP w -> "JMP $" ++ ppWord16 w
    IndJMP w -> "JMP ($" ++ ppWord16 w ++ ")"
    Undoc b -> "byt $" ++ ppWord8 b

--------------------------------------------------------------------------------
-- * Decoding

decodeInstructions :: IntMap Word8 -> IntMap Instruction
decodeInstructions memory =
  IntMap.mapMaybeWithKey (\pc _ -> decodeInstruction memory pc) memory

decodeInstruction :: IntMap Word8 -> Int -> Maybe Instruction
decodeInstruction memory pc =
  do op <- decodeOp <$> IntMap.lookup pc memory
     case op of
       OpReg op ->
         Just (Reg op)
       OpStack op ->
         Just (Stack op)
       OpRd op mode ->
         Read op <$> decodeArg mode
       OpWr op mode ->
         Write op <$> decodeArg mode
       OpRW op mode ->
         Modify op <$> decodeArg mode
       OpAcc op ->
         Just (Accumulator op)
       OpBranch op ->
         Branch op <$> getRel
       OpBRK ->
         Just BRK
       OpJSR ->
         JSR <$> getWord16
       OpRTI ->
         Just RTI
       OpRTS ->
         Just RTS
       OpAbsJMP ->
         AbsJMP <$> getWord16
       OpIndJMP ->
         IndJMP <$> getWord16
       OpUndoc w ->
         Just (Undoc w)
  where
    getWord8 :: Maybe Word8
    getWord8 = IntMap.lookup (pc + 1) memory

    getWord16 :: Maybe Word16
    getWord16 =
      do lo <- IntMap.lookup (pc + 1) memory
         hi <- IntMap.lookup (pc + 2) memory
         pure (fromIntegral lo + fromIntegral hi * 256)

    getInt8 :: Maybe Int8
    getInt8 = fromIntegral <$> getWord8

    getRel :: Maybe Word16
    getRel = (\i -> fromIntegral pc + 2 + fromIntegral i) <$> getInt8

    decodeArg :: AddrMode -> Maybe AddrArg
    decodeArg =
      \case
        InX -> IndirectX <$> getWord8
        Zpg -> ZeroPage <$> getWord8
        Imm -> Immediate <$> getWord8
        Abs -> Absolute <$> getWord16
        InY -> IndirectY <$> getWord8
        ZpX -> ZeroPageX <$> getWord8
        ZpY -> ZeroPageY <$> getWord8
        AbY -> AbsoluteY <$> getWord16
        AbX -> AbsoluteX <$> getWord16

decodeOp :: Word8 -> Op
decodeOp x = opTable ! x

opTable :: Array Word8 Op
opTable = listArray (0x00, 0xff)
  [ OpBRK       , OpRd ORA InX, OpUndoc 0x02, OpUndoc 0x03
  , OpUndoc 0x04, OpRd ORA Zpg, OpRW ASL Zpg, OpUndoc 0x07
  , OpStack PHP , OpRd ORA Imm, OpAcc ASL   , OpUndoc 0x0b
  , OpUndoc 0x08, OpRd ORA Abs, OpRW ASL Abs, OpUndoc 0x0f
  , OpBranch BPL, OpRd ORA InY, OpUndoc 0x12, OpUndoc 0x13
  , OpUndoc 0x0c, OpRd ORA ZpX, OpRW ASL ZpX, OpUndoc 0x17
  , OpReg CLC   , OpRd ORA AbY, OpUndoc 0x1a, OpUndoc 0x1b
  , OpUndoc 0x10, OpRd ORA AbX, OpRW ASL AbX, OpUndoc 0x1f

  , OpJSR       , OpRd AND InX, OpUndoc 0x22, OpUndoc 0x23
  , OpRd BIT Zpg, OpRd AND Zpg, OpRW ROL Zpg, OpUndoc 0x27
  , OpStack PLP , OpRd AND Imm, OpAcc ROL   , OpUndoc 0x2b
  , OpRd BIT Abs, OpRd AND Abs, OpRW ROL Abs, OpUndoc 0x2f
  , OpBranch BMI, OpRd AND InY, OpUndoc 0x32, OpUndoc 0x33
  , OpUndoc 0x34, OpRd AND ZpX, OpRW ROL ZpX, OpUndoc 0x37
  , OpReg SEC   , OpRd AND AbY, OpUndoc 0x3a, OpUndoc 0x3b
  , OpUndoc 0x3c, OpRd AND AbX, OpRW ROL AbX, OpUndoc 0x3f

  , OpRTI       , OpRd EOR InX, OpUndoc 0x42, OpUndoc 0x43
  , OpUndoc 0x44, OpRd EOR Zpg, OpRW LSR Zpg, OpUndoc 0x47
  , OpStack PHA , OpRd EOR Imm, OpAcc LSR   , OpUndoc 0x4b
  , OpAbsJMP    , OpRd EOR Abs, OpRW LSR Abs, OpUndoc 0x4f
  , OpBranch BVC, OpRd EOR InY, OpUndoc 0x52, OpUndoc 0x53
  , OpUndoc 0x54, OpRd EOR ZpX, OpRW LSR ZpX, OpUndoc 0x57
  , OpReg CLI   , OpRd EOR AbY, OpUndoc 0x5a, OpUndoc 0x5b
  , OpUndoc 0x48, OpRd EOR AbX, OpRW LSR AbX, OpUndoc 0x5f

  , OpRTS       , OpRd ADC InX, OpUndoc 0x62, OpUndoc 0x63
  , OpUndoc 0x64, OpRd ADC Zpg, OpRW ROR Zpg, OpUndoc 0x67
  , OpStack PLA , OpRd ADC Imm, OpAcc ROR   , OpUndoc 0x6b
  , OpIndJMP    , OpRd ADC Abs, OpRW ROR Abs, OpUndoc 0x6f
  , OpBranch BVS, OpRd ADC InY, OpUndoc 0x72, OpUndoc 0x73
  , OpUndoc 0x74, OpRd ADC ZpX, OpRW ROR ZpX, OpUndoc 0x77
  , OpReg SEI   , OpRd ADC AbY, OpUndoc 0x7a, OpUndoc 0x7b
  , OpUndoc 0x7c, OpRd ADC AbX, OpRW ROR AbX, OpUndoc 0x7f

  , OpUndoc 0x80, OpWr STA InX, OpUndoc 0x82, OpUndoc 0x83
  , OpWr STY Zpg, OpWr STA Zpg, OpWr STX Zpg, OpUndoc 0x87
  , OpReg DEY   , OpUndoc 0x89, OpReg TXA   , OpUndoc 0x8b
  , OpWr STY Abs, OpWr STA Abs, OpWr STX Abs, OpUndoc 0x8f
  , OpBranch BCC, OpWr STA InY, OpUndoc 0x92, OpUndoc 0x93
  , OpWr STY ZpX, OpWr STA ZpX, OpWr STX ZpY, OpUndoc 0x97
  , OpReg TYA   , OpWr STA AbY, OpReg TXS   , OpUndoc 0x9b
  , OpUndoc 0x9c, OpWr STA AbX, OpUndoc 0x9e, OpUndoc 0x9f

  , OpRd LDY Imm, OpRd LDA InX, OpRd LDX Imm, OpUndoc 0xa3
  , OpRd LDY Zpg, OpRd LDA Zpg, OpRd LDX Zpg, OpUndoc 0xa7
  , OpReg TAY   , OpRd LDA Imm, OpReg TAX   , OpUndoc 0xab
  , OpRd LDY Abs, OpRd LDA Abs, OpRd LDX Abs, OpUndoc 0xaf
  , OpBranch BCS, OpRd LDA InY, OpUndoc 0xb2, OpUndoc 0xb3
  , OpRd LDY ZpX, OpRd LDA ZpX, OpRd LDX ZpY, OpUndoc 0xb7
  , OpReg CLV   , OpRd LDA AbY, OpReg TSX   , OpUndoc 0xbb
  , OpRd LDY AbX, OpRd LDA AbX, OpRd LDX AbY, OpUndoc 0xbf

  , OpRd CPY Imm, OpRd CMP InX, OpUndoc 0xc2, OpUndoc 0xc3
  , OpRd CPY Zpg, OpRd CMP Zpg, OpRW DEC Zpg, OpUndoc 0xc7
  , OpReg INY   , OpRd CMP Imm, OpReg DEX   , OpUndoc 0xcb
  , OpRd CPY Abs, OpRd CMP Abs, OpRW DEC Abs, OpUndoc 0xcf
  , OpBranch BNE, OpRd CMP InY, OpUndoc 0xd2, OpUndoc 0xd3
  , OpUndoc 0xd4, OpRd CMP ZpX, OpRW DEC ZpX, OpUndoc 0xd7
  , OpReg CLD   , OpRd CMP AbY, OpUndoc 0xda, OpUndoc 0xdb
  , OpUndoc 0xdc, OpRd CMP AbX, OpRW DEC AbX, OpUndoc 0xdf

  , OpRd CPX Imm, OpRd SBC InX, OpUndoc 0xe2, OpUndoc 0xe3
  , OpRd CPX Zpg, OpRd SBC Zpg, OpRW INC Zpg, OpUndoc 0xe7
  , OpReg INX   , OpRd SBC Imm, OpReg NOP   , OpUndoc 0xeb
  , OpRd CPX Abs, OpRd SBC Abs, OpRW INC Abs, OpUndoc 0xef
  , OpBranch BEQ, OpRd SBC InY, OpUndoc 0xf2, OpUndoc 0xf3
  , OpUndoc 0xf4, OpRd SBC ZpX, OpRW INC ZpX, OpUndoc 0xf7
  , OpReg SED   , OpRd SBC AbY, OpUndoc 0xfa, OpUndoc 0xfb
  , OpUndoc 0xfc, OpRd SBC AbX, OpRW INC AbX, OpUndoc 0xff
  ]
