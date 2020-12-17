{-# LANGUAGE LambdaCase #-}

module Dis65.Effect.Reg where

import Data.Bits
import Data.Word

import Dis65.Effect.Class

--------------------------------------------------------------------------------
-- * Registers

data Reg = A | X | Y | S | C | Z | I | D | V | N
  deriving (Eq, Show)

regBit :: Reg -> Int
regBit =
  \case
    A -> 0
    X -> 1
    Y -> 2
    S -> 3
    C -> 4
    Z -> 5
    I -> 6
    D -> 7
    V -> 8
    N -> 9

--------------------------------------------------------------------------------
-- * Register sets

newtype Regs = Regs Word16
  deriving (Eq, Show)

instance Semigroup Regs where
  Regs x <> Regs y = Regs (x .|. y)

instance Monoid Regs where
  mempty = Regs 0

allRegs :: Regs
allRegs = Regs (complement 0)

inter :: Regs -> Regs -> Regs
inter (Regs x) (Regs y) = Regs (x .&. y)

diff :: Regs -> Regs -> Regs
diff (Regs x) (Regs y) = Regs (x .&. complement y)

reg :: Reg -> Regs
reg r = Regs (bit (regBit r))

regA, regX, regY, regS :: Regs
regA = reg A
regX = reg X
regY = reg Y
regS = reg S

regC, regZ, regI, regD, regV, regN :: Regs
regC = reg C
regZ = reg Z
regI = reg I
regD = reg D
regV = reg V
regN = reg N

regP :: Regs
regP = regC <> regZ <> regI <> regD <> regV <> regN

--------------------------------------------------------------------------------
-- * Register effects

-- | Read and write effects for combinations of CPU registers.
data RegEffect = RegEffect !Regs !Regs !Regs -- reads, writes, clears
  deriving (Eq, Show)

instance Choice RegEffect where
  RegEffect r1 w1 c1 +++ RegEffect r2 w2 c2 =
    RegEffect (r1 <> r2) (w1 <> w2) (inter c1 c2)

instance Effect RegEffect where
  RegEffect r1 w1 c1 >>> RegEffect r2 w2 c2 =
    RegEffect (r1 <> diff r2 c1) (w1 <> w2) (c1 <> c2)

instance NoEffect RegEffect where
  noEffect = RegEffect mempty mempty mempty

instance Bottom RegEffect where
  bottom = RegEffect mempty mempty allRegs

readReg :: Reg -> RegEffect
readReg r = RegEffect (reg r) mempty mempty

writeReg :: Reg -> RegEffect
writeReg r = RegEffect mempty (reg r) (reg r)

readA, readX, readY, readS, readC, readZ, readI, readD, readV, readN :: RegEffect
readA = readReg A
readX = readReg X
readY = readReg Y
readS = readReg S
readC = readReg C
readZ = readReg Z
readI = readReg I
readD = readReg D
readV = readReg V
readN = readReg N

readP :: RegEffect
readP = RegEffect regP mempty mempty

writeA, writeX, writeY, writeS, writeC, writeZ, writeI, writeD, writeV, writeN :: RegEffect
writeA = writeReg A
writeX = writeReg X
writeY = writeReg Y
writeS = writeReg S
writeC = writeReg C
writeZ = writeReg Z
writeI = writeReg I
writeD = writeReg D
writeV = writeReg V
writeN = writeReg N

writeP :: RegEffect
writeP = RegEffect mempty regP regP

writeNZ :: RegEffect
writeNZ = writeN >>> writeZ

writeNZC :: RegEffect
writeNZC = writeN >>> writeZ >>> writeC

--------------------------------------------------------------------------------
-- * Pretty printing

ppRegs :: Regs -> String
ppRegs (Regs mask) = concatMap showReg [A,X,Y,S,C,Z,I,D,V,N]
  where showReg r = if testBit mask (regBit r) then show r else "-"

ppRegEffect :: RegEffect -> String
ppRegEffect (RegEffect r w _) = concat [ppRegs r, "/", ppRegs w]
