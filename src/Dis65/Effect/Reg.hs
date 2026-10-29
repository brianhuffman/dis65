{-# LANGUAGE LambdaCase #-}

module Dis65.Effect.Reg where

import Data.Bits
import Data.Word

import Dis65.Effect.Class

--------------------------------------------------------------------------------
-- * Registers

data Reg = A | X | Y | S
  deriving (Eq, Show)

regBit :: Reg -> Int
regBit =
  \case
    A -> 0
    X -> 1
    Y -> 2
    S -> 3

--------------------------------------------------------------------------------
-- * Register sets

newtype Regs = Regs Word8
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

readA, readX, readY, readS :: RegEffect
readA = readReg A
readX = readReg X
readY = readReg Y
readS = readReg S

writeA, writeX, writeY, writeS :: RegEffect
writeA = writeReg A
writeX = writeReg X
writeY = writeReg Y
writeS = writeReg S

--------------------------------------------------------------------------------
-- * Pretty printing

ppRegs :: Regs -> String
ppRegs (Regs mask) = concatMap showReg [A,X,Y,S]
  where showReg r = if testBit mask (regBit r) then show r else "-"

ppRegEffect :: RegEffect -> String
ppRegEffect (RegEffect r w _) = concat [ppRegs r, "/", ppRegs w]

ppRegEffect' :: RegEffect -> String
ppRegEffect' (RegEffect (Regs r) (Regs w) (Regs o)) =
  concatMap showReg [A,X,Y,S]
  where
    showReg b =
      case (testBit r i, testBit w i, testBit o i) of
        (True, _, _) -> show b
        (False, True, True) -> "-"
        (False, False, False) -> "="
        (False, False, True) -> "?"
        (False, True, False) -> "-"
      where i = regBit b
