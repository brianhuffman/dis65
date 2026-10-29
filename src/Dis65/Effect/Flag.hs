{-# LANGUAGE LambdaCase #-}

module Dis65.Effect.Flag
  ( FlagEffect
  , readC, readZ, readI, readD, readV, readN
  , clrC, clrZ, clrI, clrD, clrV, clrN
  , setC, setZ, setI, setD, setV, setN
  , writeC, writeZ, writeI, writeD, writeV, writeN
  , readP, writeP
  , writeNZ, writeNZC
  , ppFlagEffect
  , ppFlagEffect'
  ) where

import Data.Bits
import Data.Word

import Dis65.Effect.Class

--------------------------------------------------------------------------------
-- * Flags

data Flag = C | Z | I | D | V | N
  deriving (Eq, Show)

{-
flagC :: Word8
flagC = bit 0

flagZ :: Word8
flagZ = bit 1

flagI :: Word8
flagI = bit 2

flagD :: Word8
flagD = bit 3

flagV :: Word8
flagV = bit 6

flagN :: Word8
flagN = bit 7
-}

flagBit :: Flag -> Int
flagBit =
  \case
    C -> 0
    Z -> 1
    I -> 2
    D -> 3
    V -> 6
    N -> 7

flagMask :: Flag -> Word8
flagMask f = bit (flagBit f)

--------------------------------------------------------------------------------
-- * Flag effects

-- | Read and write effects for combinations of CPU status flags.
--
-- The type consists of four 8-bit fields packed into a 32-bit word:
-- * Read (bits 0-7)
-- * Clear (bits 8-15)
-- * Set (bits 16-23)
-- * Keep (bits 24-31)
newtype FlagEffect = FlagEffect Word32
  deriving (Eq, Show)

mkFlagEffect :: Word8 -> Word8 -> Word8 -> Word8 -> FlagEffect
mkFlagEffect r c s k =
  FlagEffect $
  sum
  [ fromIntegral r `shiftL` 0
  , fromIntegral c `shiftL` 8
  , fromIntegral s `shiftL` 16
  , fromIntegral k `shiftL` 24
  ]

readMask, clrMask, setMask, keepMask :: FlagEffect -> Word8
readMask (FlagEffect x) = fromIntegral (x `shiftR` 0)
clrMask (FlagEffect x) = fromIntegral (x `shiftR` 8)
setMask (FlagEffect x) = fromIntegral (x `shiftR` 16)
keepMask (FlagEffect x) = fromIntegral (x `shiftR` 24)

instance Bottom FlagEffect where
  bottom = FlagEffect 0x00000000

instance Choice FlagEffect where
  FlagEffect x +++ FlagEffect y = FlagEffect (x .|. y)

instance NoEffect FlagEffect where
  noEffect = mkFlagEffect 0x00 0x00 0x00 0xff

instance Effect FlagEffect where
  e1 >>> e2 = mkFlagEffect r c s k
    where
      r = readMask e1 .|. (keepMask e1 .&. readMask e2)
      c = (clrMask e1 .&. keepMask e2) .|. clrMask e2
      s = (setMask e1 .&. keepMask e2) .|. setMask e2
      k = keepMask e1 .&. keepMask e2

readFlag :: Flag -> FlagEffect
readFlag f = mkFlagEffect (flagMask f) 0x00 0x00 0xff

clrFlag :: Flag -> FlagEffect
clrFlag f = mkFlagEffect 0x00 (flagMask f) 0x00 (complement (flagMask f))

setFlag :: Flag -> FlagEffect
setFlag f = mkFlagEffect 0x00 0x00 (flagMask f) (complement (flagMask f))

writeFlag :: Flag -> FlagEffect
writeFlag f = clrFlag f +++ setFlag f

readC, readZ, readI, readD, readV, readN :: FlagEffect
readC = readFlag C
readZ = readFlag Z
readI = readFlag I
readD = readFlag D
readV = readFlag V
readN = readFlag N

readP :: FlagEffect
readP = mkFlagEffect 0xff 0x00 0x00 0xff

clrC, clrZ, clrI, clrD, clrV, clrN :: FlagEffect
clrC = clrFlag C
clrZ = clrFlag Z
clrI = clrFlag I
clrD = clrFlag D
clrV = clrFlag V
clrN = clrFlag N

setC, setZ, setI, setD, setV, setN :: FlagEffect
setC = setFlag C
setZ = setFlag Z
setI = setFlag I
setD = setFlag D
setV = setFlag V
setN = setFlag N

writeC, writeZ, writeI, writeD, writeV, writeN :: FlagEffect
writeC = writeFlag C
writeZ = writeFlag Z
writeI = writeFlag I
writeD = writeFlag D
writeV = writeFlag V
writeN = writeFlag N

writeP :: FlagEffect
writeP = mkFlagEffect 0x00 0xff 0xff 0x00

writeNZ :: FlagEffect
writeNZ = writeN >>> writeZ

writeNZC :: FlagEffect
writeNZC = writeN >>> writeZ >>> writeC

--------------------------------------------------------------------------------
-- * Pretty printing

ppFlagEffect :: FlagEffect -> String
ppFlagEffect e = unwords $ filter (not . null) [ppFlagEffectRead e, "->", ppFlagEffectWrite e]

ppFlagEffectRead :: FlagEffect -> String
ppFlagEffectRead e = unwords $ filter (not . null) $ map showFlag [C,Z,I,D,V,N]
  where showFlag f = if testBit (readMask e) (flagBit f) then show f else ""

ppFlagEffectWrite :: FlagEffect -> String
ppFlagEffectWrite e = unwords $ filter (not . null) $ map showFlag [C,Z,I,D,V,N]
  where
    showFlag f =
      case (testBit (clrMask e) i, testBit (setMask e) i, testBit (keepMask e) i) of
        (False, False, _) -> ""
        (True, False, False) -> "CL" ++ show f
        (False, True, False) -> "SE" ++ show f
        (True, False, True) -> "CL" ++ show f ++ "?"
        (False, True, True) -> "SE" ++ show f ++ "?"
        (True, True, _) -> show f
      where i = flagBit f

-- | Print a constant-width condensed summary of live flags.
ppFlagEffect' :: FlagEffect -> String
ppFlagEffect' e = concatMap showFlag [C,Z,I,D,V,N]
  where
    (r, c, s, k) = (readMask e, clrMask e, setMask e, keepMask e)
    showFlag f =
      case (testBit r i, testBit c i, testBit s i, testBit k i) of
        (True, _, _, _) -> show f
        (_, True, True, _) -> "-"
        (_, True, False, _) -> "0"
        (_, False, True, _) -> "1"
        (_, False, False, True) -> "="
        (_, False, False, False) -> "?"
      where i = flagBit f
