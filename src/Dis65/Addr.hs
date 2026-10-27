module Dis65.Addr where

import Data.Word

--------------------------------------------------------------------------------
-- * Addresses

-- | An 'Addr' is simply an 'Int'. The low 16 bits are used for the
-- actual 6502 address. Higher bits can be used to specify a memory
-- bank.
type Addr = Int

word :: Word8 -> Word8 -> Word16
word lo hi = fromIntegral lo + fromIntegral hi * 256

addr8 :: Word8 -> Addr
addr8 = fromIntegral

addr16 :: Word16 -> Addr
addr16 = fromIntegral

-- | Create 16-bit address with bank number.
addr16' :: Int -> Word16 -> Addr
addr16' bank offset = 65536 * bank + fromIntegral offset

-- | Extract the low 16 bits of an address.
addrOffset :: Addr -> Word16
addrOffset = fromIntegral

-- | Extract the high bits of an address, indicating the bank number.
addrBank :: Addr -> Int
addrBank addr = addr `div` 65536

-- | Update the low 16 bits of an address, keeping the high bits
-- (specifying the bank) the same.
sameBank :: Addr -> Word16 -> Addr
sameBank addr target = addr16' (addrBank addr) target

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

ppAddr :: Addr -> String
ppAddr a = ppWord16 (addrOffset a) ++ suffix
  where
    suffix = if b == 0 then "" else "_" ++ show b
    b = addrBank a
