module Dis65.Addr where

import Data.Word

type Addr = Int

word :: Word8 -> Word8 -> Word16
word lo hi = fromIntegral lo + fromIntegral hi * 256

addr8 :: Word8 -> Addr
addr8 = fromIntegral

addr16 :: Word16 -> Addr
addr16 = fromIntegral

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
ppAddr a = ppWord16 (fromIntegral a)
