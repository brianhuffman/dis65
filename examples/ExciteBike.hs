{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad
import           Data.Char (toUpper)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.List
import           Data.Maybe
import           Data.Word


import BinaryFile (readBytes)
import Dis65.Addr
import Dis65.Effect
import Dis65.Effect.Class
import Dis65.Effect.Mem (canRead, canWrite)
import Dis65.Effect.Reg
import Dis65.Effect.Stack (ppFinalStackEffect')
import Dis65.Backward
import Dis65.Entry
import Dis65.Forward
import Dis65.Instruction
import Dis65.Statement

import Dis65

import qualified Dis65.Effect.Mem as Mem


-- readBytes :: FilePath -> IO [Word8]

loadAt :: FilePath -> Int -> IO (IntMap Word8)
loadAt path offset = memAt offset <$> readBytes path

memAt :: Int -> [Word8] -> IntMap Word8
memAt offset bs = IntMap.fromAscList (zip [offset..] bs)

main :: IO ()
main =
  excitebike_2

-- | No special knowledge about program.
excitebike_1 :: IO ()
excitebike_1 =
  do bs <- readBytes "roms/excitebike_ntsc.nes"
     let
       mem = memAt 0xc000 (take 0x4000 (drop 16 bs))
       instrs = decodeInstructions mem
       overrides = IntMap.empty
       usage0 =
         IntMap.unionsWith (<>)
         [ markType mem 0xfffa TCodePtr
         , markType mem 0xfffc TCodePtr
         , markType mem 0xfffe TCodePtr
         ]

     effs <- computeFinalEffects instrs overrides
     let pcs0 = [0xc23b,0xc184,0xfff0]
     let successors = fmap overrideSuccessors overrides <> allSuccessors instrs
     let instrs' = IntMap.intersectionWith (,) instrs successors
     let usage = computeUsage' instrs' usage0 pcs0
     let statements = disassemble mem usage
     let jsrs = allJSRs $ IntMap.filterWithKey (\k _ -> alreadyExecuted usage k) instrs
     let roots = IntSet.fromList pcs0 <> jsrs
     let (entries, regions, edges) = entryPoints successors roots
     ppMemEffectsByPC (ppLabel usage) (fmap (memory' . snd) (IntMap.restrictKeys effs entries))
     putStrLn (replicate 80 '-')
     ppCombined (fmap snd effs) usage regions statements
     putStrLn (replicate 80 '-')
     mapM_ putStrLn $ ppCallGraph effs usage edges

-- | Add overrides for always-taken branches.
excitebike_2 :: IO ()
excitebike_2 =
  do bs <- readBytes "roms/excitebike_ntsc.nes"
     let
       mem = memAt 0xc000 (take 0x4000 (drop 16 bs))
       instrs = decodeInstructions mem
       vs1 = maybe (error "vectors C000") id (getVecs mem 0xc000 14)
       vs2 = maybe (error "vectors C03C") id (getVecs mem 0xc03c 34)
       vs4 = maybe (error "vectors E6B9") id (getVecs mem 0xe6b9 16)
       vs5 = maybe (error "vectors E6DB") id (getVecs mem 0xe6db 3)
       vectors = vs1 ++ vs2 ++ [0xcb3b,0xcb77,0xcba4,0xcc6b]
       overrides =
         IntMap.fromList
         [ alwaysBranch instrs 0xc568
         , alwaysBranch instrs 0xc651 -- investigating non-normal C64E routine
         , alwaysBranch instrs 0xc724
         , alwaysBranch instrs 0xcc13
         , alwaysBranch instrs 0xf9a1 -- investigating non-normal F8D4 routine
         -- indirect jump instructions at $cb38, $d930, $da55, $e7a0
         , (0xcb38, Override vectors mconcat)
         , (0xd930, Override [0xd933,0xd953,0xd983,0xd9d3,0xd9f6] mconcat)
         , (0xda55, Override [0xe0e6,0xe2af] mconcat)
         , (0xe7a0, Override (vs4 ++ vs5) mconcat)
         ]
       usage0 =
         IntMap.unionsWith (<>)
         [ markType mem 0xfffa TCodePtr
         , markType mem 0xfffc TCodePtr
         , markType mem 0xfffe TCodePtr
         -- investigating non-normal C2ED routine (indirect jump)
         , markTypes mem 0xc03c (replicate 2 TCodePtr)
         , markTypes mem 0xc040 (replicate 3 TCodePtr)
         , markTypes mem 0xc046 (replicate 6 TCodePtr)
         , markTypes mem 0xc052 (replicate 5 TCodePtr)
         , markTypes mem 0xc05c (replicate 4 TCodePtr)
         , markTypes mem 0xc064 (replicate 6 TCodePtr)
         , markTypes mem 0xc070 (replicate 5 TCodePtr)
         , markTypes mem 0xc07a (replicate 3 TCodePtr)
         -- noticed C2A9 routine (indirect jump)
         , markTypes mem 0xc000 (replicate 14 TCodePtr)
         -- jump table in CB2C routine (indirect jump)
         , IntMap.singleton 0xcb3b addrJumpInd
         , IntMap.singleton 0xcb77 addrJumpInd
         , IntMap.singleton 0xcba4 addrJumpInd
         , IntMap.singleton 0xcc6b addrJumpInd
         -- jump table in D924 routine (indirect jump)
         , IntMap.singleton 0xd933 addrJumpInd
         , IntMap.singleton 0xd953 addrJumpInd
         , IntMap.singleton 0xd983 addrJumpInd
         , IntMap.singleton 0xd9d3 addrJumpInd
         , IntMap.singleton 0xd9f6 addrJumpInd
         -- jump table in DA26 routine (indirect jump)
         , IntMap.singleton 0xe0e6 addrJumpInd
         , IntMap.singleton 0xe2af addrJumpInd
         -- jump table in E794 routine (indirect jump)
         , markTypes mem 0xe6b7 (replicate 21 TCodePtr)
         -- , IntMap.singleton 0xe963 addrJumpInd
         ]
       -- extra code at EC5D
       -- JSR'd from EBBD

     effs <- computeFinalEffects instrs overrides
     let pcs0 = [0xc23b,0xc184,0xfff0]
     let successors = fmap overrideSuccessors overrides <> allSuccessors instrs
     let instrs' = IntMap.intersectionWith (,) instrs successors
     let usage = computeUsage' instrs' usage0 pcs0
     let statements = disassemble mem usage
     let jsrs = allJSRs $ IntMap.filterWithKey (\k _ -> alreadyExecuted usage k) instrs
     let vecs =
           IntSet.fromList $
           maybe (error "vectors") id (getVecs mem 0xc000 14) ++
           maybe (error "vectors") id (getVecs mem 0xc03c 34)
     let roots = IntSet.fromList pcs0 <> jsrs <> vecs <> IntSet.fromList [0xc2ed]
     let (entries, regions, edges) = entryPoints successors roots
     ppMemEffectsByPC (ppLabel usage) (fmap (memory' . snd) (IntMap.restrictKeys effs entries))
     putStrLn (replicate 80 '-')
     ppCombined' knownLabels (fmap snd effs) usage regions statements
     putStrLn (replicate 80 '-')
     putStrLn "Writing `excitebike.dot`."
     writeFile "excitebike.dot" $
       unlines $ ppCallGraph' knownLabels effs usage edges

ppLabel :: MemUsage -> Addr -> String
ppLabel usage addr =
  fromMaybe "$" (IntMap.lookup addr usage >>= labelPrefix) ++ ppAddr addr


ppJSRs :: IntMap (Instruction, FinalEffect) -> IO ()
ppJSRs effs =
  do let instrs = fmap fst effs
     let jsrs = allJSRs instrs
     mapM_ pp1 (IntSet.toList jsrs)
  where
    pp1 :: Int -> IO ()
    pp1 addr =
      case IntMap.lookup addr effs of
        Nothing ->
          putStrLn $ "JSR $" ++ ppAddr addr ++ ": <unmapped>"
        Just (_, e)
          | normalSubroutine e ->
            putStrLn $ "JSR $" ++ ppAddr addr ++ ": Normal\n" ++
            unlines (map (replicate 11 ' ' ++) (ppFinalEffect label e))
          | otherwise ->
            putStrLn $ "JSR $" ++ ppAddr addr ++ ": NOT NORMAL\n" ++
            unlines (map (replicate 11 ' ' ++) (ppFinalEffect label e))
    label :: Word16 -> String
    label w = "$" ++ ppWord16 w

knownLabels :: IntMap String
knownLabels =
  IntMap.fromList $
  [ (0x0380, "L0380_4_LO")
  , (0x0384, "L0380_4_HI")
  , (0x2000, "L2000_PPUCTRL")
  , (0x2001, "L2001_PPUMASK")
  , (0x2002, "L2002_PPUSTATUS")
  , (0x2003, "L2003_OAMADDR")
  , (0x2004, "L2004_OAMDATA")
  , (0x2005, "L2005_PPUSCROLL")
  , (0x2006, "L2006_PPUADDR")
  , (0x2007, "L2007_PPUDATA")
  , (0x4000, "L4000_APU_PULSE1_SETTINGS")
  , (0xc184, "C184_RESET")
  , (0xc1fd, "C1FD_INIT_580_5A3_6E0")
  , (0xc229, "C229_INIT_5A0_5C3")
  , (0xc23b, "C23B_NMI")
  , (0xc32f, "C32F_DISABLE_NMI")
  , (0xc333, "C333_SET_PPUCTRL")
  , (0xc339, "C339_ENABLE_NMI")
  , (0xc346, "C346_V0")
  , (0xc36a, "C36A_INIT")
  , (0xc3af, "C3AF_INIT")
  , (0xc3b1, "C3B1_CLEAR_40_44")
  , (0xc3ef, "C3EF_STA_0200")
  , (0xc454, "C454_RTS")
  , (0xc464, "C464_RTS")
  , (0xc514, "C514_INIT")
  , (0xc518, "C518_INIT")
  , (0xc55e, "C55E_INIT")
  , (0xc562, "C562_INIT")
  , (0xc566, "C566_INIT")
  , (0xc56a, "C56A_INIT")
  , (0xc56e, "C56E_STA_45")
  , (0xc62d, "C62D_INIT")
  , (0xc639, "C639_INIT")
  , (0xc68a, "C68A_INC_08_09")
    -- increment 16-bit value by 1
  , (0xc69a, "C691_INIT")
  , (0xc716, "C716_INIT")
  , (0xc726, "C726_INIT")
  , (0xc869, "C869_INIT")
  , (0xc971, "C971_INC_0300")           -- $0300 += A (return old value in X)
  , (0xca84, "CA84_RTS")
  , (0xcb05, "CB05_RTS")
  , (0xcb13, "CB13_SET_A_FROM_02_46")
  , (0xcc2f, "CC2F_STA_45")
  , (0xcc6a, "CC6A_RTS")
  , (0xce1c, "CE1C_RTS")
  , (0xce82, "CE82_RTS")
  , (0xcf0b, "CF0B_RTS")
  , (0xcf87, "CF87_RTS")
  , (0xcf8b, "CF8B_TEST_3B_3C_3D")
  , (0xcf96, "CF96_SET_00_01")
    -- input X, Y
    -- $80[X] + $C0D6[Y] -> $00
    -- $80[X] + $C0D6[Y] - $C0D7[Y] -> $01
    -- ...
  , (0xcfe9, "CFE9_RTS")
  , (0xcfff, "CFFF_RTS")
  , (0xd11f, "D11F_INIT_0300_0311")
    -- $0300 := #$11
    -- $0301 := #$23
    -- $0302 := #$a0
    -- $0303 := #$0e
    -- $0304..$0311 := #$fe
  , (0xd197, "D197_STA_PPUSCROLL")
  , (0xd1a9, "D1A9_ZERO_PPUSCROLL")
  , (0xd23b, "D23B_CLEAR_SCREENS")
  , (0xd24f, "D24F_SCREEN_OFF")
  , (0xd255, "D255_RESET_PAGE_2")
  , (0xd260, "D260_CLEAR_SCREEN")
  , (0xd310, "D310_DEC_ARRAY_21")
  , (0xd326, "D326_LFSR")
  , (0xd3c1, "D3C1_WRITE_SCREEN_DATA")
  , (0xda15, "DA15_COMPUTE_6C")
  , (0xda69, "DA69_RTS")
  , (0xdbc3, "DBC3_RTS")
  , (0xdc19, "DC19_RTS")
  , (0xdc97, "DC97_03F1_BC_B8")
  , (0xdc9a, "DC9A_SBC_BC_B8")
  , (0xdd37, "DD37_RTS")
  , (0xdd6f, "DD6F_INC_0380")
    -- word ($0384/$0380),X += $038c,X
    -- $01 -= $037c,X
    -- A = $01 + $0384,X
  , (0xdd8c, "DD8C_RTS")
  , (0xdea5, "DEA5_RTS")
  , (0xdf7f, "DF7F_TRIPLE_DIVMOD")
    -- $03d6 := ($02,X)/10
    -- $03d7 := ($02,X)%10
    -- $03d3 := ($01,X)/10
    -- $03d4 := ($01,X)%10
    -- $03d1 := ($00,X)%10
  , (0xdfa2, "DFA2_DIVMOD_10")
    -- (A, X) := (A%10, A/10)
  , (0xdfa4, "DFA4_DIVMOD")
    -- (A, X) := (A%X, A/X)
  , (0xe097, "E097_RTS")
  , (0xe09e, "E09E_RTS")
  , (0xe17e, "E17E_RTS")
  , (0xe269, "E269_RTS")
  , (0xe358, "E358_RTS")
  , (0xe49b, "E49B_PPU_WRITES")
    -- call E4AD with Y=0,1,2
  , (0xe4ad, "E4AD_PPU_WRITE")
    -- $01 := #$02
    -- $00 := $07 + $d8dc,Y  ($d8dc = [00,0c,18])
    -- For Y=0,4,8: ($00),Y := #$f8
  , (0xe6b7, "E6B7_JUMP_VECTORS")
  , (0xe7ff, "E7FF_READ_ARRAY_0400")
  , (0xe803, "E803_READ_ARRAY_0400")
  , (0xe80d, "E80D_SET_PTR_03_04") -- copy vector from $E54E,Y to ($03,$04)
  , (0xe845, "E845_VECTOR_4")
  , (0xe854, "E854_VECTOR_5")
  , (0xe85d, "E85D_VECTOR_2")
  , (0xe86a, "E86A_VECTOR_3")
  , (0xe893, "E893_B4_D4_C8_D0")
  , (0xe8e3, "E8E3_VECTOR_1")
  , (0xe933, "E933_RTS")
  , (0xe934, "E934_VECTOR_6")
  , (0xe963, "E963_VECTOR_0")
  , (0xeac0, "EAC0_INIT")
  , (0xeac4, "EAC4_RTS")
  , (0xec3a, "EC3A_RTS")
  , (0xec3b, "EC3B_COPY_PAGE_5E0_6E0")
  , (0xec98, "EC98_RTS")
  , (0xecbf, "ECBF_INC_MOD_64")
  , (0xecc6, "ECC6_INIT_A")
  , (0xecc8, "ECC8_RTS")
  , (0xecc9, "ECC9_TEST")
  , (0xf676, "F676_FILL_0303") -- fill $0303-$030D with #$fc
  , (0xf68c, "F68C_RTS")
  , (0xf6fb, "F6FB_INC_B1_B0")
  , (0xf821, "F821_05E0_INX_B7_0_C5")
  , (0xf826, "F826_INX_B7_0_C5")
  , (0xf82e, "F82E_SET_PTR_05_06") -- $F063,X -> $05; $F087,X -> $06;
  , (0xf839, "F839_SET_PTR_07_08") -- $F4F5,X -> $07; $F4FA,X -> $08;
  , (0xf844, "F844_AUDIO_MAIN")
  , (0xf86b, "F86B_FANCY_NOTE_PULSE1")
  , (0xf871, "F871_PLAY_NOTE_PULSE1")
  , (0xf873, "F873_PLAY_NOTE")
  , (0xf885, "F885_PLAY_NOTE_PULSE2")
  , (0xf889, "F889_PLAY_NOTE_TRIANGLE")
  , (0xf8d4, "F8D4_AUDIO_1")
  , (0xf942, "F942_INIT")
  , (0xf947, "F947_RTS")
  , (0xfa74, "FA74_AUDIO_2")
  , (0xfc3d, "FC3D_AUDIO_3")
  , (0xfc8d, "FC8D_AUDIO_4")
  ]

{-

NES Rom file header (16 bytes)

First 4 bytes are magic numbers to identify file type.
0: 0x4e 'N'
1: 0x45 'E'
2: 0x53 'S'
3: 0x1a DOS end-of-file

4: 0x01 (length of PRG rom in 16K blocks)
5: 0x01 (length of CHR rom in 8K blocks)
6: 0x01 flags: bits7-4 = lower nybble of mapper number
7: 0x00 flags: bits7-4 = high nybble of mapper number
8: 0x00 size of PRG ram in 8K blocks (little-used)
9: 0x00 0=NTSC, 1=PAL
a: 0x00 reserved
b: 0x00 reserved
c: 0x00 reserved
d: 0x00 reserved
e: 0x00 reserved
f: 0x00 reserved

Excitebike uses mapper number 1:
$8000-$BFFF: 16K PRG rom
$C000-$FFFF: 16K PRG rom

State machine:
Top level state stored in $41:
0:  LC346  0 LC03C
1:  LC395  1 LC040
2:  LC99A  2 LC046
3:  LCB2C  3 LC052
4:  LC465  4 LC05C
5:  LC99A  2 LC046
6:  LCB2C  3 LC052
7:  LC465  4 LC05C
8:  LC3BD  6 LC070
9:  LC97C  5 LC064
10: LC99A  2 LC046
11: LCB2C  3 LC052
12: LC43D  7 LC07A
13: LC41C  8 LC076


-}
