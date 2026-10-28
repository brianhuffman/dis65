module Main (main) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.Maybe
import           Data.Word


import BinaryFile (readBytes)
import Dis65.Addr
import Dis65.Effect
import Dis65.Effect.Class
import Dis65.Backward
import Dis65.Entry
import Dis65.Forward
import Dis65.Instruction
import Dis65.Statement

import Dis65

{-

http://computerarcheology.com/NES/Zelda/index.html

NES Rom file header (16 bytes)

First 4 bytes are magic numbers to identify file type.
0: 0x4e 'N'
1: 0x45 'E'
2: 0x53 'S'
3: 0x1a DOS end-of-file

4: 0x08 (length of PRG rom in 16K blocks)
5: 0x00 (length of CHR rom in 8K blocks)
6: 0x12 flags: bits7-4 = lower nybble of mapper number
7: 0x00 flags: bits7-4 = high nybble of mapper number
8: 0x00 size of PRG ram in 8K blocks (little-used)
9: 0x00 0=NTSC, 1=PAL
a: 0x00 reserved
b: 0x00 reserved
c: 0x00 reserved
d: 0x00 reserved
e: 0x00 reserved
f: 0x00 reserved

$8000-$BFFF: 16K PRG rom 0-6
$C000-$FFFF: 16K PRG rom 7

Zelda NES screen layout (32x30):

 01234567890123456789012345678901
0
1
2  LEVEL-3
3  @@@@@@@@ $nnn/B\/A\  -LIFE-
4  @@@@@@@@     | || |
5  @@@@@@@@ kxn | || |
6  @@@@@@@@ bxn \-/\-/ OOOOO
7
8................................
9................................
0................................
1................................
2................................
3................................
4................................
5................................
6................................
7................................
8...........Playfield............
9.............32x22..............
0................................
1................................
2................................
3................................
4................................
5................................
6................................
7................................
8................................
9................................




-}

-- readBytes :: FilePath -> IO [Word8]

memAt :: Int -> [Word8] -> IntMap Word8
memAt offset bs = IntMap.fromAscList (zip [offset..] bs)

main :: IO ()
main =
  if False then zelda_1 else
  zelda_2

-- | No special knowledge about program.
zelda_1 :: IO ()
zelda_1 =
  do bs <- drop 16 <$> readBytes "roms/zelda_ntsc.nes"
     let
       mem =
         memAt 0x8000 (take 0x4000 bs) <>
         memAt 0xc000 (take 0x4000 (drop (0x4000 * 7) bs))
       instrs = decodeInstructions mem
       overrides = IntMap.empty
       usage0 =
         IntMap.unionsWith (<>)
         [ markType mem 0xfffa TCodePtr
         , markType mem 0xfffc TCodePtr
         -- , markType mem 0xfffe TCodePtr
         ]

     effs <- computeFinalEffects instrs overrides
     let pcs0 = maybe (error "vectors FFFA") id (getVecs mem 0xfffa 2) -- IRQ vector unused
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
zelda_2 :: IO ()
zelda_2 =
  do bs <- drop 16 <$> readBytes "roms/zelda_ntsc.nes"
     let
       banks = [ take 0x4000 (drop (0x4000 * i) bs) | i <- [0..7] ]
       mem =
         memAt 0x08000 (banks !! 0) <>
         memAt 0x18000 (banks !! 1) <>
         memAt 0x28000 (banks !! 2) <>
         memAt 0x38000 (banks !! 3) <>
         memAt 0x48000 (banks !! 4) <>
         memAt 0x58000 (banks !! 5) <>
         memAt 0x68000 (banks !! 6) <>
         memAt 0x78000 (banks !! 7) <>
         memAt 0x0c000 (banks !! 7) <>
         memAt 0x1c000 (banks !! 7) <>
         memAt 0x2c000 (banks !! 7) <>
         memAt 0x3c000 (banks !! 7) <>
         memAt 0x4c000 (banks !! 7) <>
         memAt 0x5c000 (banks !! 7) <>
         memAt 0x6c000 (banks !! 7) <>
         memAt 0x7c000 (banks !! 7)
       instrs = decodeInstructions mem
       -- vs1 = maybe (error "vectors C000") id (getVecs mem 0xc000 14)
       -- vs2 = maybe (error "vectors C03C") id (getVecs mem 0xc03c 34)
       -- vs4 = maybe (error "vectors E6B7") id (getVecs mem 0xe6b7 21)
       -- vectors = vs1 ++ vs2 ++ [0xcb3b,0xcb77,0xcba4,0xcc6b]
       jump_tables =
         [ (0x18721, 9)
         , (0x18a04, 10)
         , (0x18aef, 3)
         , (0x18b38, 5)
         , (0x18bba, 5) -- ?
         , (0x18c71, 4) -- ?
         , (0x1f1a4, 4)
         , (0x2900b, 3)
         , (0x29017, 3)
         , (0x29032, 3)
         , (0x290d7, 2)
         , (0x290e1, 5)
         , (0x2a28f, 7)
         , (0x2a593, 2)
         , (0x2a6af, 3)
         , (0x2aa12, 5)
         , (0x2eb37, 20)
         , (0x481fb, 3)
         , (0x484a4, 6)
         , (0x484e3, 3)
         , (0x49008, 6)
         , (0x4905a, 6)
         , (0x49505, 4)
         , (0x49691, 2)
         , (0x498bf, 3)
         , (0x49903, 5)
         , (0x49d84, 2)
         , (0x49fbe, 4)
         , (0x4a4af, 4)
         , (0x4a672, 9)
         , (0x4abb1, 4) -- ?
         , (0x4ac80, 3)
         , (0x4b1c1, 3)
         , (0x4b238, 2) -- ?
         , (0x4f1a4, 4)
         , (0x4fbb1, 106)
         , (0x4fd7f, 95)
         , (0x5800c, 9)
         , (0x58021, 10)
         , (0x582aa, 7)
         , (0x58426, 8)
         , (0x58be7, 8)
         , (0x58c7f, 13)
         , (0x58e23, 9)
         , (0x59082, 9)
         , (0x59099, 9)
         , (0x5910c, 10)
         , (0x59222, 8)
         , (0x5a860, 5)
         , (0x5b0fe, 11)
         , (0x5e6a3, 9)
         , (0x5e920, 20)
         , (0x5e9e2, 9)
         , (0x5f1a4, 4)
         , (0x68040, 2)
         ]
       switches =
         go 0xc000 (banks !! 7)
         where
           go a (0xa9 : i : 0x20 : 0xac : 0xff : rest) = (a+2, fromIntegral i) : go (a+5) rest
           go a (_ : rest) = go (a+1) rest
           go _ [] = []
       overrides =
         IntMap.fromList $
         -- step 1: notice non-normal E5E2 routine (jump table)
         [ jumpTable (sameBank a 0xe5e2) mem a l | (a, l) <- jump_tables ]

         -- step 2: add always-branch hints to avoid illegal JSRs to unmapped memory
         -- , alwaysBranch instrs 0xf0b5 -- non-normal F0AC routine

         -- , alwaysBranch instrs 0xc651 -- investigating non-normal C64E routine
         -- , alwaysBranch instrs 0xf9a1 -- investigating non-normal F8D4 routine
         -- , (0xcb38, Override vectors mconcat)
         -- , (0xd930, Override [0xd933,0xd953,0xd983,0xd9d3,0xd9f6] mconcat)
         -- , (0xda55, Override [0xe0e6,0xe2af] mconcat)
         -- , (0xe7a0, Override vs4 mconcat)
         -- Bank-switching calls to JSR $FFAC
         ++
         [ (addr16' i a, Override [addr16' b 0xffac, addr16' b a + 3] -- mconcat
             (\es ->
                thenFinalEffect (noEffect { subroutines = IntSet.singleton (addr16' b 0xffac) })
                (jsrFinalEffect (head es) (mconcat (tail es)))
             )
           )
         | (a, b) <- switches, i <- [0..7]
         ]
       usage0 =
         IntMap.unionsWith (<>) $
         [ markType mem 0xfffa TCodePtr
         , markType mem 0xfffc TCodePtr
         -- , markType mem 0xfffe TCodePtr
         ] ++
         [ markTypes mem (a+3) (replicate l TCodePtr) | (a, l) <- jump_tables ]
       -- extra code at EC5D
       -- JSR'd from EBBD

     mapM_ (\(a,i) -> putStrLn (ppWord16 a ++ " -> " ++ show i))  switches
     effs <- computeFinalEffects instrs overrides
     let pcs0 = maybe (error "vectors FFFA") id (getVecs mem 0xfffa 3)
     let successors = fmap overrideSuccessors overrides <> allSuccessors instrs
     let instrs' = IntMap.intersectionWith (,) instrs successors
     let usage1 = computeUsage' instrs' usage0 pcs0
     let normAddr a = if a' < 0x8000 {- || a' >= 0xc000 -} then a' else a
           where a' = a `mod` 65536
     let usage = IntMap.mapKeysWith (<>) normAddr usage1
     let statements = disassemble (IntMap.mapKeys normAddr mem) usage
     let jsrs = allJSRs $ IntMap.filterWithKey (\k _ -> alreadyExecuted usage k) instrs
     let vecs =
           IntSet.fromList $ concat $
           [ (sameBank a . fromIntegral) <$> maybe (error "vectors") id (getVecs mem (a+3) l)
           | (a, l) <- jump_tables ]
     let roots = IntSet.fromList pcs0 <> jsrs <> vecs
     let (entries, regions, edges) = entryPoints successors roots
     ppMemEffectsByPC (ppLabel usage) (fmap (memory' . snd) (IntMap.restrictKeys effs entries))
     putStrLn (replicate 80 '-')
     ppCombined' knownLabels (fmap snd effs) usage regions statements
     putStrLn (replicate 80 '-')
     mapM_ putStrLn $ ppCallGraph' knownLabels effs usage edges

ppLabel :: MemUsage -> Addr -> String
ppLabel usage addr =
  fromMaybe "$" (IntMap.lookup addr usage >>= labelPrefix) ++ ppAddr addr

knownLabels :: IntMap String
knownLabels =
  IntMap.fromList $
  [ (0x0e484, "LE484_NMI")
  , (0x0ff50, "LFF50_RESET") -- never exits
  , (0x0ffac, "LFFAC_PRG_BANK")
  ]

{-

EB30:
switches to bank 2.
jump table to EC1B.
beq $ec2c

JSRs along the way:

    JSR R6e1d
    JSR $eda0 ; JSR $fe98, bank 5
    JSR $f769
    JSR $f36f
    JSR $f4a0
    JSR $f92a
    JSR $f92a
    JSR $f73e




Lec1b_2:                                        ; PC JMP()
    LDA $3c                                     ; ec1b_2 --Y=--=D-- 0
    BNE $ebf8                                   ; ec1d_2 --Y=-Z=D-- 0
    LDA L051e                                   ; ec1f_2 --Y=--=D-- 0
    BEQ $ec2c                                   ; ec22_2 --Y=-Z=D-- 0
...
Lec2c_2:                                        ; PC B+
    LDA $e1                                     ; ec2c_2 --Y=--=D-- -
    BNE $ec58                                   ; ec2e_2 --Y=-Z=D-- -
|    LDA $e0                                     ; ec30_2 --Y=--=D-- -
|    CMP #$02                                    ; ec32_2 A-Y=--=D-- -
|    BEQ $ec49                                   ; ec34_2 --Y=-Z=D-- -
|    LDA $f8                                     ; ec36_2 --Y=--=D-- -
|    AND #$20                                    ; ec38_2 A-Y=--=D-- -
|    BEQ $ec49                                   ; ec3a_2 --Y=-Z=D-- -
|    LDA $e0                                     ; ec3c_2 --Y=--=D-- -
|    EOR #$01                                    ; ec3e_2 A-Y=--=D-- -
|    STA $e0                                     ; ec40_2 A-Y=-Z=D-- -
|    BNE $ec49                                   ; ec42_2 --Y=-Z=D-- -
|    LDA #$0f                                    ; ec44_2 --Y=--=D-- -
|    STA L4015                                   ; ec46_2 A-Y=--=D-- -
|Lec49_2:                                        ; PC B+
|    LDA $e0                                     ; ec49_2 --Y=--=D-- -
|    BEQ $ec58                                   ; ec4b_2 --Y=-Z=D-- -
...
Lec58_2:                                        ; PC B+
    JSR R6e1d                                   ; ec58_2 --Y=--=D-- -
    LDA $fa                                     ; ec5b_2 --Y=--=D-- 0
    AND #$0f                                    ; ec5d_2 A-Y=--=D-- 0
    STA L03f8                                   ; ec5f_2 A-Y=--=D-- 0
    LDA $e1                                     ; ec62_2 --Y=--=D-- 0
    BEQ $ec71                                   ; ec64_2 --Y=-Z=D-- 0
...
Lec71_2:                                        ; PC B+
    LDA $f8                                     ; ec71_2 --Y=--=D-- 0
    AND #$10                                    ; ec73_2 A-Y=--=D-- 0
    BEQ $ec7a                                   ; ec75_2 --Y=-Z=D-- 0
...
Lec7a_2:                                        ; PC B+
    LDA L066c                                   ; ec7a_2 --Y=--=D-- -
    BEQ $ec88                                   ; ec7d_2 --Y=-Z=D-- -
|    LDA L04f0                                   ; ec7f_2 --Y=--=D-- -
|    CLC                                         ; ec82_2 A-Y=--=D-- -
|    ADC #$10                                    ; ec83_2 A-Y=C-=D-- -
|    STA L04f0                                   ; ec85_2 A-Y=--=D-- -
Lec88_2:                                        ; PC B+
    JSR $eda0                                   ; ec88_2 --Y=--=D-- -
    LDA $11                                     ; ec8b_2 --Y=--=D-- -
    BNE $ec92                                   ; ec8d_2 --Y=-Z=D-- -
...
Lec92_2:                                        ; PC B+
    LDA $60                                     ; ec92_2 --Y=--=D-- -
    BNE $ec9e                                   ; ec94_2 --Y=-Z=D-- -
|    LDA $70                                     ; ec96_2 --Y=--=D-- -
|    STA $61                                     ; ec98_2 A-Y=--=D-- -
|    LDA $84                                     ; ec9a_2 --Y=--=D-- -
|    STA $62                                     ; ec9c_2 A-Y=--=D-- -
Lec9e_2:                                        ; PC B+
    LDX #$0d                                    ; ec9e_2 --Y=--=D-- -
    JSR $f769                                   ; eca0_2 -XY=--=D-- -
    LDX #$0e                                    ; eca3_2 --Y=--=D-- -
    JSR $f36f                                   ; eca5_2 -XY=--=D-- -
    LDX #$0f                                    ; eca8_2 --Y=--=D-- -
    JSR $f4a0                                   ; ecaa_2 -XY=--=D-- -
    LDX #$10                                    ; ecad_2 --Y=--=D-- -
    JSR $f92a                                   ; ecaf_2 -XY=--=D-- -
    LDX #$11                                    ; ecb2_2 --Y=--=D-- -
    JSR $f92a                                   ; ecb4_2 -XY=--=D-- -
    LDX #$12                                    ; ecb7_2 --Y=--=D-- -
    JSR $f73e                                   ; ecb9_2 -XY=--=D-- -


Should be unreachable:
R8346_2
f519_2
f73e_2 (jsr from ecb9/ec9e) or f4a0_2 (jsr from ecaa/ec9e)
ec9e_2
ec88_2
ec7a_2
ec71_2
ec58_2
ec49_2
ec2c_2
ec1b_2
(vector from eb30)

f5dd: JSR $8346 (reachable in bank 4 and 2)


E5E2: jump table subroutine.
Index in A, vectors following JSR E5E2 call.

FF98: Set control register of MMC1 mapper. Input in A.

4bit0
-----
CPPMM
|||||
|||++- Mirroring (0: one-screen, lower bank; 1: one-screen, upper bank;
|||               2: vertical; 3: horizontal)
|++--- PRG ROM bank mode (0, 1: switch 32 KB at $8000, ignoring low bit of bank number;
|                         2: fix first bank at $8000 and switch 16 KB bank at $C000;
|                         3: fix last bank at $C000 and switch 16 KB bank at $8000)
+----- CHR ROM bank mode (0: switch 8 KB at a time; 1: switch two separate 4 KB banks)

FFAC: PRG bank switch subroutine.
Index in A.


--------------------------------------------------------------------------------
https://www.computerarcheology.com/NES/Zelda/
--------------------------------------------------------------------------------


Code that writes to the memory region $6000-$7fff:

L7fff
    READ by: Le484 Re8f8 Reb30 Lff50 L8815_1 L8c99_1 Rf21b_1 Rf223_1 Rf231_1 Rf23c_1 Leb62_2 Leb7e_2 Leb86_2 Lebc0_2 Lec1b_2 Reda0_2 Rf23c_2 Rfb74_2 L8d90_4 L8f76_4 L90e4_4 La987_4 Rf213_4 Rf229_4 Rf231_4 Rf23c_4 Lfe80_4 Lfe88_4 Lfe90_4 R82a8_5 L82bb_5 R841e_5 L861a_5 L877b_5 R87c6_5 R8929_5 L8a2f_5 R8ba8_5 L8cda_5 L9080_5 L9097_5 L910a_5 Lb07a_5 Lb0fc_5 Lb117_5 Rb127_5 Lb180_5 Lb199_5 Lb1cd_5 Rb3f4_5 Rb4ac_5 Le9a1_5 Rea3d_5 Lea6b_5 Reda0_5 $edbf_5 Ledc5_5 Rf238_5 Rf23c_5 $f2f4_5 $ff95_7
    WRITTEN by: Le484 Re8f8

All routines, in order by number of addresses written:
Le484 Reb30 Re8f8 R8109_6 Leb96_2 Lb0fc_5 R82a8_5 Le9a1_5 Le96f_5 La6ad_2 L82bb_5 Ra764_2 Ra35d_2 Lec1b_2 Lea6b_5 Le9d8_5 Le9cb_5 La734_2 La2a0_2 L9fc5_2 L910a_5 L9097_5 L9080_5 L905d_2 L9020_2 $e9dd_5 Rfb74_2 Rf23c_5 Rf23c_4 Rf23c_2 Rf23c_1 Rf238_5 Rf231_4 Rf231_1 Rf229_4 Rf223_1 Rf21b_1 Rf213_4 Reda0_5 Reda0_2 Rea3d_5 Rb4ac_5 Rb3f4_5 Rb127_5 R8ba8_5 R8929_5 R87c6_5 R841e_5 Lff50 Lfe90_4 Lfe88_4 Lfe80_4 Ledc5_5 Lebc0_2 Leb86_2 Leb7e_2 Leb62_2 Lb1cd_5 Lb199_5 Lb180_5 Lb117_5 Lb07a_5 Lb02a_5 Lb01d_5 La987_4 L90e4_4 L8f76_4 L8d90_4 L8cda_5 L8c99_1 L8a2f_5 L8815_1 L877b_5 L861a_5 $ff95_7 $f2f4_5 $edbf_5 Rab30_5 Ra8c4_5 Ra3c8_2 Le9c3_5 Lb036_5 Lac20_5 La6b8_2 L9ef0_2 L835c_5 L833d_5 Rea2b_5 Ra437_2 R8006_5 Lea11_5 Lb153_5 Lb14f_5 Lb14b_5 Lb147_5 Lb143_5 La47f_2 L8600_5 L819a_5

$e9dd_5
$edbf_5
$f2f4_5
$ff95_7
L819a_5
L82bb_5
L833d_5
L835c_5
L8600_5
L861a_5
L877b_5
L8815_1
L8a2f_5
L8c99_1
L8cda_5
L8d90_4
L8f76_4
L9020_2
L905d_2
L9080_5
L9097_5
L90e4_4
L910a_5
L9ef0_2
L9fc5_2
La2a0_2
La47f_2
La6ad_2
La6b8_2
La734_2
La987_4
Lac20_5
Lb01d_5
Lb02a_5
Lb036_5
Lb07a_5
Lb0fc_5
Lb117_5
Lb143_5
Lb147_5
Lb14b_5
Lb14f_5
Lb153_5
Lb180_5
Lb199_5 Lb1cd_5 Le484 Le96f_5 Le9a1_5 Le9c3_5 Le9cb_5 Le9d8_5 Lea11_5 Lea6b_5 Leb62_2 Leb7e_2 Leb86_2 Leb96_2 Lebc0_2 Lec1b_2 Ledc5_5 Lfe80_4 Lfe88_4 Lfe90_4 Lff50 R8006_5 R8109_6 R82a8_5 R841e_5 R87c6_5 R8929_5 R8ba8_5 Ra35d_2 Ra3c8_2 Ra437_2 Ra764_2 Ra8c4_5 Rab30_5 Rb127_5 Rb3f4_5 Rb4ac_5 Re8f8 Rea2b_5 Rea3d_5 Reb30 Reda0_2 Reda0_5 Rf213_4 Rf21b_1 Rf223_1 Rf229_4 Rf231_1 Rf231_4 Rf238_5 Rf23c_1 Rf23c_2 Rf23c_4 Rf23c_5 Rfb74_2"


L6001
    WRITTEN by: Le484 Re8f8
L652a
    WRITTEN by: Le484 Re8f8 Reb30 Lff50 L8815_1 L8c99_1 Rf21b_1 Rf223_1 Rf231_1 Rf23c_1 Leb62_2 Leb7e_2 Leb86_2 Lebc0_2 Lec1b_2 Reda0_2 Rf23c_2 Rfb74_2 L8d90_4 L8f76_4 L90e4_4 La987_4 Rf213_4 Rf229_4 Rf231_4 Rf23c_4 Lfe80_4 Lfe88_4 Lfe90_4 R82a8_5 L82bb_5 R841e_5 L861a_5 L877b_5 R87c6_5 R8929_5 L8a2f_5 R8ba8_5 L8cda_5 L9080_5 L9097_5 L910a_5 Lb07a_5 Lb0fc_5 Lb117_5 Rb127_5 Lb180_5 Lb199_5 Lb1cd_5 Rb3f4_5 Rb4ac_5 Le9a1_5 Rea3d_5 Lea6b_5 Reda0_5 $edbf_5 Ledc5_5 Rf238_5 Rf23c_5 $f2f4_5 $ff95_7
L652b
    WRITTEN by: Le484 Re8f8 Reb30 Lff50 L8815_1 L8c99_1 Rf21b_1 Rf223_1 Rf231_1 Rf23c_1 Leb62_2 Leb7e_2 Leb86_2 Lebc0_2 Lec1b_2 Reda0_2 Rf23c_2 Rfb74_2 L8d90_4 L8f76_4 L90e4_4 La987_4 Rf213_4 Rf229_4 Rf231_4 Rf23c_4 Lfe80_4 Lfe88_4 Lfe90_4 R82a8_5 L82bb_5 R841e_5 L861a_5 L877b_5 R87c6_5 R8929_5 L8a2f_5 R8ba8_5 L8cda_5 L9080_5 L9097_5 L910a_5 Lb07a_5 Lb0fc_5 Lb117_5 Rb127_5 Lb180_5 Lb199_5 Lb1cd_5 Rb3f4_5 Rb4ac_5 Le9a1_5 Rea3d_5 Lea6b_5 Reda0_5 $edbf_5 Ledc5_5 Rf238_5 Rf23c_5 $f2f4_5 $ff95_7
L652c
    WRITTEN by: Le484 Re8f8 Reb30 Lff50 L8815_1 L8c99_1 Rf21b_1 Rf223_1 Rf231_1 Rf23c_1 Leb62_2 Leb7e_2 Leb86_2 Lebc0_2 Lec1b_2 Reda0_2 Rf23c_2 Rfb74_2 L8d90_4 L8f76_4 L90e4_4 La987_4 Rf213_4 Rf229_4 Rf231_4 Rf23c_4 Lfe80_4 Lfe88_4 Lfe90_4 R82a8_5 L82bb_5 R841e_5 L861a_5 L877b_5 R87c6_5 R8929_5 L8a2f_5 R8ba8_5 L8cda_5 L9080_5 L9097_5 L910a_5 Lb07a_5 Lb0fc_5 Lb117_5 Rb127_5 Lb180_5 Lb199_5 Lb1cd_5 Rb3f4_5 Rb4ac_5 Le9a1_5 Rea3d_5 Lea6b_5 Reda0_5 $edbf_5 Ledc5_5 Rf238_5 Rf23c_5 $f2f4_5 $ff95_7
L6810
    WRITTEN by: Le484 Re8f8 Le9c3_5
L6825
    WRITTEN by: Le484 Re8f8 Lb01d_5 Lb02a_5 Le9cb_5 Le9d8_5 $e9dd_5
L6827
    WRITTEN by: Le484 Re8f8 R82a8_5 L82bb_5 L833d_5 L835c_5 Ra8c4_5 Rab30_5 Lac20_5 Lb01d_5 Lb02a_5 Lb036_5 Lb0fc_5 Le9a1_5 Le9cb_5 Le9d8_5 $e9dd_5
L6828
    WRITTEN by: Le484 Re8f8 R82a8_5 L82bb_5 L833d_5 L835c_5 Ra8c4_5 Rab30_5 Lac20_5 Lb01d_5 Lb02a_5 Lb036_5 Lb0fc_5 Le9a1_5 Le9cb_5 Le9d8_5 $e9dd_5
L68ba
    WRITTEN by: Le484 Reb30 Leb96_2 R8109_6
L68f2
    WRITTEN by: Le484 Reb30 Leb96_2 R8109_6
L6a09
    WRITTEN by: Le484 Reb30 Leb96_2 R8109_6
L6a3a
    WRITTEN by: Le484 Reb30 Leb96_2 R8109_6
L6a72
    WRITTEN by: Le484 Reb30 Leb96_2 R8109_6
L6b3a
    WRITTEN by: Le484 Reb30 Leb96_2 R8109_6
L6b72
    WRITTEN by: Le484 Reb30 Leb96_2 R8109_6
L6b92
    WRITTEN by: Le484 Re8f8 L8600_5 L9080_5 L9097_5 L910a_5 Lb0fc_5 Lb143_5 Lb147_5 Lb14b_5 Lb14f_5 Lb153_5 Le9cb_5 Le9d8_5 $e9dd_5 Lea11_5 Rea2b_5
L7fff
    WRITTEN by: Le484 Re8f8


L651e,Y
    WRITTEN by: Le484 Re8f8 Reb30 L9020_2 L905d_2 L9fc5_2 La2a0_2 Ra35d_2 La6ad_2 La734_2 Ra764_2 Le96f_5
L6521,Y
    WRITTEN by: Le484 Re8f8 Reb30 L9020_2 L905d_2 L9fc5_2 La2a0_2 Ra35d_2 La6ad_2 La734_2 Ra764_2 Le96f_5
L6524,Y
    WRITTEN by: Le484 Re8f8 Reb30 L9020_2 L905d_2 L9fc5_2 La2a0_2 Ra35d_2 La6ad_2 La734_2 Ra764_2 Le96f_5
L652a,Y
    WRITTEN by: Le484 Re8f8 Reb30 L9020_2 L905d_2 L9ef0_2 L9fc5_2 La2a0_2 Ra35d_2 Ra3c8_2 Ra437_2 La6ad_2 La6b8_2 La734_2 Ra764_2 Le96f_5 Le9c3_5
L6817,Y
    WRITTEN by: Le484 Re8f8 Lea6b_5
L6ba7,Y
    WRITTEN by: Le484 Reb30 Leb96_2 R8109_6
L6d9c,Y
    WRITTEN by: Le484 Reb30 L9ef0_2 Ra3c8_2 La6ad_2 La6b8_2


L6804,X
    WRITTEN by: Le484 Re8f8 La47f_2 Le96f_5
L684b,X
    WRITTEN by: Le484 Reb30 Lec1b_2 R8006_5 L819a_5
L68fe,X
    WRITTEN by: Le484 Reb30 Leb96_2 R8109_6

--------------------------------------------------------------------------------

    Le484 Re8f8
    Le484 Re8f8 Reb30 Lff50 L8815_1 L8c99_1 Rf21b_1 Rf223_1 Rf231_1 Rf23c_1 Leb62_2 Leb7e_2 Leb86_2 Lebc0_2 Lec1b_2 Reda0_2 Rf23c_2 Rfb74_2 L8d90_4 L8f76_4 L90e4_4 La987_4 Rf213_4 Rf229_4 Rf231_4 Rf23c_4 Lfe80_4 Lfe88_4 Lfe90_4 R82a8_5 L82bb_5 R841e_5 L861a_5 L877b_5 R87c6_5 R8929_5 L8a2f_5 R8ba8_5 L8cda_5 L9080_5 L9097_5 L910a_5 Lb07a_5 Lb0fc_5 Lb117_5 Rb127_5 Lb180_5 Lb199_5 Lb1cd_5 Rb3f4_5 Rb4ac_5 Le9a1_5 Rea3d_5 Lea6b_5 Reda0_5 $edbf_5 Ledc5_5 Rf238_5 Rf23c_5 $f2f4_5 $ff95_7
    Le484 Re8f8 Reb30 Lff50 L8815_1 L8c99_1 Rf21b_1 Rf223_1 Rf231_1 Rf23c_1 Leb62_2 Leb7e_2 Leb86_2 Lebc0_2 Lec1b_2 Reda0_2 Rf23c_2 Rfb74_2 L8d90_4 L8f76_4 L90e4_4 La987_4 Rf213_4 Rf229_4 Rf231_4 Rf23c_4 Lfe80_4 Lfe88_4 Lfe90_4 R82a8_5 L82bb_5 R841e_5 L861a_5 L877b_5 R87c6_5 R8929_5 L8a2f_5 R8ba8_5 L8cda_5 L9080_5 L9097_5 L910a_5 Lb07a_5 Lb0fc_5 Lb117_5 Rb127_5 Lb180_5 Lb199_5 Lb1cd_5 Rb3f4_5 Rb4ac_5 Le9a1_5 Rea3d_5 Lea6b_5 Reda0_5 $edbf_5 Ledc5_5 Rf238_5 Rf23c_5 $f2f4_5 $ff95_7
    Le484 Re8f8 Reb30 Lff50 L8815_1 L8c99_1 Rf21b_1 Rf223_1 Rf231_1 Rf23c_1 Leb62_2 Leb7e_2 Leb86_2 Lebc0_2 Lec1b_2 Reda0_2 Rf23c_2 Rfb74_2 L8d90_4 L8f76_4 L90e4_4 La987_4 Rf213_4 Rf229_4 Rf231_4 Rf23c_4 Lfe80_4 Lfe88_4 Lfe90_4 R82a8_5 L82bb_5 R841e_5 L861a_5 L877b_5 R87c6_5 R8929_5 L8a2f_5 R8ba8_5 L8cda_5 L9080_5 L9097_5 L910a_5 Lb07a_5 Lb0fc_5 Lb117_5 Rb127_5 Lb180_5 Lb199_5 Lb1cd_5 Rb3f4_5 Rb4ac_5 Le9a1_5 Rea3d_5 Lea6b_5 Reda0_5 $edbf_5 Ledc5_5 Rf238_5 Rf23c_5 $f2f4_5 $ff95_7
    Le484 Re8f8 Le9c3_5
    Le484 Re8f8 Lb01d_5 Lb02a_5 Le9cb_5 Le9d8_5 $e9dd_5
    Le484 Re8f8 R82a8_5 L82bb_5 L833d_5 L835c_5 Ra8c4_5 Rab30_5 Lac20_5 Lb01d_5 Lb02a_5 Lb036_5 Lb0fc_5 Le9a1_5 Le9cb_5 Le9d8_5 $e9dd_5
    Le484 Re8f8 R82a8_5 L82bb_5 L833d_5 L835c_5 Ra8c4_5 Rab30_5 Lac20_5 Lb01d_5 Lb02a_5 Lb036_5 Lb0fc_5 Le9a1_5 Le9cb_5 Le9d8_5 $e9dd_5
    Le484 Reb30 Leb96_2 R8109_6
    Le484 Reb30 Leb96_2 R8109_6
    Le484 Reb30 Leb96_2 R8109_6
    Le484 Reb30 Leb96_2 R8109_6
    Le484 Reb30 Leb96_2 R8109_6
    Le484 Reb30 Leb96_2 R8109_6
    Le484 Reb30 Leb96_2 R8109_6
    Le484 Re8f8 L8600_5 L9080_5 L9097_5 L910a_5 Lb0fc_5 Lb143_5 Lb147_5 Lb14b_5 Lb14f_5 Lb153_5 Le9cb_5 Le9d8_5 $e9dd_5 Lea11_5 Rea2b_5
    Le484 Re8f8
    Le484 Re8f8 Reb30 L9020_2 L905d_2 L9fc5_2 La2a0_2 Ra35d_2 La6ad_2 La734_2 Ra764_2 Le96f_5
    Le484 Re8f8 Reb30 L9020_2 L905d_2 L9fc5_2 La2a0_2 Ra35d_2 La6ad_2 La734_2 Ra764_2 Le96f_5
    Le484 Re8f8 Reb30 L9020_2 L905d_2 L9fc5_2 La2a0_2 Ra35d_2 La6ad_2 La734_2 Ra764_2 Le96f_5
    Le484 Re8f8 Reb30 L9020_2 L905d_2 L9ef0_2 L9fc5_2 La2a0_2 Ra35d_2 Ra3c8_2 Ra437_2 La6ad_2 La6b8_2 La734_2 Ra764_2 Le96f_5 Le9c3_5
    Le484 Re8f8 Lea6b_5
    Le484 Reb30 Leb96_2 R8109_6
    Le484 Reb30 L9ef0_2 Ra3c8_2 La6ad_2 La6b8_2
    Le484 Re8f8 La47f_2 Le96f_5
    Le484 Reb30 Lec1b_2 R8006_5 L819a_5
    Le484 Reb30 Leb96_2 R8109_6


Are there subroutines that change to a new bank before they return?

e446 -> 5
e4be -> 6
e4e4 -> 5
e55e -> 0
e67f -> 5
e699 -> 5
e7ac -> 5
e7cf -> 5
e7e8 -> 5
e8c4 -> 5
e8d0 -> 4
e8da -> 5
e8fe -> 1
e906 -> 6
e91b -> 5
e953 -> 2
e961 -> 1
e969 -> 2
e971 -> 2
e98b -> 3
e993 -> 1
e99b -> 6
e9a3 -> 5
e9c5 -> 2
e9d2 -> 2
e9da -> 5
ea48 -> 5
eacd -> 5
eadf -> 5
eb19 -> 5
eb24 -> 1
eb32 -> 2
eb64 -> 5
eb78 -> 5
eb80 -> 5
eb88 -> 5
eb90 -> 5
eb9b -> 6
ec05 -> 5
ec26 -> 4
ec4f -> 5
ec68 -> 5
ed2d -> 4
ed38 -> 5
ed46 -> 1
ed64 -> 4
ed8b -> 5
edbc -> 5
efbc -> 1
f023 -> 1
f04f -> 5
f076 -> 5
f087 -> 5
f139 -> 1
f2f1 -> 5
f8bd -> 4
f99c -> 5
fb77 -> 4
fbab -> 1
fd0c -> 5
fd2f -> 5
fd3e -> 4
fd72 -> 1
fe42 -> 1
fe4a -> 1
fe52 -> 1
fe5a -> 1
fe62 -> 1
fe6a -> 1
fe72 -> 1
fe7a -> 1
fe82 -> 1
fe8a -> 1
fe92 -> 1
ff92 -> 7

e484 (NMI)
	j_e484 -> j_ffac;
	j_e484 -> j_ffac_5;
	j_e484 -> j_ffac_6;
	j_e8f8 -> j_ffac_1;
	j_e8f8 -> j_ffac_5;
	j_e8f8 -> j_ffac_6;
	j_eb30 -> j_ffac_2;
	j_ff50 -> j_ffac_7;
	j_e78a_1 -> j_ffac_5;
	j_f026_1 -> j_ffac_5;
	j_f116_1 -> j_ffac_1;
	j_f21e_1 -> j_ffac_1;
	j_f23c_1 -> j_ffac_5;
	j_e78a_2 -> j_ffac_5;
	j_eb62_2 -> j_ffac_5;
	j_eb76_2 -> j_ffac_5;
	j_eb7e_2 -> j_ffac_5;
	j_eb86_2 -> j_ffac_5;
	j_eb8e_2 -> j_ffac_5;
	j_eb96_2 -> j_ffac_6;
	j_ebc0_2 -> j_ffac_5;
	j_ec1b_2 -> j_ffac_1;
	j_ec1b_2 -> j_ffac_4;
	j_ec1b_2 -> j_ffac_5;
	j_eda0_2 -> j_ffac_5;
	j_f23c_2 -> j_ffac_5;
	j_f886_2 -> j_ffac_4;
	j_f92a_2 -> j_ffac_5;
	j_fb74_2 -> j_ffac_1;
	j_fb74_2 -> j_ffac_4;
	j_fb74_2 -> j_ffac_5;
	j_a987_4 -> j_ffac_5;
	j_e679_4 -> j_ffac_5;
	j_e78a_4 -> j_ffac_5;
	j_e862_4 -> j_ffac_5;
	j_efd0_4 -> j_ffac_1;
	j_efd0_4 -> j_ffac_5;
	j_f0e3_4 -> j_ffac_1;
	j_f213_4 -> j_ffac_4;
	j_f229_4 -> j_ffac_4;
	j_f23c_4 -> j_ffac_5;
	j_fe40_4 -> j_ffac_1;
	j_fe48_4 -> j_ffac_1;
	j_fe50_4 -> j_ffac_1;
	j_fe58_4 -> j_ffac_1;
	j_fe60_4 -> j_ffac_1;
	j_fe68_4 -> j_ffac_1;
	j_fe70_4 -> j_ffac_1;
	j_fe78_4 -> j_ffac_1;
	j_fe80_4 -> j_ffac_1;
	j_fe88_4 -> j_ffac_1;
	j_fe90_4 -> j_ffac_1;
	j_861a_5 -> j_ffac_7;
	j_e679_5 -> j_ffac_5;
	j_e68f_5 -> j_ffac_5;
	j_e78a_5 -> j_ffac_5;
	j_e7d7_5 -> j_ffac_5;
	j_e862_5 -> j_ffac_5;
	j_e8c7_5 -> j_ffac_4;
	j_e8d8_5 -> j_ffac_5;
	j_e94b_5 -> j_ffac_1;
	j_e94b_5 -> j_ffac_2;
	j_e96f_5 -> j_ffac_2;
	j_e977_5 -> j_ffac_1;
	j_e977_5 -> j_ffac_3;
	j_e977_5 -> j_ffac_6;
	j_e9a1_5 -> j_ffac_5;
	j_e9c3_5 -> j_ffac_2;
	j_e9cb_5 -> j_ffac_2;
	j_e9d8_5 -> j_ffac_5;
	j_ea3d_5 -> j_ffac_5;
	j_ea6b_5 -> j_ffac_5;
	j_eadd_5 -> j_ffac_1;
	j_eadd_5 -> j_ffac_5;
	j_ed89_5 -> j_ffac_5;
	j_eda0_5 -> j_ffac_5;
	j_ef71_5 -> j_ffac_1;
	j_efd0_5 -> j_ffac_1;
	j_f066_5 -> j_ffac_5;
	j_f07e_5 -> j_ffac_5;
	j_f0e3_5 -> j_ffac_1;
	j_f23c_5 -> j_ffac_5;
	j_ff95_7 -> j_ffac_5;
-}
