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
  drmario

-- | No special knowledge about program.
drmario_1 :: IO ()
drmario_1 =
  do bs <- readBytes "roms/drmario_ntsc.nes"
     let
       mem = memAt 0x8000 (take 0x8000 (drop 16 bs))
       instrs = decodeInstructions mem
       overrides = IntMap.empty
       usage0 =
         IntMap.unionsWith (<>)
         [ markType mem 0xfffa TCodePtr
         , markType mem 0xfffc TCodePtr
         , markType mem 0xfffe TCodePtr
         ]

     effs <- computeFinalEffects instrs overrides
     let pcs0 = [0x8005,0xff00,0x8035]
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
drmario_2 :: IO ()
drmario_2 =
  do bs <- readBytes "roms/drmario_ntsc.nes"
     let
       mem = memAt 0x8000 (take 0x8000 (drop 16 bs))
       instrs = decodeInstructions mem
       overrides =
         IntMap.fromList
         [ alwaysBranch instrs 0xb693
         , alwaysBranch instrs 0xb6a0
         , alwaysBranch instrs 0xb732
         , alwaysBranch instrs 0xb749 -- non-trivial always-branch
         , alwaysBranch instrs 0xd270
         , alwaysBranch instrs 0xd274
         , alwaysBranch instrs 0xd278
         , alwaysBranch instrs 0xd3d6
         , alwaysBranch instrs 0xd3de
         , alwaysBranch instrs 0xd3e6
         , alwaysBranch instrs 0xd3f3
         , alwaysBranch instrs 0xdb99
         , alwaysBranch instrs 0xdd86
         -- , alwaysBranch instrs 0xb728 -- to avoid tricky case in stack analysis
         -- , alwaysBranch instrs 0xb736 -- to avoid tricky case in stack analysis
         ]
       usage0 =
         IntMap.unionsWith (<>)
         [ markType mem 0xfffa TCodePtr
         , markType mem 0xfffc TCodePtr
         , markType mem 0xfffe TCodePtr
         ]

     effs <- computeFinalEffects instrs overrides
     let pcs0 = [0x8005,0xff00,0x8035]
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

-- | Add overrides for jump tables and inline arguments.
drmario_3 :: IO ()
drmario_3 =
  do bs <- readBytes "roms/drmario_ntsc.nes"
     let
       mem = memAt 0x8000 (take 0x8000 (drop 16 bs))
       instrs = decodeInstructions mem
       overrides =
         IntMap.fromList
         [ alwaysBranch instrs 0xb693
         , alwaysBranch instrs 0xb6a0
         , alwaysBranch instrs 0xb732
         , alwaysBranch instrs 0xb749 -- non-trivial always-branch
         , alwaysBranch instrs 0xd270
         , alwaysBranch instrs 0xd274
         , alwaysBranch instrs 0xd278
         , alwaysBranch instrs 0xd3d6
         , alwaysBranch instrs 0xd3de
         , alwaysBranch instrs 0xd3e6
         , alwaysBranch instrs 0xd3f3
         , alwaysBranch instrs 0xdb99
         , alwaysBranch instrs 0xdd86
         -- , alwaysBranch instrs 0xb728 -- to avoid tricky case in stack analysis
         -- , alwaysBranch instrs 0xb736 -- to avoid tricky case in stack analysis
         , jumpTable 0xb8ac mem 0x8159 9
         , jumpTable 0xb8ac mem 0x8c75 9
         , jumpTable 0xb8ac mem 0x9bbb 7
         , jumpTable 0xb8ac mem 0xb441 31
         ] <>
         overrideB6C2 instrs
       usage0 =
         IntMap.unionsWith (<>)
         [ markType mem 0xfffa TCodePtr
         , markType mem 0xfffc TCodePtr
         , markType mem 0xfffe TCodePtr
         ]

     effs <- computeFinalEffects instrs overrides
     let pcs0 = [0x8005,0xff00,0x8035]
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

drmario :: IO ()
drmario =
  do bs <- readBytes "roms/drmario_ntsc.nes"
     let
       mem = memAt 0x8000 (take 0x8000 (drop 16 bs))
       instrs = decodeInstructions mem
       overrides =
         IntMap.fromList
         [ alwaysBranch instrs 0xb693
         , alwaysBranch instrs 0xb6a0
         , alwaysBranch instrs 0xb732
         , alwaysBranch instrs 0xb749 -- non-trivial always-branch
         , alwaysBranch instrs 0xd270
         , alwaysBranch instrs 0xd274
         , alwaysBranch instrs 0xd278
         , alwaysBranch instrs 0xd3d6
         , alwaysBranch instrs 0xd3de
         , alwaysBranch instrs 0xd3e6
         , alwaysBranch instrs 0xd3f3
         , alwaysBranch instrs 0xdb99
         , alwaysBranch instrs 0xdd86
         , alwaysBranch instrs 0xb728 -- to avoid tricky case in stack analysis
         , alwaysBranch instrs 0xb736 -- to avoid tricky case in stack analysis
         , jumpTable 0xb8ac mem 0x8159 9
         , jumpTable 0xb8ac mem 0x8c75 9
         , jumpTable 0xb8ac mem 0x9bbb 7
         , jumpTable 0xb8ac mem 0xb441 31
         , overrideIndirectE2 mem
         , overrideBug8DB6
         ] <>
         overrideB6C2 instrs
       usage0 =
         IntMap.unionsWith (<>)
         [ markType mem 0xfffa TCodePtr
         , markType mem 0xfffc TCodePtr
         , markType mem 0xfffe TCodePtr
         -- JSR $b8ac
         , markTypes mem 0x815c (replicate 9 TCodePtr)
         , markTypes mem 0x8c78 (replicate 9 TCodePtr)
         , markTypes mem 0x9bbe (replicate 7 TCodePtr)
         , markTypes mem 0xb444 (replicate 31 TCodePtr)
         -- JSR $b6c2
         , markType mem 0x818e TDataPtr
         , markType mem 0x82d4 TDataPtr
         , markType mem 0x8517 TDataPtr
         , markType mem 0x8527 TDataPtr
         , markType mem 0x8537 TDataPtr
         , markType mem 0x8583 TDataPtr
         , markType mem 0x8592 TDataPtr
         , markType mem 0x859e TDataPtr
         , markType mem 0x85aa TDataPtr
         , markType mem 0x85b9 TDataPtr
         , markType mem 0x85c5 TDataPtr
         , markType mem 0x85d1 TDataPtr
         , markType mem 0x85dd TDataPtr
         , markType mem 0x85e9 TDataPtr
         , markType mem 0x85f5 TDataPtr
         , markType mem 0x8537 TDataPtr
         , markType mem 0x8601 TDataPtr
         , markType mem 0x860d TDataPtr
         , markType mem 0x9871 TDataPtr
         , markType mem 0x9997 TDataPtr
         , markType mem 0x99be TDataPtr
         , markType mem 0x9e81 TDataPtr
         -- other pointer structures
         , markTypes mem 0xa8c2 (replicate 75 TDataPtr)
         -- vectors for sound routines
         , markTypes mem 0xd200 (replicate 55 TCodePtr)
         -- pointers used by subroutine $DD15
         , markTypes mem 0xe018 (replicate 31 TDataPtr)
         -- song data
         , let
             p n = TPtr (replicate n TDataPtr)
             f i j k l = [TByte, TByte, p i, p j, p k, p l]
           in
             markTypes mem 0xe26e $
             f 8 7 5 5 ++
             f 3 2 2 2 ++
             f 10 10 10 10 ++
             f 13 11 9 10 ++
             f 3 0 0 0 ++
             f 6 6 6 6 ++
             f 8 7 6 6 ++
             f 7 6 5 5 ++
             f 4 6 4 4 ++
             f 16 16 16 16 ++
             f 4 4 4 4 ++
             f 5 4 4 0 ++
             f 5 4 4 0
         ]

     -- mapM_ (\(addr, u) -> putStrLn (ppAddr addr ++ " " ++ ppAddrUsage u)) (IntMap.assocs usage0)
     effs <- computeFinalEffects instrs overrides
     let pcs0 = [0x8005,0xff00,0x8035]
     let successors = fmap overrideSuccessors overrides <> allSuccessors instrs
     let instrs' = IntMap.intersectionWith (,) instrs successors
     let usage = computeUsage' instrs' usage0 pcs0
     let statements = disassemble mem usage
     let jsrs = allJSRs $ IntMap.filterWithKey (\k _ -> alreadyExecuted usage k) instrs
     let vectors8157 = [0x981C, 0x9953, 0x816E, 0x8206, 0x9B73, 0x9532, 0x8FEF, 0x958A, 0x9C91]
     let vectors9bb9 = [0x8D56, 0x8C6F, 0x9BD4, 0x9BCF, 0x9700, 0x9BCC, 0x9F87]
     let vectors8c73 = [0x8c8a, 0x8c91, 0x8c91, 0x8c94, 0x9413, 0x91fb, 0x9469, 0x92db, 0x8d4f]
     let roots = IntSet.fromList (pcs0 ++ vectors8157 ++ vectors9bb9 ++ vectors8c73) <> jsrs
     let (entries, regions, edges) = entryPoints successors roots
     ppMemEffectsByPC (ppLabel usage) (fmap (memory' . snd) (IntMap.restrictKeys effs entries))
     putStrLn (replicate 80 '-')
     ppCombined (fmap snd effs) usage regions statements
     putStrLn (replicate 80 '-')
     mapM_ putStrLn $ ppCallGraph effs usage edges

overrideB6C2 :: IntMap Instruction -> IntMap Override
overrideB6C2 = IntMap.mapMaybeWithKey f
  where
    f addr (JSR 0xb6c2) = Just (Override [0xb6c2, addr+5] jsrEffect)
    f _ _ = Nothing
    jsrEffect [] = bottom
    jsrEffect (target : after) =
      let effect = noEffect { subroutines = IntSet.singleton 0xb6c2 }
      in thenFinalEffect effect $ jsrFinalEffect target (mconcat after)

overrideIndirectE2 :: IntMap Word8 -> (Addr, Override)
overrideIndirectE2 mem = (0xd42a, Override addrs mconcat)
  where
    addrs = maybe (error "overrideIndirectE2") id (getVecs mem 0xd200 55)

-- | Ugly hack to patch over what seems like a bug in the source program.
overrideBug8DB6 :: (Addr, Override)
overrideBug8DB6 = (0x8db6, Override [] (const rtsFinalEffect))

ppLabel :: MemUsage -> Addr -> String
ppLabel usage addr =
  fromMaybe "$" (IntMap.lookup addr usage >>= labelPrefix) ++ ppAddr addr

{-
  $00 $01 $05 $06
  $33 $36 $37 $38 $39 $3a $3b $3c $3d $3e
  $42 $44 $45 $46 $47 $48 $49 $4a $4b $4c $4f $50 $51 $52 $53 $54 $55
  $57 $58 $59 $5a $5b $5c $5d $5e $60 $61 $65 $66 $68 $69
  $70 $72 $73 $74 $75 $76 $77 $78 $79 $7a $7b $7d $7e $7f $80 $81 $82 $83 $84 $85 $86 $87 $88 $89 $8a $8b $8e $8f $90 $91 $92 $93 $94 $95 $96 $97 $98 $9a $9b $9c $9d $9e
  $a0 $a1 $a2 $a3 $a4 $a5 $a6 $a7 $a8 $ad
  $e0 $e1 $e2 $e3 $e4 $e6 $e7
  $ea $eb $ec $ed $ee $ef $f5 $fe $ff
  $0300 $030b $0316 $0317 $031e $0324 $0328 $0380 $038b $0396 $0397 $039e $03a4 $03a8
  $067e $067f $0682 $0683 $068a $068b $068c $0690 $0691 $069c $069f $06b4 $06b5 $06b6 $06b7 $06c0 $06c1 $06c8 $06c9 $06ca $06cc
  $06d9 $06de $06df $06e0 $06e1 $06e2 $06e3 $06e4 $06e6 $06e8 $06ea $06f0 $06f1 $06f3 $06f4 $06f5 $06f8 $06f9 $06fa $06fb $06fc $06fd $06ff
  $0700 $0701 $0702 $0703 $0704 $0705 $0725 $0727 $0728 $0729 $072a $072b $072c $072d $072e $0731 $0732 $0733 $0734 $0735 $0736 $0737
  $073c $073d $073e $073f $0740 $0741
  $2000 $2001 $2005 $2006 $2007
  $4000 $4001 $4002 $4003 $4004 $4005 $4006 $4007 $4008 $400a $400b $400c $400e $400f $4010 $4011 $4012 $4013 $4015 $4017
  $bfff $dfff
  ($00),Y ($57),Y ($e0),Y $00,X
  $72,X $75,X $78,X $80,X $a9,X $0200,Y $0500,Y $0580,Y $06a7,Y $06f8,Y
  $4000,Y $4001,Y $4002,Y $4003,Y
  $0103,X $0104,X $0200,X $0201,X $0202,X $0203,X $0204,X $0205,X $0206,X $0207,X $0300,X $0318,X $0380,X $0400,X $0580,X
  $0680,X $0681,X $0690,X $0692,X $0693,X $069a,X $069d,X $06a0,X $06a1,X $06a8,X $06ac,X $06b0,X $06b4,X $06b8,X $06bc,X
  $06c3,X $06c7,X $06c8,X $06cd,X $06d1,X $06d5,X $06da,X $06df,X $06e3,X $06e7,X $06ef,X $06f8,X $0760,X $0761,X $0780,X
  $4000,X $4004,X
-}

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

{-

NES Rom file header (16 bytes)

First 4 bytes are magic numbers to identify file type.
0: 0x4e 'N'
1: 0x45 'E'
2: 0x53 'S'
3: 0x1a DOS end-of-file

4: 0x02 (length of PRG rom in 16K blocks)
5: 0x04 (length of CHR rom in 8K blocks)
6: 0x10 flags: bits7-4 = lower nybble of mapper number
7: 0x00 flags: bits7-4 = high nybble of mapper number
8: 0x00 size of PRG ram in 8K blocks (little-used)
9: 0x00 0=NTSC, 1=PAL
a: 0x00 reserved
b: 0x00 reserved
c: 0x00 reserved
d: 0x00 reserved
e: 0x00 reserved
f: 0x00 reserved

Dr. Mario uses mapper number 1:
$8000-$BFFF: 16K PRG rom
$C000-$FFFF: 16K PRG rom

-}
