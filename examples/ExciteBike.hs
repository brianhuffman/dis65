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
       vs4 = maybe (error "vectors E6B7") id (getVecs mem 0xe6b7 21)
       vectors = vs1 ++ vs2 ++ [0xcb3b,0xcb77,0xcba4,0xcc6b]
       overrides =
         IntMap.fromList
         [ alwaysBranch instrs 0xc651 -- investigating non-normal C64E routine
         , alwaysBranch instrs 0xf9a1 -- investigating non-normal F8D4 routine
         , (0xcb38, Override vectors mconcat)
         , (0xd930, Override [0xd933,0xd953,0xd983,0xd9d3,0xd9f6] mconcat)
         , (0xda55, Override [0xe0e6,0xe2af] mconcat)
         , (0xe7a0, Override vs4 mconcat)
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
     ppCombined (fmap snd effs) usage regions statements
     putStrLn (replicate 80 '-')
     mapM_ putStrLn $ ppCallGraph effs usage edges

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
