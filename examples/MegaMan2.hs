module Main where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Maybe
import           Data.Word

import qualified Dis65.Effect.Stack as Stack

import BinaryFile (readBytes)
import Dis65.Addr
import Dis65.Effect
import Dis65.Effect.Class
import Dis65.Effect.Mem (canRead, canWrite)
import Dis65.Backward
import Dis65.Entry
import Dis65.Forward
import Dis65.Instruction
import Dis65.Statement

import Dis65

-- readBytes :: FilePath -> IO [Word8]

loadAt :: FilePath -> Int -> IO (IntMap Word8)
loadAt path offset = memAt offset <$> readBytes path

memAt :: Int -> [Word8] -> IntMap Word8
memAt offset bs = IntMap.fromAscList (zip [offset..] bs)

main :: IO ()
main =
  megaman2_1

-- | No special knowledge about program.
megaman2_1 :: IO ()
megaman2_1 =
  do bs <- drop 16 <$> readBytes "roms/megaman_2_ntsc.nes"
     let
       banks = [ take 0x4000 (drop (0x4000 * i) bs) | i <- [0..15] ]
       mem =
         memAt 0x08000 (banks !! 0) <>
         memAt 0x18000 (banks !! 1) <>
         memAt 0x28000 (banks !! 2) <>
         memAt 0x38000 (banks !! 3) <>
         memAt 0x48000 (banks !! 4) <>
         memAt 0x58000 (banks !! 5) <>
         memAt 0x68000 (banks !! 6) <>
         memAt 0x78000 (banks !! 7) <>
         memAt 0x88000 (banks !! 8) <>
         memAt 0x98000 (banks !! 9) <>
         memAt 0xa8000 (banks !! 10) <>
         memAt 0xb8000 (banks !! 11) <>
         memAt 0xc8000 (banks !! 12) <>
         memAt 0xd8000 (banks !! 13) <>
         memAt 0xe8000 (banks !! 14) <>
         memAt 0xf8000 (banks !! 15) <>
         memAt 0x0c000 (banks !! 15) <>
         memAt 0x1c000 (banks !! 15) <>
         memAt 0x2c000 (banks !! 15) <>
         memAt 0x3c000 (banks !! 15) <>
         memAt 0x4c000 (banks !! 15) <>
         memAt 0x5c000 (banks !! 15) <>
         memAt 0x6c000 (banks !! 15) <>
         memAt 0x7c000 (banks !! 15) <>
         memAt 0x8c000 (banks !! 15) <>
         memAt 0x9c000 (banks !! 15) <>
         memAt 0xac000 (banks !! 15) <>
         memAt 0xbc000 (banks !! 15) <>
         memAt 0xcc000 (banks !! 15) <>
         memAt 0xdc000 (banks !! 15) <>
         memAt 0xec000 (banks !! 15) <>
         memAt 0xfc000 (banks !! 15)
       instrs = decodeInstructions mem
       jump_tables =
         [ (0xc85c2, 0xc8556, 7)
         , (0xc87c0, 0xc8556, 10)
         ]
       tables2 =
         [ jumpTable2 0xb8028 0xb8047 0xb8055 14 mem
         , jumpTable2 0xb80d0 0xb82d9 0xb82de 5 mem
         , jumpTable2 0xb82ee 0xb84f3 0xb84f7 4 mem
         , jumpTable2 0xb8506 0xb864e 0xb8652 4 mem
         , jumpTable2 0xb8661 0xb8796 0xb879a 4 mem
         , jumpTable2 0xb87a9 0xb894c 0xb8951 5 mem
         , jumpTable2 0xb8961 0xb8b16 0xb8b1b 5 mem
         , jumpTable2 0xb8b2b 0xb8cbb 0xb8cbf 4 mem
         , jumpTable2 0xb8cce 0xb8e08 0xb8e0c 4 mem
         , jumpTable2 0xb8e1b 0xb9205 0xb920c 7 mem
         , jumpTable2 0xb921e 0xb9395 0xb9398 3 mem
         , jumpTable2 0xb93a6 0xb9662 0xb9668 6 mem
         , jumpTable2 0xb9679 0xb96bc 0xb96be 2 mem
         , jumpTable2 0xb96cb 0xb9b1c 0xb9b23 7 mem
         , jumpTable2 0xb9b35 0xb9fbd 0xb9fc8 11 mem
         , jumpTable2 0xb9d43 0xb9fc0 0xb9fcb 5 mem
         , jumpTable2 0xb9fe5 0xba100 0xba106 6 mem
         , jumpTable2 0xba5fe 0xba930 0xba939 9 mem
         , jumpTable2 0xbdd11 0xbdd14 0xbdd24 16 mem

         , jumpTable2 0xe82e9 0xe84dc 0xe84e5 9 mem
         , jumpTable2 0xe8505 0xe8783 0xe878f 12 mem -- 4th entry causes trouble $8569/$879B
         , jumpTable2 0xe9296 0xe92f0 0xe9370 127 mem
         -- , jumpTable2 0xe92d5 0xe92f0 0xe9370 127 mem -- jsr table
         -- , jumpTable2 0xe92e4 0xe9470 0xe947f 10 mem -- jsr table

         , jumpTable2 0xeda67 0xedcb8 0xedcc4 12 mem
         -- , jumpTable2 0xedd11 0xedd14 0xedd24 16 mem -- jsr table
         , jumpTable2 0xee64f 0xee986 0xee98f 9 mem
         ]
       ((_, Override vecs127 _), _) = jumpTable2 0xe92d5 0xe92f0 0xe9370 127 mem
       ((_, Override vecs10 _), _) = jumpTable2 0xe92e4 0xe9470 0xe947f 10 mem
       ((_, Override vecs16 _), _) = jumpTable2 0xedd11 0xedd14 0xedd24 16 mem
       overrides =
         IntMap.fromList $
         -- always branches
         -- [ alwaysBranch instrs 0xe9966 ] ++ -- TODO: fix alwaysBranch for banked memory
         [ (0xe9966, Override [0xe9972] mconcat) ] ++
         -- bank switches
         [ (b + 0xd0a7, Override [0xcd0aa] mconcat) | b <- [0,0x10000..0xf0000] ] ++
         [ (b + 0xc034, Override [0xcc037] mconcat) | b <- [0,0x10000..0xf0000] ] ++
         -- [ (b + 0xf2e0, Override [0xe8000] mconcat) | b <- [0,0x10000..0xf0000] ] ++
         -- inline jump table routine
         [ jumpTable b mem a l | (a, b, l) <- jump_tables ] ++
         -- indirect jumps
         map fst tables2 ++
         -- 9296_14 needs a special override; it's a JSR table
         [ (0xe9296, Override (0xe9299 : vecs127)
             (\(e : es) ->
                thenFinalEffect (noEffect { stack = Stack.pull >>> Stack.pull })
                (jsrFinalEffect (mconcat (take 1 es)) e))) -- FIXME: 127
         , (0xe92d5, Override (0xe92e7 : vecs127)
             (\(e : es) ->
                thenFinalEffect (noEffect { stack = Stack.pull >>> Stack.pull })
                (jsrFinalEffect (mconcat (take 1 es)) e))) -- FIXME: take 1
         , (0xe92e4, Override (0xe92e7 : vecs10)
             (\(e : es) ->
                thenFinalEffect (noEffect { stack = Stack.pull >>> Stack.pull })
                (jsrFinalEffect (mconcat (take 1 es)) e))) -- FIXME: take 1
         , (0xedd11, Override (0xedce9 : vecs16)
             (\(e : es) ->
                thenFinalEffect (noEffect { stack = Stack.pull >>> Stack.pull })
                (jsrFinalEffect (mconcat (take 1 es)) e))) -- FIXME: take 1
            {-

c819_14: stack effect confused
    JSR $84ee                                   ; c81f_14 --Y-C-IDV- ?
    JSR $dcd0                                   ; c822_14 --Y---ID-- ?
    JSR $d658                                   ; c825_14 --Y---ID-- ?
    JSR $c5a9                                   ; c828_14 --Y---IDV- ?
    JSR $925b                                   ; c82b_14 --Y---ID-- ?
    JSR $cc77                                   ; c82e_14 A-Y---IDV- ?


JSR $879b

87ed: PLA/PLA/JMP $86BC_14
-}
         ] ++
         do b <- [0,0x10000..0xf0000]
            let c000 x = (sameBank b (fromIntegral (x - 2)), Override [x] mconcat)
            map c000 $
              [ 0xdc073
              , 0xec07b
              , 0xec0a7
              , 0xdc0d3
              , 0xec0fc
              , 0xec1b3
              , 0xdc1c0
              , 0xec1c8
              , 0xec230
              , 0xdc249
              , 0xec251
              , 0xdc26b
              , 0xec354
              , 0xec4c9
              , 0xec54a
              , 0xdc559
              , 0xec561
              , 0xdc567
              , 0xec56f
              , 0xdc595
              , 0xec5a5
              , 0xbc5af
              , 0xec5b7
              , 0xdc640
              , 0xdc685
              , 0xec7b1
              , 0xec804
              , 0xbc80e
              , 0xec816
              , 0xeca03
              , 0xecb08
              , 0xecc43
              , 0xacc79
              , 0xecd01
              , 0xdd633
              , 0x9d62b
              , 0xdd633
              , 0x9d639
              , 0x9d644
              , 0x9d64f
              , 0xed74f
              , 0xef2dd
              ]
              -- what about d0c3? c19a? c461? c628? c498? c4af? c4d1?
              -- what about c537? c66a? c7a8? c7cd? c96f? ca12? ca1a?
              -- what about cb34? cb4a? cb60? cbc7? d65c?

       usage0 =
         IntMap.unionsWith (<>) $
         [ markType mem a TCodePtr | a <- [0x0fffa, 0x1fffa .. 0xffffa] ] ++ -- NMI
         [ markType mem a TCodePtr | a <- [0x0fffc, 0x1fffc .. 0xffffc] ] ++ -- RESET
         [ markType mem a TCodePtr | a <- [0x0fffe, 0x1fffe .. 0xffffe] ] ++ -- IRQ
         [ markTypes mem (a+3) (replicate l TCodePtr) | (a, _, l) <- jump_tables ] ++
         map snd tables2
     effs <- computeFinalEffects instrs overrides
     let pcs0 =
           concat
           [ (sameBank a . fromIntegral) <$> maybe (error "vectors FFFA") id (getVecs mem a 3)
           | a <- [0x0fffa, 0x1fffa .. 0xffffa]
           ]
     let successors = fmap overrideSuccessors overrides <> allSuccessors instrs
     let instrs' = IntMap.intersectionWith (,) instrs successors
     let usage1 = computeUsage' instrs' usage0 pcs0
     let normAddr a = if a' < 0x8000 {- || a' >= 0xc000 -} then a' else a
           where a' = a `mod` 65536
     let usage = IntMap.mapKeysWith (<>) normAddr usage1
     let statements = disassemble (IntMap.mapKeys normAddr mem) usage
     let jsrs = allJSRs $ IntMap.filterWithKey (\k _ -> alreadyExecuted usage k) instrs
     let roots = IntSet.fromList pcs0 <> jsrs
     let (entries, regions, edges) = entryPoints successors roots
     ppMemEffectsByPC (ppLabel usage) (fmap (memory' . snd) (IntMap.restrictKeys effs entries))
     putStrLn (replicate 80 '-')
     ppCombined' knownLabels (fmap snd effs) usage regions statements
     putStrLn (replicate 80 '-')
     mapM_ putStrLn $ ppCallGraph' knownLabels effs usage edges

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

-- | Override for an indirect jump instruction that loads the MSB and
-- LSB from separate byte arrays. Also returns a usage map.
jumpTable2 ::
  -- | Address of indirect jump instruction
  Addr ->
  -- | Address of LSB table
  Addr ->
  -- | Address of MSB table
  Addr ->
  -- | Number of vectors
  Int ->
  -- | Memory contents
  IntMap Word8 ->
  ((Addr, Override), IntMap AddrUsage)
jumpTable2 addr lsb msb n mem =
  case result of
    Nothing -> error "jumpTable2: bad address"
    Just vecs -> ((addr, Override vecs mconcat), IntMap.fromList [ (v, addrJumpInd) | v <- vecs ])
  where
    result =
      do lsbs <- traverse (flip IntMap.lookup mem) $ take n [lsb..]
         msbs <- traverse (flip IntMap.lookup mem) $ take n [msb..]
         pure (map (sameBank addr) (zipWith word lsbs msbs))

knownLabels :: IntMap String
knownLabels =
  IntMap.fromList $
  [ (0x0cff0, "LCFF0_0_NMI")
  , (0x1cff0, "LCFF0_1_NMI")
  , (0x2cff0, "LCFF0_2_NMI")
  , (0x3cff0, "LCFF0_3_NMI")
  , (0x4cff0, "LCFF0_4_NMI")
  , (0x5cff0, "LCFF0_5_NMI")
  , (0x6cff0, "LCFF0_6_NMI")
  , (0x7cff0, "LCFF0_7_NMI")
  , (0x8cff0, "LCFF0_8_NMI")
  , (0x9cff0, "LCFF0_9_NMI")
  , (0xacff0, "LCFF0_A_NMI")
  , (0xbcff0, "LCFF0_B_NMI")
  , (0xccff0, "LCFF0_C_NMI")
  , (0xdcff0, "LCFF0_D_NMI")
  , (0xecff0, "LCFF0_E_NMI")
  , (0xfcff0, "LCFF0_F_NMI")
  , (0x0ffe0, "LFFE0_0_RESET")
  , (0x1ffe0, "LFFE0_1_RESET")
  , (0x2ffe0, "LFFE0_2_RESET")
  , (0x3ffe0, "LFFE0_3_RESET")
  , (0x4ffe0, "LFFE0_4_RESET")
  , (0x5ffe0, "LFFE0_5_RESET")
  , (0x6ffe0, "LFFE0_6_RESET")
  , (0x7ffe0, "LFFE0_7_RESET")
  , (0x8ffe0, "LFFE0_8_RESET")
  , (0x9ffe0, "LFFE0_9_RESET")
  , (0xaffe0, "LFFE0_A_RESET")
  , (0xbffe0, "LFFE0_B_RESET")
  , (0xcffe0, "LFFE0_C_RESET")
  , (0xdffe0, "LFFE0_D_RESET")
  , (0xeffe0, "LFFE0_E_RESET")
  , (0xfffe0, "LFFE0_F_RESET")
  , (0xe8000, "L8000_E_INITIALIZATION")
  ]

{-

c_8556: jump table routine (input in X)

Routines that show stack blowing up:
R8000_12
R86b4_12
R86c3_12
R86d3_12
R8706_12 (this one is recursive

; JSR $8706 $88a9 $88e1 $8935 $8954
-}
