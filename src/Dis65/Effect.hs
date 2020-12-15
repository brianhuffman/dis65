{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Dis65.Effect where

import           Control.Applicative (liftA2)
import           Control.Monad (unless, when)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word

import           Dis65.Effect.Class
import qualified Dis65.Effect.Stack as Stack
import qualified Dis65.Effect.Mem as Mem
import qualified Dis65.Effect.Reg as Reg

import Dis65.Instruction

--------------------------------------------------------------------------------
-- * BasicEffect

data BasicEffect =
  BasicEffect
  { stack :: !Stack.StackEffect
  , memory :: !Mem.MemEffect
  , registers :: !Reg.RegEffect
  , subroutines :: !IntSet
  , branch :: !Bool
  }
  deriving (Eq, Show)

instance Effect BasicEffect where
  e1 +++ e2 =
    BasicEffect
    { stack = stack e1 +++ stack e2
    , memory = memory e1 +++ memory e2
    , registers = registers e1 +++ registers e2
    , subroutines = subroutines e1 <> subroutines e2
    , branch = branch e1 || branch e2
    }

  e1 >>> e2 =
    BasicEffect
    { stack = stack e1 >>> stack e2
    , memory = memory e1 >>> memory e2
    , registers = registers e1 >>> registers e2
    , subroutines = subroutines e1 <> subroutines e2
    , branch = branch e1 || branch e2
    }

instance NoEffect BasicEffect where
  noEffect =
    BasicEffect
    { stack = noEffect
    , memory = noEffect
    , registers = noEffect
    , subroutines = mempty
    , branch = False
    }

--------------------------------------------------------------------------------
-- * Effects of various instruction types

push2 :: BasicEffect
push2 = noEffect { stack = Stack.push >>> Stack.push }

pull2 :: BasicEffect
pull2 = noEffect { stack = Stack.pull >>> Stack.pull }

doOpStack :: OpStack -> BasicEffect
doOpStack =
  \case
    PHP -> noEffect { stack = Stack.push, registers = Reg.readP }
    PLP -> noEffect { stack = Stack.pull, registers = Reg.writeP }
    PHA -> noEffect { stack = Stack.push, registers = Reg.readA }
    PLA -> noEffect { stack = Stack.pull, registers = Reg.writeA >>> Reg.writeNZ }

doOpReg :: OpReg -> Reg.RegEffect
doOpReg =
  \case
    CLC -> Reg.writeC
    SEC -> Reg.writeC
    CLI -> Reg.writeI
    SEI -> Reg.writeI
    CLV -> Reg.writeV
    CLD -> Reg.writeD
    SED -> Reg.writeD
    DEX -> Reg.readX >>> Reg.writeX >>> Reg.writeNZ
    TXA -> Reg.readX >>> Reg.writeA >>> Reg.writeNZ
    TAX -> Reg.readA >>> Reg.writeX >>> Reg.writeNZ
    INX -> Reg.readX >>> Reg.writeX >>> Reg.writeNZ
    DEY -> Reg.readY >>> Reg.writeY >>> Reg.writeNZ
    TYA -> Reg.readY >>> Reg.writeA >>> Reg.writeNZ
    TAY -> Reg.readA >>> Reg.writeY >>> Reg.writeNZ
    INY -> Reg.readY >>> Reg.writeY >>> Reg.writeNZ
    TXS -> Reg.readX >>> Reg.writeS
    TSX -> Reg.readS >>> Reg.writeX >>> Reg.writeNZ
    NOP -> noEffect

doOpBranch :: OpBranch -> Reg.RegEffect
doOpBranch =
  \case
    BPL -> Reg.readN
    BMI -> Reg.readN
    BVC -> Reg.readV
    BVS -> Reg.readV
    BCC -> Reg.readC
    BCS -> Reg.readC
    BNE -> Reg.readZ
    BEQ -> Reg.readZ

doOpR :: OpR -> Reg.RegEffect
doOpR =
  \case
    ORA -> Reg.readA >>> Reg.writeA >>> Reg.writeNZ
    AND -> Reg.readA >>> Reg.writeA >>> Reg.writeNZ
    EOR -> Reg.readA >>> Reg.writeA >>> Reg.writeNZ
    ADC -> Reg.readA >>> Reg.readC >>> Reg.readD >>> Reg.writeA >>> Reg.writeNZC >>> Reg.writeV
    SBC -> Reg.readA >>> Reg.readC >>> Reg.readD >>> Reg.writeA >>> Reg.writeNZC >>> Reg.writeV
    BIT -> Reg.readA >>> Reg.writeNZ >>> Reg.writeV
    CMP -> Reg.readA >>> Reg.writeNZC
    CPX -> Reg.readX >>> Reg.writeNZC
    CPY -> Reg.readX >>> Reg.writeNZC
    LDA -> Reg.writeA >>> Reg.writeNZ
    LDX -> Reg.writeX >>> Reg.writeNZ
    LDY -> Reg.writeY >>> Reg.writeNZ

doOpRW :: OpRW -> Reg.RegEffect
doOpRW =
  \case
    ASL -> Reg.writeNZC
    ROL -> Reg.readC >>> Reg.writeNZC
    LSR -> Reg.writeNZC
    ROR -> Reg.readC >>> Reg.writeNZC
    DEC -> Reg.writeNZ
    INC -> Reg.writeNZ

doOpW :: OpW -> Reg.RegEffect
doOpW =
  \case
    STA -> Reg.readA
    STX -> Reg.readX
    STY -> Reg.readY

doAddrMode :: AddrMode -> Reg.RegEffect
doAddrMode =
  \case
    InX -> Reg.readX
    Zpg -> noEffect
    Imm -> noEffect
    Abs -> noEffect
    InY -> Reg.readY
    ZpX -> Reg.readX
    ZpY -> Reg.readY
    AbY -> Reg.readY
    AbX -> Reg.readX

doAddrArg :: (AddrArg -> Mem.MemEffect) -> AddrArg -> BasicEffect
doAddrArg rw arg =
  case arg of
    IndirectX z -> noEffect { memory = rw arg, registers = Reg.readX }
    ZeroPage z  -> noEffect { memory = rw arg }
    Immediate _ -> noEffect
    Absolute w  -> noEffect { memory = rw arg }
    IndirectY z -> noEffect { memory = rw arg, registers = Reg.readY }
    ZeroPageX z -> noEffect { memory = rw arg, registers = Reg.readX }
    ZeroPageY z -> noEffect { memory = rw arg, registers = Reg.readY }
    AbsoluteY w -> noEffect { memory = rw arg, registers = Reg.readY }
    AbsoluteX w -> noEffect { memory = rw arg, registers = Reg.readX }

--------------------------------------------------------------------------------
-- * FinalEffect

-- | The effect of running an instruction all the way to a
-- block-ending instruction: Either an RTS, RTI, BRK or crash.

data FinalEffect =
  FinalEffect
  { loop :: Maybe BasicEffect
  , rts :: Maybe BasicEffect
  , rti :: Maybe BasicEffect
  , brk :: Maybe BasicEffect
  , undoc :: Map Word8 BasicEffect
  , jmpAbs :: IntMap BasicEffect
  , jmpInd :: IntMap BasicEffect
  }
  deriving (Eq, Show)

combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe _ Nothing y = y
combineMaybe _ x Nothing = x
combineMaybe f (Just x) (Just y) = Just (f x y)

instance Semigroup FinalEffect where
  e1 <> e2 =
    FinalEffect
    { loop = combineMaybe (+++) (loop e1) (loop e2)
    , rts = combineMaybe (+++) (rts e1) (rts e2)
    , rti = combineMaybe (+++) (rti e1) (rti e2)
    , brk = combineMaybe (+++) (brk e1) (brk e2)
    , undoc = Map.unionWith (+++) (undoc e1) (undoc e2)
    , jmpAbs = IntMap.unionWith (+++) (jmpAbs e1) (jmpAbs e2)
    , jmpInd = IntMap.unionWith (+++) (jmpInd e1) (jmpInd e2)
    }

instance Monoid FinalEffect where
  mempty =
    FinalEffect
    { loop = Nothing
    , rts = Nothing
    , rti = Nothing
    , brk = Nothing
    , undoc = Map.empty
    , jmpAbs = IntMap.empty
    , jmpInd = IntMap.empty
    }

bottomFinalEffect :: FinalEffect
bottomFinalEffect =
  FinalEffect
  { loop = Just noEffect
  , rts = Nothing
  , rti = Nothing
  , brk = Nothing
  , undoc = Map.empty
  , jmpAbs = IntMap.empty
  , jmpInd = IntMap.empty
  }

thenFinalEffect :: BasicEffect -> FinalEffect -> FinalEffect
thenFinalEffect e1 e2 =
  FinalEffect
  { loop = fmap (e1 >>>) (loop e2)
  , rts = fmap (e1 >>>) (rts e2)
  , rti = fmap (e1 >>>) (rti e2)
  , brk = fmap (e1 >>>) (brk e2)
  , undoc = fmap (e1 >>>) (undoc e2)
  , jmpAbs = fmap (e1 >>>) (jmpAbs e2)
  , jmpInd = fmap (e1 >>>) (jmpInd e2)
  }

jsrFinalEffect :: FinalEffect -> FinalEffect -> FinalEffect
jsrFinalEffect subroutine after =
  case rts subroutine of
    Nothing -> thenFinalEffect push2 subroutine
    Just body ->
      thenFinalEffect push2 subroutine { rts = Nothing } <>
      thenFinalEffect (push2 >>> body >>> pull2) after

--------------------------------------------------------------------------------
-- * Computing FinalEffect for one instruction

type Addr = Int

computeSuccessors :: Addr -> Instruction -> [Addr]
computeSuccessors pc =
  \case
    Reg _ -> [pc + 1]
    Stack _ -> [pc + 1]
    Read _ arg -> [pc + 1 + sizeAddrArg arg]
    Write _ arg -> [pc + 1 + sizeAddrArg arg]
    Modify _ arg -> [pc + 1 + sizeAddrArg arg]
    Accumulator op -> [pc + 1]
    Branch _ target -> [pc + 2, fromIntegral target]
    JSR target -> [pc + 3, fromIntegral target]
    BRK -> []
    RTI -> []
    RTS -> []
    AbsJMP target -> [fromIntegral target]
    IndJMP _ -> []
    Undoc _ -> []

computePredecessors :: IntMap [Addr] -> IntMap [Addr]
computePredecessors successors =
  IntMap.fromListWith (++) $
  do (a, bs) <- IntMap.assocs successors
     b <- bs
     pure (b, [a])

jmpUnmapped :: Int -> FinalEffect
jmpUnmapped pc = mempty { jmpAbs = IntMap.singleton pc noEffect }

lookupEffect :: IntMap FinalEffect -> Int -> FinalEffect
lookupEffect state pc =
  case IntMap.lookup pc state of
    Nothing -> jmpUnmapped pc
    Just e -> e

computeEffectAt ::
  -- | instructions
  IntMap Instruction ->
  -- | state
  IntMap FinalEffect ->
  -- | address
  Int ->
  FinalEffect
computeEffectAt instructions state pc =
  case IntMap.lookup pc instructions of
    Nothing -> jmpUnmapped pc
    Just instr ->
     case instr of
       Reg op ->
         let effect = noEffect { registers = doOpReg op }
         in thenFinalEffect effect $ lookupEffect state (pc + 1)
       Stack op ->
         let effect = doOpStack op
         in thenFinalEffect effect $ lookupEffect state (pc + 1)
       Read op arg ->
         let effect = doAddrArg Mem.readAddr arg >>> noEffect { registers = doOpR op }
         in thenFinalEffect effect $ lookupEffect state (pc + 1 + sizeAddrArg arg)
       Write op arg ->
         let effect = doAddrArg Mem.writeAddr arg >>> noEffect { registers = doOpW op }
         in thenFinalEffect effect $ lookupEffect state (pc + 1 + sizeAddrArg arg)
       Modify op arg ->
         let effect = doAddrArg Mem.modifyAddr arg >>> noEffect { registers = doOpRW op }
         in thenFinalEffect effect $ lookupEffect state (pc + 1 + sizeAddrArg arg)
       Accumulator op ->
         let effect = noEffect { registers = Reg.readA >>> doOpRW op >>> Reg.writeA }
         in thenFinalEffect effect $ lookupEffect state (pc + 1)
       Branch op (fromIntegral -> target) ->
         let effect = noEffect { registers = doOpBranch op, branch = True }
         in thenFinalEffect effect $ (lookupEffect state (pc + 2) <> lookupEffect state target)
       JSR (fromIntegral -> target) ->
         let
           effect = noEffect { subroutines = IntSet.singleton target }
           subEffect = thenFinalEffect effect $ lookupEffect state target
           afterEffect = lookupEffect state (pc + 3)
         in
           jsrFinalEffect subEffect afterEffect
       BRK ->
         mempty { brk = Just noEffect }
       RTI ->
         mempty { rti = Just noEffect }
       RTS ->
         mempty { rts = Just noEffect }
       AbsJMP (fromIntegral -> target) ->
         lookupEffect state target
       IndJMP (fromIntegral -> target) ->
         mempty { jmpInd = IntMap.singleton target noEffect }
       Undoc op ->
         mempty { undoc = Map.singleton op noEffect }

getAddr :: IntMap Word8 -> Addr -> AddrMode -> Maybe Addr
getAddr memory pc =
  \case
    InX -> one
    Zpg -> one
    Imm -> pure (pc + 1)
    Abs -> two
    InY -> one
    ZpX -> one
    ZpY -> one
    AbY -> two
    AbX -> two
  where
    one =
      do lo <- IntMap.lookup (pc + 1) memory
         pure (addr8 lo)
    two =
      do lo <- IntMap.lookup (pc + 1) memory
         hi <- IntMap.lookup (pc + 2) memory
         pure (addr16 lo hi)

addr8 :: Word8 -> Addr
addr8 = fromIntegral

addr16 :: Word8 -> Word8 -> Addr
addr16 lo hi = fromIntegral lo + fromIntegral hi * 256

--------------------------------------------------------------------------------
-- * Computing FinalEffect for all instructions

{-

Inputs:

Memory contents:
memory :: IntMap Word8

From this we can derive a table of successor addresses
> successors :: IntMap [Addr]
For invalid opcodes, RTS, RTI the set of successors will be empty.
This will have to rely on a user-provided set of successors for indirect jump instructions.
Also we will need a user-provided set of successors for any self-modified JMP/JSR.

Then we can invert successors to get a table of predecessors
> predecessors :: IntMap [Addr]

Then we start the main loop for the fixpoint computation. This uses a
worklist and a current state.
> worklist :: IntSet
> state :: IntMap FinalEffect

The initial worklist is the set of all keys from the memory map. The
initial state has the same set of keys, with all of the entries
initialized to the bottom element (mempty).

In each step we select an address from the worklist and then look up
the current value at that address in the state. We then compute a new
value using the instruction and the values of the successor addresses.
If the value is the same, then we recurse with a smaller worklist. If
the new value is different, then we update the state, add the
predecessors to the worklist, and recurse. This continues until the
worklist is empty.

To ensure termination, we rely on the fact that the state is
initialized with minimal elements, that the state update function is
monotonic, and that there are no infinite ascending chains in the
state value type.

Addresses should be selected from the worklist using `IntSet.maxView`,
because at the beginning that means that address will be processed in
a high-to-low order. This is desirable because predecessor addresses
are most often immediately below the successor address.

-}

computeFinalEffects ::
  Bool ->
  -- | Memory
  IntMap Word8 ->
  IO (IntMap (Instruction, FinalEffect))
computeFinalEffects debug memory =
  do let instructions = decodeInstructions memory
     let successors = IntMap.mapWithKey computeSuccessors instructions
     let predecessors = fmap IntSet.fromList (computePredecessors successors)
     let state0 = IntMap.map (const bottomFinalEffect) instructions
     let worklist0 = IntMap.keysSet instructions
     let
       go :: IntSet -> IntMap FinalEffect -> IO (IntMap FinalEffect)
       go worklist state =
         case IntSet.maxView worklist of
           Nothing -> pure state
           Just (addr, worklist') ->
             do let old = fromMaybe (error "invariant violation") (IntMap.lookup addr state) -- lookup should always succeed
                let new = computeEffectAt instructions state addr
                let succs = fromMaybe mempty (IntMap.lookup addr successors)
                when debug $
                  do putStrLn $ "**********" ++ ppWord16 (fromIntegral addr) ++ "**********"
                     putStrLn $ unwords $ "opcode:" : maybe "<none>" ppWord8 (IntMap.lookup addr memory) : "succs:" : map (ppWord16 . fromIntegral) succs
                     case IntMap.lookup addr instructions of
                       Just instr -> putStrLn $ ppInstruction instr
                       Nothing -> pure ()
                     putStr $ unlines $ ppFinalEffect new
                if old == new then go worklist' state else
                  do let dirty = fromMaybe mempty (IntMap.lookup addr predecessors)
                     when debug $ unless (IntSet.null dirty) $ putStrLn $ unwords $ "dirty:" : map (ppWord16 . fromIntegral) (IntSet.elems dirty)
                     go (IntSet.union dirty worklist') (IntMap.insert addr new state)

     final <- go worklist0 state0
     pure (IntMap.intersectionWith (,) instructions final)

ppFinalEffects :: IntMap (Instruction, FinalEffect) -> IO ()
ppFinalEffects = mapM_ pp1 . IntMap.assocs
  where
    pp1 (addr, (instr, e)) =
      putStrLn $
      ppWord16 (fromIntegral addr) ++ ": " ++
      take 16 (ppInstruction instr ++ repeat ' ') ++ " " ++
      drop 23 (unlines (map (replicate 23 ' ' ++) (ppFinalEffect e)))

--------------------------------------------------------------------------------
-- * Pretty printing


ppMemAddr :: AddrMode -> Int -> String
ppMemAddr mode addr =
  case mode of
    InX -> "($" ++ ppWord8 z ++ ",X)"
    Zpg -> "$" ++ ppWord8 z
    Imm -> "#$" ++ ppWord8 z
    Abs -> "$" ++ ppWord16 w
    InY -> "($" ++ ppWord8 z ++"),Y"
    ZpX -> "$" ++ ppWord8 z ++ ",X"
    ZpY -> "$" ++ ppWord8 z ++ ",Y"
    AbY -> "$" ++ ppWord16 w ++ ",Y"
    AbX -> "$" ++ ppWord16 w ++ ",X"
  where
    z = fromIntegral addr
    w = fromIntegral addr

ppSubroutines :: IntSet -> String
ppSubroutines s
  | IntSet.null s = ""
  | otherwise =
    unwords ("JSR" : [ "$" ++ ppWord16 (fromIntegral a) | a <- IntSet.elems s ])

ppBasicEffect :: BasicEffect -> String
ppBasicEffect e =
  unwords $
  filter (not . null) $
  [ Reg.ppRegEffect (registers e)
  , Stack.ppStackEffect (stack e)
  , Mem.ppMemEffect (memory e)
  , ppSubroutines (subroutines e)
  , if branch e then "Branches" else ""
  ]

ppFinalEffect :: FinalEffect -> [String]
ppFinalEffect e =
  catMaybes $
  [ fmap (("LOOP: " ++) . ppBasicEffect) (loop e)
  , fmap (("RTS: " ++) . ppBasicEffect) (rts e)
  , fmap (("RTI: " ++) . ppBasicEffect) (rti e)
  , fmap (("BRK: " ++) . ppBasicEffect) (brk e)
  ] ++
  [ Just (ppWord8 op ++ ": " ++ ppBasicEffect be)
  | (op, be) <- Map.assocs (undoc e) ]
  ++
  [ Just ("JMP $" ++ ppWord16 (fromIntegral a) ++ ": " ++ ppBasicEffect be)
  | (a, be) <- IntMap.assocs (jmpAbs e) ]
  ++
  [ Just ("JMP ($" ++ ppWord16 (fromIntegral a) ++ "): " ++ ppBasicEffect be)
  | (a, be) <- IntMap.assocs (jmpInd e) ]
