{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Dis65.Backward where

import           Control.Monad (unless, when)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Maybe
import           Data.Word

import           Dis65.Effect.Class
import qualified Dis65.Effect.Stack as Stack
import qualified Dis65.Effect.Mem as Mem
import qualified Dis65.Effect.Reg as Reg

import Dis65.Addr
import Dis65.Effect
import Dis65.Instruction

--------------------------------------------------------------------------------
-- * Effects of various instruction types

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
    IndirectX _ -> noEffect { memory = rw arg, registers = Reg.readX }
    ZeroPage  _ -> noEffect { memory = rw arg }
    Immediate _ -> noEffect
    Absolute  _ -> noEffect { memory = rw arg }
    IndirectY _ -> noEffect { memory = rw arg, registers = Reg.readY }
    ZeroPageX _ -> noEffect { memory = rw arg, registers = Reg.readX }
    ZeroPageY _ -> noEffect { memory = rw arg, registers = Reg.readY }
    AbsoluteY _ -> noEffect { memory = rw arg, registers = Reg.readY }
    AbsoluteX _ -> noEffect { memory = rw arg, registers = Reg.readX }

--------------------------------------------------------------------------------
-- * Computing FinalEffect for one instruction

computeSuccessors :: Addr -> Instruction -> [Addr]
computeSuccessors pc =
  \case
    Reg _ -> [pc + 1]
    Stack _ -> [pc + 1]
    Read _ arg -> [pc + 1 + sizeAddrArg arg]
    Write _ arg -> [pc + 1 + sizeAddrArg arg]
    Modify _ arg -> [pc + 1 + sizeAddrArg arg]
    Accumulator _ -> [pc + 1]
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

lookupEffect :: IntMap FinalEffect -> Int -> FinalEffect
lookupEffect state pc =
  case IntMap.lookup pc state of
    Nothing -> jmpAbsFinalEffect pc
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
    Nothing -> jmpAbsFinalEffect pc -- should never happen
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
           subEffect = lookupEffect state target
           afterEffect = lookupEffect state (pc + 3)
         in
           thenFinalEffect effect $ jsrFinalEffect subEffect afterEffect
       BRK ->
         brkFinalEffect
       RTI ->
         rtiFinalEffect
       RTS ->
         rtsFinalEffect
       AbsJMP target ->
         lookupEffect state (fromIntegral target)
       IndJMP target ->
         jmpIndFinalEffect target
       Undoc op ->
         undocFinalEffect op

getAddr :: IntMap Word8 -> Addr -> AddrMode -> Maybe Addr
getAddr mem pc =
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
      do lo <- IntMap.lookup (pc + 1) mem
         pure (addr8 lo)
    two =
      do lo <- IntMap.lookup (pc + 1) mem
         hi <- IntMap.lookup (pc + 2) mem
         pure (addr16 (word lo hi))

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
computeFinalEffects debug mem =
  do let instructions = decodeInstructions mem
     let successors = IntMap.mapWithKey computeSuccessors instructions
     let predecessors = fmap IntSet.fromList (computePredecessors successors)
     let state0 = IntMap.map (const bottom) instructions
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
                     putStrLn $ unwords $ "opcode:" : maybe "<none>" ppWord8 (IntMap.lookup addr mem) : "succs:" : map (ppWord16 . fromIntegral) succs
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
