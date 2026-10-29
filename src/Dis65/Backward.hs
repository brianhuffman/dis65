{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Dis65.Backward where

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
import qualified Dis65.Effect.Flag as Flag

import Dis65.Addr
import Dis65.Effect
import Dis65.Instruction

--------------------------------------------------------------------------------
-- * Effects of various instruction types

doOpStack :: OpStack -> BasicEffect
doOpStack =
  \case
    PHP -> noEffect { stack = Stack.push, flags = Flag.readP }
    PLP -> noEffect { stack = Stack.pull, flags = Flag.writeP }
    PHA -> noEffect { stack = Stack.push, registers = Reg.readA }
    PLA -> noEffect { stack = Stack.pull, registers = Reg.writeA, flags = Flag.writeNZ }

doOpReg :: OpReg -> Reg.RegEffect
doOpReg =
  \case
    CLC -> noEffect
    SEC -> noEffect
    CLI -> noEffect
    SEI -> noEffect
    CLV -> noEffect
    CLD -> noEffect
    SED -> noEffect
    DEX -> Reg.readX >>> Reg.writeX
    TXA -> Reg.readX >>> Reg.writeA
    TAX -> Reg.readA >>> Reg.writeX
    INX -> Reg.readX >>> Reg.writeX
    DEY -> Reg.readY >>> Reg.writeY
    TYA -> Reg.readY >>> Reg.writeA
    TAY -> Reg.readA >>> Reg.writeY
    INY -> Reg.readY >>> Reg.writeY
    TXS -> Reg.readX >>> Reg.writeS
    TSX -> Reg.readS >>> Reg.writeX
    NOP -> noEffect

opRegFlags :: OpReg -> Flag.FlagEffect
opRegFlags =
  \case
    CLC -> Flag.clrC
    SEC -> Flag.setC
    CLI -> Flag.clrI
    SEI -> Flag.setI
    CLV -> Flag.clrV
    CLD -> Flag.clrD
    SED -> Flag.setD
    DEX -> Flag.writeNZ
    TXA -> Flag.writeNZ
    TAX -> Flag.writeNZ
    INX -> Flag.writeNZ
    DEY -> Flag.writeNZ
    TYA -> Flag.writeNZ
    TAY -> Flag.writeNZ
    INY -> Flag.writeNZ
    TXS -> noEffect
    TSX -> Flag.writeNZ
    NOP -> noEffect

doOpBranch :: OpBranch -> Flag.FlagEffect
doOpBranch =
  \case
    BPL -> Flag.readN
    BMI -> Flag.readN
    BVC -> Flag.readV
    BVS -> Flag.readV
    BCC -> Flag.readC
    BCS -> Flag.readC
    BNE -> Flag.readZ
    BEQ -> Flag.readZ

doOpR :: OpR -> Reg.RegEffect
doOpR =
  \case
    ORA -> Reg.readA >>> Reg.writeA
    AND -> Reg.readA >>> Reg.writeA
    EOR -> Reg.readA >>> Reg.writeA
    ADC -> Reg.readA >>> Reg.writeA
    SBC -> Reg.readA >>> Reg.writeA
    BIT -> Reg.readA
    CMP -> Reg.readA
    CPX -> Reg.readX
    CPY -> Reg.readY
    LDA -> Reg.writeA
    LDX -> Reg.writeX
    LDY -> Reg.writeY

opRFlags :: OpR -> Flag.FlagEffect
opRFlags =
  \case
    ORA -> Flag.writeNZ
    AND -> Flag.writeNZ
    EOR -> Flag.writeNZ
    ADC -> Flag.readC >>> Flag.readD >>> Flag.writeNZC >>> Flag.writeV
    SBC -> Flag.readC >>> Flag.readD >>> Flag.writeNZC >>> Flag.writeV
    BIT -> Flag.writeNZ >>> Flag.writeV
    CMP -> Flag.writeNZC
    CPX -> Flag.writeNZC
    CPY -> Flag.writeNZC
    LDA -> Flag.writeNZ
    LDX -> Flag.writeNZ
    LDY -> Flag.writeNZ

doOpRW :: OpRW -> Flag.FlagEffect
doOpRW =
  \case
    ASL -> Flag.writeNZC
    ROL -> Flag.readC >>> Flag.writeNZC
    LSR -> Flag.writeNZC
    ROR -> Flag.readC >>> Flag.writeNZC
    DEC -> Flag.writeNZ
    INC -> Flag.writeNZ

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
    Branch _ target -> [pc + 2, sameBank pc target]
    JSR target -> [pc + 3, sameBank pc target]
    BRK -> []
    RTI -> []
    RTS -> []
    AbsJMP target -> [sameBank pc target]
    IndJMP _ -> []
    Undoc _ -> []

allSuccessors :: IntMap Instruction -> IntMap [Addr]
allSuccessors = IntMap.mapWithKey computeSuccessors

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
  Addr ->
  FinalEffect
computeEffectAt instructions state pc =
  case IntMap.lookup pc instructions of
    Nothing -> jmpAbsFinalEffect pc -- should never happen
    Just instr ->
     case instr of
       Reg op ->
         let effect = noEffect { registers = doOpReg op, flags = opRegFlags op }
         in thenFinalEffect effect $ lookupEffect state (pc + 1)
       Stack op ->
         let effect = doOpStack op
         in thenFinalEffect effect $ lookupEffect state (pc + 1)
       Read op arg ->
         let effect = doAddrArg Mem.readAddr arg >>> noEffect { registers = doOpR op, flags = opRFlags op }
         in thenFinalEffect effect $ lookupEffect state (pc + 1 + sizeAddrArg arg)
       Write op arg ->
         let effect = doAddrArg Mem.writeAddr arg >>> noEffect { registers = doOpW op }
         in thenFinalEffect effect $ lookupEffect state (pc + 1 + sizeAddrArg arg)
       Modify op arg ->
         let effect = doAddrArg Mem.modifyAddr arg >>> noEffect { flags = doOpRW op }
         in thenFinalEffect effect $ lookupEffect state (pc + 1 + sizeAddrArg arg)
       Accumulator op ->
         let effect = noEffect { registers = Reg.readA >>> Reg.writeA, flags = doOpRW op }
         in thenFinalEffect effect $ lookupEffect state (pc + 1)
       Branch op (sameBank pc -> target) ->
         let effect = noEffect { flags = doOpBranch op, branch = True }
         in thenFinalEffect effect $ (lookupEffect state (pc + 2) <> lookupEffect state target)
       JSR (sameBank pc -> target) ->
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
         lookupEffect state (sameBank pc target)
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
-- * Overrides

-- | An override specifies an alternative semantics for an
-- instruction. It contains a list of successor addresses, as well as
-- a function that combines the effects from those addresses to
-- produce a new effect.
data Override = Override [Addr] ([FinalEffect] -> FinalEffect)

overrideSuccessors :: Override -> [Addr]
overrideSuccessors (Override xs _) = xs


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
  -- | Program
  IntMap Instruction ->
  -- | Overrides
  IntMap Override ->
  IO (IntMap (Instruction, FinalEffect))
computeFinalEffects instructions overrides =
  do let successors = fmap overrideSuccessors overrides <> allSuccessors instructions
     let predecessors = fmap IntSet.fromList (computePredecessors successors)
     let state0 = IntMap.map (const bottom) instructions
     let worklist0 = IntMap.keysSet instructions
     let
       go :: IntSet -> IntMap FinalEffect -> IO (IntMap FinalEffect)
       go worklist state =
         case IntSet.maxView worklist of
           Nothing -> pure state
           Just (addr, worklist') ->
             do let
                  old = fromMaybe (error "invariant violation") (IntMap.lookup addr state) -- lookup should always succeed
                  new =
                    case IntMap.lookup addr overrides of
                      Nothing -> computeEffectAt instructions state addr
                      Just (Override addrs f) -> f (map (lookupEffect state) addrs)
                if old == new then go worklist' state else
                  do let dirty = fromMaybe mempty (IntMap.lookup addr predecessors)
                     go (IntSet.union dirty worklist') (IntMap.insert addr new state)

     final <- go worklist0 state0
     pure (IntMap.intersectionWith (,) instructions final)

-- | Parameterized by a printer for labels.
ppFinalEffects :: (Addr -> String) -> IntMap (Instruction, FinalEffect) -> IO ()
ppFinalEffects label = mapM_ pp1 . IntMap.assocs
  where
    pp1 (addr, (instr, e)) =
      putStrLn $
      ppWord16 (fromIntegral addr) ++ ": " ++
      take 16 (ppInstruction (label . sameBank addr) instr ++ repeat ' ') ++ " " ++
      drop 23 (unlines (map (replicate 23 ' ' ++) (ppFinalEffect label e)))
