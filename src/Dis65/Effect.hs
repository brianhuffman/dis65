{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Dis65.Effect where

import           Control.Applicative (liftA2)
import           Control.Monad (unless)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
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
  , memory :: !(Map AddrMode Mem.MemEffect)
  , registers :: !Reg.RegEffect
  , subroutines :: !IntSet -- can JSR to these addresses
  , branch :: !Bool -- can reach a branch instruction
  , rts :: !Bool -- can reach an RTS instruction
  , rti :: !Bool -- can reach an RTI instruction
  , brk :: !Bool -- can reach a BRK instruction
  , undoc :: !(Set Word8) -- can reach an undocumented instruction
  , jmpAbs :: !IntSet -- can jump to an unmapped address
  , jmpInd :: !IntSet -- can jump indirectly through a vector
  }
  deriving (Eq, Show)

instance Effect BasicEffect where
  e1 +++ e2 =
    BasicEffect
    { stack = stack e1 +++ stack e2
    , memory = Map.unionWith (+++) (memory e1) (memory e2)
    , registers = registers e1 +++ registers e2
    , subroutines = subroutines e1 <> subroutines e2
    , branch = branch e1 || branch e2
    , rts = rts e1 || rts e2
    , rti = rti e1 || rti e2
    , brk = brk e1 || brk e2
    , undoc = Set.union (undoc e1) (undoc e2)
    , jmpAbs = IntSet.union (jmpAbs e1) (jmpAbs e2)
    , jmpInd = IntSet.union (jmpInd e1) (jmpInd e2)
    }

  e1 >>> e2 =
    BasicEffect
    { stack = stack e1 >>> stack e2
    , memory = Map.unionWith (>>>) (memory e1) (memory e2)
    , registers = registers e1 >>> registers e2
    , subroutines = subroutines e1 <> subroutines e2
    , branch = branch e1 || branch e2
    , rts = rts e1 || rts e2
    , rti = rti e1 || rti e2
    , brk = brk e1 || brk e2
    , undoc = Set.union (undoc e1) (undoc e2)
    , jmpAbs = IntSet.union (jmpAbs e1) (jmpAbs e2)
    , jmpInd = IntSet.union (jmpInd e1) (jmpInd e2)
    }

instance NoEffect BasicEffect where
  noEffect =
    BasicEffect
    { stack = noEffect
    , memory = Map.empty
    , registers = noEffect
    , subroutines = mempty
    , branch = False
    , rts = False
    , rti = False
    , brk = False
    , undoc = Set.empty
    , jmpAbs = IntSet.empty
    , jmpInd = IntSet.empty
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

doAddrArg :: (Addr -> Mem.MemEffect) -> AddrArg -> BasicEffect
doAddrArg rw =
  \case
    IndirectX z -> noEffect { memory = go InX z, registers = Reg.readX }
    ZeroPage z  -> noEffect { memory = go Zpg z }
    Immediate _ -> noEffect
    Absolute w  -> noEffect { memory = go Abs w }
    IndirectY z -> noEffect { memory = go InY z, registers = Reg.readY }
    ZeroPageX z -> noEffect { memory = go ZpX z, registers = Reg.readX }
    ZeroPageY z -> noEffect { memory = go ZpY z, registers = Reg.readY }
    AbsoluteY w -> noEffect { memory = go AbY w, registers = Reg.readY }
    AbsoluteX w -> noEffect { memory = go AbX w, registers = Reg.readX }
  where
    go mode addr = Map.singleton mode $! rw (fromIntegral addr)


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

jmpUnmapped :: Int -> BasicEffect
jmpUnmapped pc = noEffect { jmpAbs = IntSet.singleton pc }

lookupEffect :: IntMap BasicEffect -> Int -> BasicEffect
lookupEffect state pc =
  case IntMap.lookup pc state of
    Nothing -> jmpUnmapped pc
    Just e -> e

computeEffectAt ::
  -- | instructions
  IntMap Instruction ->
  -- | successors
  IntMap [Addr] ->
  -- | state
  IntMap BasicEffect ->
  -- | address
  Int ->
  BasicEffect
computeEffectAt instructions successors state pc =
  case IntMap.lookup pc instructions of
    Nothing -> jmpUnmapped pc
    Just instr ->
     let
       nexts = fromMaybe [] $ IntMap.lookup pc successors
       nextEffect = foldr (+++) noEffect (map (lookupEffect state) nexts)
     in
     case instr of
       Reg op ->
         let effect = noEffect { registers = doOpReg op }
         in effect >>> nextEffect
       Stack op ->
         let effect = doOpStack op
         in effect >>> nextEffect
       Read op arg ->
         let effect = doAddrArg Mem.readAddr arg >>> noEffect { registers = doOpR op }
         in effect >>> nextEffect
       Write op arg ->
         let effect = doAddrArg Mem.writeAddr arg >>> noEffect { registers = doOpW op }
         in effect >>> nextEffect
       Modify op arg ->
         let effect = doAddrArg Mem.modifyAddr arg >>> noEffect { registers = doOpRW op }
         in effect >>> nextEffect
       Accumulator op ->
         let effect = noEffect { registers = Reg.readA >>> doOpRW op >>> Reg.writeA }
         in effect >>> nextEffect
       Branch op arg ->
         let effect = noEffect { registers = doOpBranch op, branch = True }
         in effect >>> nextEffect
       JSR (fromIntegral -> target) ->
         let
           srEffect = lookupEffect state target
           afterEffect = lookupEffect state (pc + 3)
         in
           if rts srEffect
           then push2 >>> srEffect { rts = False } >>> pull2 >>> noEffect { subroutines = IntSet.singleton target } >>> afterEffect
           else push2 >>> srEffect -- no return
       BRK ->
         noEffect { brk = True }
       RTI ->
         noEffect { rti = True }
       RTS ->
         noEffect { rts = True }
       AbsJMP _ ->
         nextEffect
       IndJMP (fromIntegral -> target) ->
         noEffect { jmpInd = IntSet.singleton target }
       Undoc op ->
         noEffect { undoc = Set.singleton op }

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
  -- | Memory
  IntMap Word8 ->
  IO (IntMap BasicEffect)
computeFinalEffects memory =
  do let instructions = decodeInstructions memory
     let successors = IntMap.mapWithKey computeSuccessors instructions
     let predecessors = fmap IntSet.fromList (computePredecessors successors)
     let state0 = IntMap.map (const noEffect) instructions
     let worklist0 = IntMap.keysSet instructions
     let
       -- invariant: worklist should be a subset of the key set of state.
       go :: IntSet -> IntMap BasicEffect -> IO (IntMap BasicEffect)
       go worklist state =
         case IntSet.maxView worklist of
           Nothing -> pure state
           Just (addr, worklist') ->
             do let old = fromMaybe (error "invariant violation") (IntMap.lookup addr state) -- lookup should always succeed
                let new = computeEffectAt instructions successors state addr
                let succs = fromMaybe mempty (IntMap.lookup addr successors)
                putStrLn $ "**********" ++ ppWord16 (fromIntegral addr) ++ "**********"
                putStrLn $ unwords $ "opcode:" : maybe "<none>" ppWord8 (IntMap.lookup addr memory) : "succs:" : map (ppWord16 . fromIntegral) succs
                case IntMap.lookup addr instructions of
                  Just instr -> putStrLn $ ppInstruction instr
                  Nothing -> pure ()
                putStrLn $ ppBasicEffect new
                if old == new then go worklist' state else
                  do let dirty = fromMaybe mempty (IntMap.lookup addr predecessors)
                     unless (IntSet.null dirty) $ putStrLn $ unwords $ "dirty:" : map (ppWord16 . fromIntegral) (IntSet.elems dirty)
                     go (IntSet.union dirty worklist') (IntMap.insert addr new state)

     go worklist0 state0

--------------------------------------------------------------------------------
-- * Pretty printing


ppMemEffects :: Map AddrMode Mem.MemEffect -> String
ppMemEffects m = unwords (("READS" : reads) ++ ("WRITES" : writes))
  where
    reads =
      do (mode, Mem.MemEffect r _) <- Map.assocs m
         addr <- IntSet.elems r
         [ppMemAddr mode addr]
    writes =
      do (mode, Mem.MemEffect _ w) <- Map.assocs m
         addr <- IntSet.elems w
         [ppMemAddr mode addr]

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
  [ if rts e then "RTS" else ""
  , if rti e then "RTI" else ""
  , if brk e then "BRK" else ""
  , unwords [ "U" ++ ppWord8 op | op <- Set.elems (undoc e) ]
  , unwords [ "JMP $" ++ ppWord16 (fromIntegral a) | a <- IntSet.elems (jmpAbs e) ]
  , unwords [ "JMP ($" ++ ppWord16 (fromIntegral a) ++ ")" | a <- IntSet.elems (jmpInd e) ]
  , Reg.ppRegEffect (registers e)
  , Stack.ppStackEffect (stack e)
  , ppMemEffects (memory e)
  , ppSubroutines (subroutines e)
  , if branch e then "Branch" else ""
  ]
