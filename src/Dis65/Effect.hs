{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Dis65.Effect where

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

instance Bottom BasicEffect where
  bottom =
    BasicEffect
    { stack = bottom
    , memory = bottom
    , registers = bottom
    , subroutines = mempty
    , branch = False
    }

push2 :: BasicEffect
push2 = noEffect { stack = Stack.push >>> Stack.push }

pull2 :: BasicEffect
pull2 = noEffect { stack = Stack.pull >>> Stack.pull }

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

instance Bottom FinalEffect where
  bottom =
    FinalEffect
    { loop = Just bottom
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
-- * Pretty printing

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
