{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Dis65.Effect where

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
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

instance Choice BasicEffect where
  e1 +++ e2 =
    BasicEffect
    { stack = stack e1 +++ stack e2
    , memory = memory e1 +++ memory e2
    , registers = registers e1 +++ registers e2
    , subroutines = subroutines e1 <> subroutines e2
    , branch = branch e1 || branch e2
    }

instance Effect BasicEffect where
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
-- * FinalEffect

-- | The effect of running an instruction all the way to a
-- block-ending instruction: Either an RTS, RTI, BRK or crash.

data FinalEffect =
  FinalEffect
  { stack' :: !Stack.FinalStackEffect
  , memory' :: !Mem.MemEffect
  , registers' :: !Reg.RegEffect
  , subroutines' :: !IntSet
  , branch' :: !Bool
  }
  deriving (Eq, Show)

instance Semigroup FinalEffect where
  e1 <> e2 =
    FinalEffect
    { stack' = stack' e1 <> stack' e2
    , memory' = memory' e1 +++ memory' e2
    , registers' = registers' e1 +++ registers' e2
    , subroutines' = subroutines' e1 <> subroutines' e2
    , branch' = branch' e1 || branch' e2
    }

instance Monoid FinalEffect where
  mempty =
    FinalEffect
    { stack' = bottom
    , memory' = bottom
    , registers' = bottom
    , subroutines' = mempty
    , branch' = False
    }

instance Bottom FinalEffect where
  bottom =
    FinalEffect
    { stack' = bottom
    , memory' = bottom
    , registers' = bottom
    , subroutines' = mempty
    , branch' = False
    }

thenFinalEffect :: BasicEffect -> FinalEffect -> FinalEffect
thenFinalEffect e1 e2 =
  FinalEffect
  { stack' = Stack.thenFinalStackEffect (stack e1) (stack' e2)
  , memory' = memory e1 >>> memory' e2
  , registers' = registers e1 >>> registers' e2
  , subroutines' = subroutines e1 <> subroutines' e2
  , branch' = branch e1 || branch' e2
  }

jsrFinalEffect :: FinalEffect -> FinalEffect -> FinalEffect
jsrFinalEffect subroutine after =
  FinalEffect
  { stack' = Stack.jsrFinalStackEffect (stack' subroutine) (stack' after)
  , memory' = memory' subroutine >>> memory' after
  , registers' = registers' subroutine >>> registers' after
  , subroutines' = {- subroutines' subroutine <> -} subroutines' after
  , branch' = branch' subroutine || branch' after
  }

brkFinalEffect :: FinalEffect
brkFinalEffect = mempty { stack' = Stack.brkEffect }

rtiFinalEffect :: FinalEffect
rtiFinalEffect = mempty { stack' = Stack.rtiEffect }

rtsFinalEffect :: FinalEffect
rtsFinalEffect = mempty { stack' = Stack.rtsEffect }

jmpAbsFinalEffect :: Int -> FinalEffect
jmpAbsFinalEffect addr = mempty { stack' = Stack.jmpAbsEffect addr }

jmpIndFinalEffect :: Word16 -> FinalEffect
jmpIndFinalEffect addr = mempty { stack' = Stack.jmpIndEffect addr }

undocFinalEffect :: Word8 -> FinalEffect
undocFinalEffect op = mempty { stack' = Stack.undocEffect op }

--------------------------------------------------------------------------------
-- * Normal subroutines

normalSubroutine :: FinalEffect -> Bool
normalSubroutine e =
  and
  [ Stack.normalStackEffect (stack' e)
  ]

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
  filter (not . null) $
  [ Reg.ppRegEffect (registers' e)
  , Stack.ppFinalStackEffect (stack' e)
  , Mem.ppMemEffect (memory' e)
  , ppSubroutines (subroutines' e)
  , if branch' e then "Branches" else ""
  ]
