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
import qualified Dis65.Effect.Flag as Flag

import Dis65.Addr

--------------------------------------------------------------------------------
-- * BasicEffect

data BasicEffect =
  BasicEffect
  { stack :: !Stack.StackEffect
  , memory :: !Mem.MemEffect
  , registers :: !Reg.RegEffect
  , flags :: !Flag.FlagEffect
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
    , flags = flags e1 +++ flags e2
    , subroutines = subroutines e1 <> subroutines e2
    , branch = branch e1 || branch e2
    }

instance Effect BasicEffect where
  e1 >>> e2 =
    BasicEffect
    { stack = stack e1 >>> stack e2
    , memory = memory e1 >>> memory e2
    , registers = registers e1 >>> registers e2
    , flags = flags e1 >>> flags e2
    , subroutines = subroutines e1 <> subroutines e2
    , branch = branch e1 || branch e2
    }

instance NoEffect BasicEffect where
  noEffect =
    BasicEffect
    { stack = noEffect
    , memory = noEffect
    , registers = noEffect
    , flags = noEffect
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
  , flags' :: !Flag.FlagEffect
  , subroutines' :: !IntSet
  , branch' :: !Bool
  , loop' :: !Bool -- ^ Whether it is possible to enter an infinite loop
  }
  deriving (Eq, Show)

instance Semigroup FinalEffect where
  e1 <> e2 =
    FinalEffect
    { stack' = stack' e1 <> stack' e2
    , memory' = memory' e1 +++ memory' e2
    , registers' = registers' e1 +++ registers' e2
    , flags' = flags' e1 +++ flags' e2
    , subroutines' = subroutines' e1 <> subroutines' e2
    , branch' = branch' e1 || branch' e2
    , loop' = loop' e1 || loop' e2
    }

instance Monoid FinalEffect where
  mempty =
    FinalEffect
    { stack' = bottom
    , memory' = bottom
    , registers' = bottom
    , flags' = bottom
    , subroutines' = mempty
    , branch' = False
    , loop' = False
    }

instance Bottom FinalEffect where
  bottom =
    FinalEffect
    { stack' = bottom
    , memory' = bottom
    , registers' = bottom
    , flags' = bottom
    , subroutines' = mempty
    , branch' = False
    , loop' = True
    }

thenFinalEffect :: BasicEffect -> FinalEffect -> FinalEffect
thenFinalEffect e1 e2 =
  FinalEffect
  { stack' = Stack.thenFinalStackEffect (stack e1) (stack' e2)
  , memory' = memory e1 >>> memory' e2
  , registers' = registers e1 >>> registers' e2
  , flags' = flags e1 >>> flags' e2
  , subroutines' = subroutines e1 <> subroutines' e2
  , branch' = branch e1 || branch' e2
  , loop' = loop' e2
  }

jsrFinalEffect :: FinalEffect -> FinalEffect -> FinalEffect
jsrFinalEffect subroutine after =
  FinalEffect
  { stack' = Stack.jsrFinalStackEffect (stack' subroutine) (stack' after)
  , memory' = memory' subroutine >>> memory' after
  , registers' = registers' subroutine >>> registers' after
  , flags' = flags' subroutine >>> flags' after
  , subroutines' = {- subroutines' subroutine <> -} subroutines' after
  , branch' = branch' subroutine || branch' after
  , loop' = loop' subroutine || loop' after
  }

mkFinalEffect :: Stack.FinalStackEffect -> FinalEffect
mkFinalEffect s =
  FinalEffect
    { stack' = s
    , memory' = noEffect
    , registers' = noEffect
    , flags' = noEffect
    , subroutines' = mempty
    , branch' = False
    , loop' = False
    }

brkFinalEffect :: FinalEffect
brkFinalEffect = mkFinalEffect Stack.brkEffect

rtiFinalEffect :: FinalEffect
rtiFinalEffect = mkFinalEffect Stack.rtiEffect

rtsFinalEffect :: FinalEffect
rtsFinalEffect = mkFinalEffect Stack.rtsEffect

jmpAbsFinalEffect :: Int -> FinalEffect
jmpAbsFinalEffect addr = mkFinalEffect (Stack.jmpAbsEffect addr)

jmpIndFinalEffect :: Word16 -> FinalEffect
jmpIndFinalEffect addr = mkFinalEffect (Stack.jmpIndEffect addr)

undocFinalEffect :: Word8 -> FinalEffect
undocFinalEffect op = mkFinalEffect (Stack.undocEffect op)

--------------------------------------------------------------------------------
-- * Normal subroutines

normalSubroutine :: FinalEffect -> Bool
normalSubroutine e =
  and
  [ Stack.normalStackEffect (stack' e)
  ]

--------------------------------------------------------------------------------
-- * Pretty printing

-- | Parameterized by a printer for labels.
ppSubroutines :: (Addr -> String) -> IntSet -> String
ppSubroutines label s
  | IntSet.null s = ""
  | otherwise =
    unwords ("JSR" : map label (IntSet.elems s))

-- | Parameterized by a printer for labels.
ppBasicEffect :: (Addr -> String) -> BasicEffect -> String
ppBasicEffect label e =
  unwords $
  filter (not . null) $
  [ Reg.ppRegEffect (registers e)
  , Flag.ppFlagEffect (flags e)
  , Stack.ppStackEffect (stack e)
  ] ++
  Mem.ppMemEffect (label . fromIntegral) (memory e) ++
  [ ppSubroutines label (subroutines e)
  , if branch e then "Branches" else ""
  ]

-- | Parameterized by a printer for labels.
ppFinalEffect :: (Addr -> String) -> FinalEffect -> [String]
ppFinalEffect label e =
  filter (not . null) $
  [ Reg.ppRegEffect (registers' e)
  , Flag.ppFlagEffect (flags' e)
  , Stack.ppFinalStackEffect (stack' e)
  ] ++
  Mem.ppMemEffect (label . addr16) (memory' e) ++
  [ ppSubroutines label (subroutines' e)
  , if branch' e then "Branches" else ""
  , if loop' e then "Loops" else ""
  ]
