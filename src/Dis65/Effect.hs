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
-- * FinalStackEffect

data FinalStackEffect =
  FinalStackEffect
  { loop :: Maybe Stack.StackEffect
  , rts :: Maybe Stack.StackEffect
  , rti :: Maybe Stack.StackEffect
  , brk :: Maybe Stack.StackEffect
  , undoc :: Map Word8 Stack.StackEffect
  , jmpAbs :: IntMap Stack.StackEffect
  , jmpInd :: IntMap Stack.StackEffect
  }
  deriving (Eq, Show)

combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe _ Nothing y = y
combineMaybe _ x Nothing = x
combineMaybe f (Just x) (Just y) = Just (f x y)

instance Semigroup FinalStackEffect where
  e1 <> e2 =
    FinalStackEffect
    { loop = combineMaybe (+++) (loop e1) (loop e2)
    , rts = combineMaybe (+++) (rts e1) (rts e2)
    , rti = combineMaybe (+++) (rti e1) (rti e2)
    , brk = combineMaybe (+++) (brk e1) (brk e2)
    , undoc = Map.unionWith (+++) (undoc e1) (undoc e2)
    , jmpAbs = IntMap.unionWith (+++) (jmpAbs e1) (jmpAbs e2)
    , jmpInd = IntMap.unionWith (+++) (jmpInd e1) (jmpInd e2)
    }

instance Monoid FinalStackEffect where
  mempty =
    FinalStackEffect
    { loop = Nothing
    , rts = Nothing
    , rti = Nothing
    , brk = Nothing
    , undoc = Map.empty
    , jmpAbs = IntMap.empty
    , jmpInd = IntMap.empty
    }

instance Bottom FinalStackEffect where
  bottom =
    FinalStackEffect
    { loop = Just bottom
    , rts = Nothing
    , rti = Nothing
    , brk = Nothing
    , undoc = Map.empty
    , jmpAbs = IntMap.empty
    , jmpInd = IntMap.empty
    }

thenFinalStackEffect :: Stack.StackEffect -> FinalStackEffect -> FinalStackEffect
thenFinalStackEffect e1 e2 =
  FinalStackEffect
  { loop = fmap (e1 >>>) (loop e2)
  , rts = fmap (e1 >>>) (rts e2)
  , rti = fmap (e1 >>>) (rti e2)
  , brk = fmap (e1 >>>) (brk e2)
  , undoc = fmap (e1 >>>) (undoc e2)
  , jmpAbs = fmap (e1 >>>) (jmpAbs e2)
  , jmpInd = fmap (e1 >>>) (jmpInd e2)
  }

jsrFinalStackEffect :: FinalStackEffect -> FinalStackEffect -> FinalStackEffect
jsrFinalStackEffect subroutine after =
  case rts subroutine of
    Nothing -> thenFinalStackEffect push2 subroutine
    Just body ->
      thenFinalStackEffect push2 subroutine { rts = Nothing } <>
      thenFinalStackEffect (push2 >>> body >>> pull2) after

push2 :: Stack.StackEffect
push2 = Stack.push >>> Stack.push

pull2 :: Stack.StackEffect
pull2 = Stack.pull >>> Stack.pull

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

--------------------------------------------------------------------------------
-- * FinalEffect

-- | The effect of running an instruction all the way to a
-- block-ending instruction: Either an RTS, RTI, BRK or crash.

data FinalEffect =
  FinalEffect
  { stack' :: !FinalStackEffect
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
  { stack' = thenFinalStackEffect (stack e1) (stack' e2)
  , memory' = memory e1 >>> memory' e2
  , registers' = registers e1 >>> registers' e2
  , subroutines' = subroutines e1 <> subroutines' e2
  , branch' = branch e1 || branch' e2
  }

jsrFinalEffect :: FinalEffect -> FinalEffect -> FinalEffect
jsrFinalEffect subroutine after =
  FinalEffect
  { stack' = jsrFinalStackEffect (stack' subroutine) (stack' after)
  , memory' = memory' subroutine >>> memory' after
  , registers' = registers' subroutine >>> registers' after
  , subroutines' = {- subroutines' subroutine <> -} subroutines' after
  , branch' = branch' subroutine || branch' after
  }

brkFinalEffect :: FinalEffect
brkFinalEffect =
  mempty { stack' = mempty { brk = Just noEffect } }

rtiFinalEffect :: FinalEffect
rtiFinalEffect =
  mempty { stack' = mempty { rti = Just noEffect } }

rtsFinalEffect :: FinalEffect
rtsFinalEffect =
  mempty { stack' = mempty { rts = Just noEffect } }

jmpAbsFinalEffect :: Int -> FinalEffect
jmpAbsFinalEffect addr =
  mempty { stack' = mempty { jmpAbs = IntMap.singleton addr noEffect } }

jmpIndFinalEffect :: Word16 -> FinalEffect
jmpIndFinalEffect addr =
  mempty { stack' = mempty { jmpInd = IntMap.singleton (fromIntegral addr) noEffect } }

undocFinalEffect :: Word8 -> FinalEffect
undocFinalEffect op =
  mempty { stack' = mempty { undoc = Map.singleton op noEffect } }


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

ppFinalStackEffect :: FinalStackEffect -> String
ppFinalStackEffect e =
  unwords $
  catMaybes $
  [ fmap (prefix "LOOP" . Stack.ppStackEffect) (loop e)
  , fmap (prefix "RTS" . Stack.ppStackEffect) (rts e)
  , fmap (prefix "RTI" . Stack.ppStackEffect) (rti e)
  , fmap (prefix "BRK" . Stack.ppStackEffect) (brk e)
  ] ++
  [ Just $ prefix (ppWord8 op) $ Stack.ppStackEffect be
  | (op, be) <- Map.assocs (undoc e) ]
  ++
  [ Just $ prefix ("JMP $" ++ ppWord16 (fromIntegral a)) $ Stack.ppStackEffect be
  | (a, be) <- IntMap.assocs (jmpAbs e) ]
  ++
  [ Just $ prefix ("JMP ($" ++ ppWord16 (fromIntegral a) ++ ")") $ Stack.ppStackEffect be
  | (a, be) <- IntMap.assocs (jmpInd e) ]
  where
    prefix s1 s2 = if null s2 then s1 else s1 ++ ": " ++ s2

ppFinalEffect :: FinalEffect -> [String]
ppFinalEffect e =
  filter (not . null) $
  [ Reg.ppRegEffect (registers' e)
  , ppFinalStackEffect (stack' e)
  , Mem.ppMemEffect (memory' e)
  , ppSubroutines (subroutines' e)
  , if branch' e then "Branches" else ""
  ]
