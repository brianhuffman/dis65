module Dis65.Effect.Stack
  ( StackEffect
  , push
  , pull
  , ppStackEffect
  , FinalStackEffect
  , thenFinalStackEffect
  , jsrFinalStackEffect
  , ppFinalStackEffect
  , ppFinalStackEffect'
  , brkEffect
  , rtiEffect
  , rtsEffect
  , jmpAbsEffect
  , jmpIndEffect
  , undocEffect
  , normalStackEffect
  ) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word

import Dis65.Addr (ppWord8, ppWord16)
import Dis65.Effect.Class

-- | Represents a contiguous subrange of integers between -255 and
-- +255.
data StackRange = StackRange !Int !Int
  deriving (Eq, Show)

instance Choice StackRange where
  StackRange lo1 hi1 +++ StackRange lo2 hi2 =
    StackRange (min lo1 lo2) (max hi1 hi2)

instance Effect StackRange where
  StackRange lo1 hi1 >>> StackRange lo2 hi2 = StackRange lo3 hi3
    where
      lo3
        | lo1 == -255 = lo1
        | lo2 == -255 = lo2
        | otherwise = max (-255) (min 255 (lo1 + lo2))
      hi3
        | hi1 == 255 = hi1
        | hi2 == 255 = hi2
        | otherwise = max (-255) (min 255 (hi1 + hi2))

instance NoEffect StackRange where
  noEffect = StackRange 0 0

-- | A 'StackEffect' contains two 'StackRanges': The first is the
-- range of possible stack heights (relative to 0 at the start) that
-- the computation can take on at any time during its execution. The
-- second is the range of possible stack heights at its conclusion,
-- which may be empty if the computation is not known to terminate.
--
-- Invariants: The first range must include 0, and it must also
-- include the second range.
data StackEffect = StackEffect !StackRange !StackRange
  deriving (Eq, Show)

instance Choice StackEffect where
  StackEffect mid1 end1 +++ StackEffect mid2 end2 =
    StackEffect (mid1 +++ mid2) (end1 +++ end2)

instance Effect StackEffect where
  StackEffect mid1 end1 >>> StackEffect mid2 end2 =
    StackEffect (mid1 +++ (end1 >>> mid2)) (end1 >>> end2)

instance NoEffect StackEffect where
  noEffect = StackEffect noEffect noEffect

push :: StackEffect
push = StackEffect (StackRange 0 1) (StackRange 1 1)

pull :: StackEffect
pull = StackEffect (StackRange (-1) 0) (StackRange (-1) (-1))

push2 :: StackEffect
push2 = push >>> push

pull2 :: StackEffect
pull2 = pull >>> pull

-- | For pretty printing, we offset the start and end values so that
-- we never have to display negative numbers.
ppStackEffect :: StackEffect -> String
ppStackEffect (StackEffect (StackRange a b) (StackRange c d))
  | (a, b, c, d) == (0, 0, 0, 0) = ""
  | c /= d = show (a,c,d,b)
  | b == max 0 c = show (0 - a) ++ "->" ++ show (c - a)
  | otherwise = show (0 - a) ++ "->" ++ show (b - a) ++ "->" ++ show (c - a)

--------------------------------------------------------------------------------
-- * FinalStackEffect

data FinalStackEffect =
  FinalStackEffect
  { mid :: StackRange -- ^ Possible range of stack heights over duration of computation
  , rts :: Maybe StackRange
  , rti :: Maybe StackRange
  , undoc :: Set Word8
  , jmpAbs :: IntMap StackRange
  , jmpInd :: IntMap StackRange
  }
  deriving (Eq, Show)

combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe _ Nothing y = y
combineMaybe _ x Nothing = x
combineMaybe f (Just x) (Just y) = Just (f x y)

-- Should be Choice
instance Semigroup FinalStackEffect where
  e1 <> e2 =
    FinalStackEffect
    { mid = mid e1 +++ mid e2
    , rts = combineMaybe (+++) (rts e1) (rts e2)
    , rti = combineMaybe (+++) (rti e1) (rti e2)
    , undoc = Set.union (undoc e1) (undoc e2)
    , jmpAbs = IntMap.unionWith (+++) (jmpAbs e1) (jmpAbs e2)
    , jmpInd = IntMap.unionWith (+++) (jmpInd e1) (jmpInd e2)
    }

instance Monoid FinalStackEffect where
  mempty =
    FinalStackEffect
    { mid = noEffect
    , rts = Nothing
    , rti = Nothing
    , undoc = Set.empty
    , jmpAbs = IntMap.empty
    , jmpInd = IntMap.empty
    }

instance Bottom FinalStackEffect where
  bottom =
    FinalStackEffect
    { mid = noEffect
    , rts = Nothing
    , rti = Nothing
    , undoc = Set.empty
    , jmpAbs = IntMap.empty
    , jmpInd = IntMap.empty
    }

thenFinalStackEffect :: StackEffect -> FinalStackEffect -> FinalStackEffect
thenFinalStackEffect (StackEffect mid1 end1) e2 =
  FinalStackEffect
  { mid = mid1 +++ (end1 >>> mid e2)
  , rts = fmap (end1 >>>) (rts e2)
  , rti = fmap (end1 >>>) (rti e2)
  , undoc = undoc e2
  , jmpAbs = fmap (end1 >>>) (jmpAbs e2)
  , jmpInd = fmap (end1 >>>) (jmpInd e2)
  }

jsrFinalStackEffect :: FinalStackEffect -> FinalStackEffect -> FinalStackEffect
jsrFinalStackEffect subroutine after =
  case rts subroutine of
    Nothing -> thenFinalStackEffect push2 subroutine
    Just body ->
      thenFinalStackEffect push2 subroutine { rts = Nothing } <>
      thenFinalStackEffect (push2 >>> StackEffect (mid subroutine) body >>> pull2) after

ppFinalStackEffect :: FinalStackEffect -> String
ppFinalStackEffect e =
  unwords $
  catMaybes $
  [ fmap (prefix "RTS" . ppStackEffect') (rts e)
  , fmap (prefix "RTI" . ppStackEffect') (rti e)
  ] ++
  [ Just (ppWord8 op) | op <- Set.elems (undoc e) ]
  ++
  [ Just $ prefix ("JMP $" ++ ppWord16 (fromIntegral a)) $ ppStackEffect' be
  | (a, be) <- IntMap.assocs (jmpAbs e) ]
  ++
  [ Just $ prefix ("JMP ($" ++ ppWord16 (fromIntegral a) ++ ")") $ ppStackEffect' be
  | (a, be) <- IntMap.assocs (jmpInd e) ]
  where
    prefix s1 s2 = if null s2 then s1 else s1 ++ ": " ++ s2
    ppStackEffect' end = ppStackEffect (StackEffect (mid e) end)

ppFinalStackEffect' :: FinalStackEffect -> String
ppFinalStackEffect' e =
  case rts e of
    Nothing -> "-"
    Just (StackRange lo hi)
      | lo == hi -> show (- lo)
      | otherwise -> "?"

brkEffect :: FinalStackEffect
brkEffect = undocEffect 0x00

rtiEffect :: FinalStackEffect
rtiEffect = mempty { rti = Just noEffect }

rtsEffect :: FinalStackEffect
rtsEffect = mempty { rts = Just noEffect }

jmpAbsEffect :: Int -> FinalStackEffect
jmpAbsEffect addr = mempty { jmpAbs = IntMap.singleton addr noEffect }

jmpIndEffect :: Word16 -> FinalStackEffect
jmpIndEffect addr = mempty { jmpInd = IntMap.singleton (fromIntegral addr) noEffect }

undocEffect :: Word8 -> FinalStackEffect
undocEffect op = mempty { undoc = Set.singleton op }

--------------------------------------------------------------------------------
-- * Normal subroutines

normalStackEffect :: FinalStackEffect -> Bool
normalStackEffect e =
  and
  [ case mid e of StackRange lo _ -> lo == 0
  , maybe False (== noEffect) (rts e)
  , rti e == Nothing
  , Set.null (undoc e)
  , IntMap.null (jmpAbs e)
  , IntMap.null (jmpInd e)
  ]
