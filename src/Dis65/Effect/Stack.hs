module Dis65.Effect.Stack
  ( StackEffect
  , push
  , pull
  , ppStackEffect
  ) where

import Dis65.Effect.Class

-- | Represents a contiguous subrange of integers between -255 and
-- +255.
data StackRange = StackRange !Int !Int
  deriving (Eq, Show)

instance Effect StackRange where
  StackRange lo1 hi1 +++ StackRange lo2 hi2 =
    StackRange (min lo1 lo2) (max hi1 hi2)
  StackRange lo1 hi1 >>> StackRange lo2 hi2 = StackRange lo3 hi3
    where
      lo3
        | lo1 == -255 = lo1
        | lo2 == -255 = lo2
        | otherwise = max (-255) (lo1 + lo2)
      hi3
        | hi1 == 255 = hi1
        | hi2 == 255 = hi2
        | otherwise = min 255 (hi1 + hi2)

instance NoEffect StackEffect where
  noEffect = StackEffect (StackRange 0 0) (StackRange 0 0)

data StackEffect = StackEffect !StackRange !StackRange
  deriving (Eq, Show)

instance Effect StackEffect where

  StackEffect mid1 end1 +++ StackEffect mid2 end2 =
    StackEffect (mid1 +++ mid2) (end1 +++ end2)

  StackEffect mid1 end1 >>> StackEffect mid2 end2 =
    StackEffect (mid1 +++ (end1 >>> mid2)) (end1 >>> end2)

push :: StackEffect
push = StackEffect (StackRange 0 1) (StackRange 1 1)

pull :: StackEffect
pull = StackEffect (StackRange (-1) 0) (StackRange (-1) (-1))

ppStackEffect :: StackEffect -> String
ppStackEffect (StackEffect (StackRange a b) (StackRange c d))
  | (a, b, c, d) == (0, 0, 0, 0) = ""
  | c /= d = show (a,c,d,b)
  | b == c = show (b - a) ++ "->" ++ show (0 - a)
  | otherwise = show (c - a) ++ "->" ++ show (b - a) ++ "->" ++ show (0 - a)
