module Dis65.Effect.Class where

import Control.Applicative (liftA2)

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

class Effect a where

  -- | Parallel composition. It should be associative, commutative,
  -- and idempotent. This can be considered as the least-upper-bound
  -- operator for an information ordering on type @a@.
  (+++) :: a -> a -> a

  -- | Sequential composition. It should be associative and distribute
  -- over parallel composition. This implies that it is monotone on
  -- the information ordering induced by '(+++)'.
  (>>>) :: a -> a -> a

class Effect a => NoEffect a where
  -- | The identity element for sequential composition '(>>>)'.
  noEffect :: a

{- Laws:
(x +++ x) = x
(x +++ y) = (y +++ x)
(x +++ y) >>> z = (x >>> z) +++ (y >>> z)
x >>> (y +++ z) = (x >>> y) +++ (x >>> z)


So (+++) is kind of like addition, while (>>>) is like multiplication.

(+++,>>>) form an idempotent semiring.

Should we require 0, 1 elements?
0 would mean a computation that crashes.
1 would mean a computation that does nothing.

-}

{-
-- | 'Nothing' is an identity element for both parallel and sequential
-- composition.
instance Effect a => Effect (Maybe a) where
  (+++) = liftA2 (+++)
  (>>>) = liftA2 (>>>)

instance Effect a => NoEffect (Maybe a) where
  noEffect = Nothing

-- | Parallel and sequential composition of maps is done elementwise.
-- The empty map is an identity element for both parallel and
-- sequential composition.
instance (Ord k, Effect a) => Effect (Map.Map k a) where
  (+++) = Map.unionWith (+++)
  (>>>) = Map.unionWith (>>>)

instance (Ord k, Effect a) => NoEffect (Map.Map k a) where
  noEffect = Map.empty

-- | Parallel and sequential composition of maps is done elementwise.
-- The empty map is an identity element for both parallel and
-- sequential composition.
instance (Effect a) => Effect (IntMap.IntMap a) where
  (+++) = IntMap.unionWith (+++)
  (>>>) = IntMap.unionWith (>>>)

instance (Effect a) => NoEffect (IntMap.IntMap a) where
  noEffect = IntMap.empty

-- | Parallel and sequential composition of tuples is done elementwise.
instance (Effect a, Effect b) => Effect (a, b) where
  (a1, b1) +++ (a2, b2) = (a1 +++ a2, b1 +++ b2)
  (a1, b1) >>> (a2, b2) = (a1 >>> a2, b1 >>> b2)

instance (NoEffect a, NoEffect b) => NoEffect (a, b) where
  noEffect = (noEffect, noEffect)

-- | Parallel and sequential composition of tuples is done elementwise.
instance (Effect a, Effect b, Effect c) => Effect (a, b, c) where
  (a1, b1, c1) +++ (a2, b2, c2) = (a1 +++ a2, b1 +++ b2, c1 +++ c2)
  (a1, b1, c1) >>> (a2, b2, c2) = (a1 >>> a2, b1 >>> b2, c1 >>> c2)

instance (NoEffect a, NoEffect b, NoEffect c) => NoEffect (a, b, c) where
  noEffect = (noEffect, noEffect, noEffect)
-}
