{-# LANGUAGE FlexibleContexts #-}
module MassivTalk.Day1.Intro03_DelayedVsManifest where

import Prelude as P
import Data.Massiv.Array as A

---------------------
-- Class Hierarchy --
---------------------



-- class (Typeable r, Index ix) => Construct r ix e where

-- class (Typeable r, Index ix) => Load r ix e where

-- class Load r ix e => Source r ix e where

-- class (Load r ix e, Source r ix e) => Manifest r ix e where

-- class Manifest r ix e => Mutable r ix e where


-- | Fuse computation and avoid array allocations

paraboloid :: Int -> Array D Ix2 Double
paraboloid n = makeArrayR D Par (Sz2 n n) $ \(i :. j) -> f (i - n2) + f (j - n2)
  where
    n2 = n `div` 2
    f x = fromIntegral (x * x)

--
-- >>> arr = paraboloid 5
-- >>> A.sum $ A.map (+10) arr
-- 350.0

-- >>> :t makeArrayR
-- makeArrayR
--   :: Construct r ix e =>
--      r -> Comp -> Sz ix -> (ix -> e) -> Array r ix e

-- | Intermediate large size arrays

ridiculousSquare :: Int -> Array D Ix2 Double
ridiculousSquare k =
  makeArray Seq (Sz2 k k) $ \(i :. j) -> fromIntegral i ** sin (fromIntegral j)

--
-- >>> arr = ridiculousSquare maxBound
-- >>> computeAs U arr ! 0
-- 1.0
-- >>> maxBound * maxBound ::Int
-- 1
-- >>> evaluateM arr (123456765432134 :. 2345677654345678)
-- 1.6379884486833446e-7

-- >>> minBound * (-1) :: Int
-- -9223372036854775808
-- >>> minBound `div` (-1) :: Int
-- *** Exception: arithmetic overflow

-- >>> abs (minBound :: Int)
-- -9223372036854775808


-- | Loading of such array into memory in full is infeasible

--
-- >>> stride = Stride (maxBound `div` 3 :. maxBound `div` 2)
-- >>> print stride
-- Stride (3074457345618258602 :. 4611686018427387903)
-- >>> computeWithStrideAs U stride $ ridiculousSquare maxBound
-- Array U Seq (Sz (4 :. 3))
--   [ [ 1.0, Infinity, 0.0 ]
--   , [ 1.0, 1.0104913692989058e-13, 3.065358632054087e18 ]
--   , [ 1.0, 6.207716406232797e-14, 6.130421407659876e18 ]
--   , [ 1.0, 4.6682420926494095e-14, 9.195372524026043e18 ]
--   ]


-- | Indices that are too large are dangerous
-- | As long as the total number of elements is below `maxBound :: Int`, we are ok.

--
-- >>> arr = ridiculousSquare maxBound
-- >>> extractM 0 (Sz2 5 6) arr


-- | Computation


--
-- >>> :t computeAs
-- computeAs
--   :: (Mutable r ix e, Load r' ix e) =>
--      r -> Array r' ix e -> Array r ix e

-- | The process of computation:
--
-- * Take a loadable array
-- * Allocate a mutable array of the same size
-- * Load each element of the loadable array into the new mutable according to
--   the computation strategy
-- * Freeze the mutable array and get as a result the pure manifest array.



-- | Fusion of computation and some pitfalls

--
-- >>> arr = A.map (sin . (+10)) $ paraboloid 3
-- >>> computeAs P $ A.zipWith (+) arr (A.map cos arr)
-- Array P Par (Sz (3 :. 3))
--   [ [ 0.3228927092740881, -0.4596796598050503, 0.3228927092740881 ]
--   , [ -0.4596796598050503, 0.31161324393199685, -0.4596796598050503 ]
--   , [ 0.3228927092740881, -0.4596796598050503, 0.3228927092740881 ]
--   ]

--
-- >>> arr = computeAs P $ A.map (sin . (+10)) $ paraboloid 3
-- >>> computeAs P $ A.zipWith (+) arr (A.map cos arr)
-- Array P Par (Sz (3 :. 3))
--   [ [ 0.3228927092740881, -0.4596796598050503, 0.3228927092740881 ]
--   , [ -0.4596796598050503, 0.31161324393199685, -0.4596796598050503 ]
--   , [ 0.3228927092740881, -0.4596796598050503, 0.3228927092740881 ]
--   ]

{- Rule of thumb. If delayed array is used more than once, compute it. -}

--
-- >>> arr = computeAs P $ A.map (sin . (+10)) $ paraboloid 3
-- >>> arr
-- Array P Par (Sz (3 :. 3))
--   [ [ -0.5365729180004349, -0.9999902065507035, -0.5365729180004349 ]
--   , [ -0.9999902065507035, -0.5440211108893698, -0.9999902065507035 ]
--   , [ -0.5365729180004349, -0.9999902065507035, -0.5365729180004349 ]
--   ]
-- >>> arr ! 1 :. 1
-- -0.5440211108893698






----------------
-- Push array --
----------------



identityD :: Int -> Array D Ix2 Int
identityD n =
  makeArray Seq (Sz2 n n) $ \(i :. j) ->
    if i == j
      then 1
      else 0

--
-- >>> identityD 5
-- Array D Seq (Sz (5 :. 5))
--   [ [ 1, 0, 0, 0, 0 ]
--   , [ 0, 1, 0, 0, 0 ]
--   , [ 0, 0, 1, 0, 0 ]
--   , [ 0, 0, 0, 1, 0 ]
--   , [ 0, 0, 0, 0, 1 ]
--   ]


-- >>> :t makeLoadArrayS
-- makeLoadArrayS
--   :: Index ix =>
--      Sz ix
--      -> e
--      -> (forall m . Monad m => (ix -> e -> m Bool) -> m ())
--      -> Array DL ix e


identityDL :: Int -> Array DL Ix2 Int
identityDL n = makeLoadArrayS (Sz2 n n) 0 $ \ writeCell -> do
  let f i = writeCell (i :. i) 1
  A.mapM_ f (0 ... n - 1)

  -- Same as:
  -- P.mapM_ f [0 .. n - 1]

-- >>> identityDL 5

