{-# OPTIONS_GHC -Wno-unused-imports #-}
module MassivTalk.Day1.Intro02_Index where

import Data.Massiv.Array as A

-------------
-- Indices --
-------------


-- | Started simple, with something that we are all familar with: tuples

--
-- >>> (1,1,1) ... (2,3,4) :: Array D (Int, Int, Int) (Int, Int, Int)

--
-- >>> (1,1,1) ... (2,3,4) :: Array D Ix3T Ix3T

--
-- >>> (1, 2, 3, 4, 5) :: Ix5T


-- | Problems with Tuples:
-- * lazy
-- * too polymorphic
-- * don't scale to an arbitrary dimension:



--
-- >>> (4 :. 5)
-- 4 :. 5

--
-- >>> (1 :> 2 :> 3 :> 4 :. 5) :: Ix5

--
-- >>> 5

--
-- >>> 5 :: Ix1
-- 5

--
-- >>> Ix1 5
-- 5

--
-- >>> (1 :> 1 :. 1) ... (2 :> 3 :. 4)
-- Array D Seq (Sz (2 :> 3 :. 4))
--   [ [ [ 1 :> 1 :. 1, 1 :> 1 :. 2, 1 :> 1 :. 3, 1 :> 1 :. 4 ]
--     , [ 1 :> 2 :. 1, 1 :> 2 :. 2, 1 :> 2 :. 3, 1 :> 2 :. 4 ]
--     , [ 1 :> 3 :. 1, 1 :> 3 :. 2, 1 :> 3 :. 3, 1 :> 3 :. 4 ]
--     ]
--   , [ [ 2 :> 1 :. 1, 2 :> 1 :. 2, 2 :> 1 :. 3, 2 :> 1 :. 4 ]
--     , [ 2 :> 2 :. 1, 2 :> 2 :. 2, 2 :> 2 :. 3, 2 :> 2 :. 4 ]
--     , [ 2 :> 3 :. 1, 2 :> 3 :. 2, 2 :> 3 :. 3, 2 :> 3 :. 4 ]
--     ]
--   ]


-- |
--
-- data Ix2 = Int :. Int
--
-- infixr 5 :.
--
-- data IxN (n :: Nat)
--   = Int :> Ix (n - 1)
--
-- infixr 5 :>
--
-- type family Ix (n :: Nat) = r | r -> n where
--   Ix 0 = Ix0
--   Ix 1 = Ix1
--   Ix 2 = Ix2
--   Ix n = IxN n
--


--
-- >>> 1 :> 10 :. 20 * 2 + 7
-- 1 :> 10 :. 47

--
-- >>> (1 :> 10 :. 20) * (2 :> 2 :. 2) + (7 :> 7 :. 7)
-- 9 :> 27 :. 47

-- 9 :> 27 :. 47



-- | Many functions that operate on indices of any dimension:

--
-- >>> :t foldlIndex
-- foldlIndex :: Index ix => (a -> Int -> a) -> a -> ix -> a

--
-- >>> foldlIndex (flip (:)) [] (2 :> 3 :> 4 :. 5)
-- [5,4,3,2]


-- >>> zeroIndex :: Ix3
-- 0 :> 0 :. 0
-- >>> oneIndex :: Ix3
-- 1 :> 1 :. 1
-- >>> pureIndex 4 :: Ix3
-- 4 :> 4 :. 4

--
-- >>> :t iter
-- iter
--   :: Index ix =>
--      ix -> ix -> ix -> (Int -> Int -> Bool) -> a -> (ix -> a -> a) -> a

--
-- >>> iter zeroIndex (3 :> 2 :. 1) oneIndex (<) [] (:)
-- [2 :> 1 :. 0,2 :> 0 :. 0,1 :> 1 :. 0,1 :> 0 :. 0,0 :> 1 :. 0,0 :> 0 :. 0]


-- | Reducing dimension by one:

--
-- >>> unconsDim (3 :> 2 :. 1)
-- (3,2 :. 1)

--
-- >>> unsnocDim (3 :> 2 :. 1)
-- (3 :. 2,1)

-- | Working with specific dimensions:
--
-- * Order of dimensions:

--
-- >>> getDim' (5 :> 46 :> 3 :> 2 :. 1) 4
-- 46

-- * Partial functions:

--
-- >>> getDim' (5 :> 4 :> 3 :> 2 :. 1) 6
-- *** Exception: IndexDimensionException: (Dim 6) for 3 :> 2 :. 1

-- * Safer alternatives:

--
-- >>> getDimM (5 :> 4 :> 3 :> 2 :. 1) 6 :: Either SomeException Int
-- Left (IndexDimensionException: (Dim 6) for 3 :> 2 :. 1)

-- * Even safer alternatives:

--
-- >>> getDimension (5 :> 4 :> 3 :> 2 :. 1) Dim4
-- 4

--
-- >>> :set -XDataKinds
-- >>> getDimension (5 :> 4 :> 3 :> 2 :. 1) (DimN :: Dimension 6)

-- >>> :i Dim

----------
-- Size --
----------

--
-- >>> Sz (3 :> 2 :. 1)

-- * Promote type safety

--
-- >>> :t makeArrayR
-- makeArrayR
--   :: Construct r ix e =>
--      r -> Comp -> Sz ix -> (ix -> e) -> Array r ix e

--
-- >>> makeArrayR D Seq (2 :. 3) $ \ (i :. j) -> i + j

-- * Prevent easy mistakes of mixing index and size

--
-- >>> Sz (3 :> 2 :. 1) + (1 :> 2 :. 3)
-- <interactive>:156:22-32: error:
--     • Couldn't match expected type ‘Sz (IxN 3)’
--                   with actual type ‘IxN 3’
--     • In the second argument of ‘(+)’, namely ‘(1 :> 2 :. 3)’
--       In the expression: Sz (3 :> 2 :. 1) + (1 :> 2 :. 3)
--       In an equation for ‘it’: it = Sz (3 :> 2 :. 1) + (1 :> 2 :. 3)

--
-- * Enforce valid size

--
-- >>> Sz (-3 :> 2 :. 1)
-- Sz (0 :> 2 :. 1)

-- * Helper pattern synonyms

--
-- >>> Sz3 3 2 1
-- Sz (3 :> 2 :. 1)

--
-- >>> Sz4 4 3 2 1 == Sz (4 :> 3 :> 2 :. 1)
-- True

------------
-- Stride --
------------

--
-- >>> computeWithStrideAs P (Stride 2) (Ix1 0 ... 10)
-- Array P Seq (Sz1 6)
--   [ 0, 2, 4, 6, 8, 10 ]


-- | Stride is similar to Sz, just a special index

--
-- >>> :i Stride

-- >>> Stride (3 :> 0 :. -2)
-- Stride (3 :> 1 :. 1)


