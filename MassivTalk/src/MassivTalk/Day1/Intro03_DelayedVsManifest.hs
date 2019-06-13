{-# LANGUAGE FlexibleContexts #-}
module MassivTalk.Day1.Intro03_DelayedVsManifest where

import Prelude as P
import Data.Massiv.Array as A

-- | Fuse computation

-- >>> f x = x * x
-- >>> arr = makeArrayR D Seq (Sz2 5 5) $ \ (i :. j) -> f (i - 2) + f (j - 2)
-- >>> A.sum $ A.map (+10) arr


-- | Intermediate large size arrays

ridiculousSquare :: Int -> Array D Ix2 Double
ridiculousSquare k =
  makeArray Seq (Sz2 k k) $ \(i :. j) -> fromIntegral i ** sin (fromIntegral j)


-- >>> arr = ridiculousSquare maxBound
-- >>> size arr
-- >>> evaluateM arr (123456765432134 :. 2345677654345678)


-- | Loading of such array into memory in full is infeasible

-- >>> stride = Stride (maxBound `div` 3 :. maxBound `div` 2)
-- >>> print stride
-- >>> computeWithStrideAs U stride $ ridiculousSquare maxBound


-- | Indices that are too large are dangerous
-- | As long as the total number of elements is below `maxBound :: Int`, we are ok.

-- >>> arr = ridiculousSquare maxBound
-- >>> extractM 0 (Sz2 5 6) arr


-- | Identity


identityD :: Int -> Array D Ix2 Int
identityD n =
  makeArray Seq (Sz2 n n) $ \(i :. j) ->
    if i == j
      then 1
      else 0


-- >>> :t makeLoadArrayS

identityDL :: Int -> Array DL Ix2 Int
identityDL n = makeLoadArrayS (Sz2 n n) 0 $ \ w -> do
  let f i = w (i :. i) 1
  A.mapM_ f (0 ... n - 1)
  -- Same as:
  -- P.mapM_ f [0 .. n - 1]



