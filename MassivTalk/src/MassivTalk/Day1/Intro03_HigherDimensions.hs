{-# LANGUAGE FlexibleContexts #-}
module MassivTalk.Day1.Intro03_HigherDimensions where

import Prelude as P
import Data.Massiv.Array as A

identityD :: Int -> Array D Ix2 Int
identityD n =
  makeArray Seq (Sz2 n n) $ \(i :. j) ->
    if i == j
      then 1 :: Int
      else 0


identityDL :: Int -> Array DL Ix2 Int
identityDL n = makeLoadArrayS (Sz2 n n) 0 $ \ w -> do
  let f i = w (i :. i) 1
  --P.mapM_ f [0 .. n - 1]
  A.mapM_ f (0 ... n - 1)


-- >>> f x = x * x
-- >>> makeArrayR D Seq (Sz2 5 5) $ \ (i :. j) -> f (i - 2) + f (j - 2)


