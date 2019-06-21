{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( someFunc
    ) where

import Data.Massiv.Array as A

someFunc :: IO ()
someFunc = putStrLn "someFunc"

dropColumnMaybe ::
  Ix1
  -> Array D Ix2 a
  -> Maybe (Array D Ix1 a, Array D Ix2 a, Array D Ix2 a)
dropColumnMaybe i arr = do
  column <- arr <!? i
  let Sz2 m n = size arr
  left <- extractM (0 :. 0) (Sz2 m i) arr
  right <- extractM (0 :. i + 1) (Sz2 m (n-i-1)) arr
  pure (column, left, right)

dropColumnMaybe' ::
     Ix1 -> Array D Ix2 a -> Maybe (Array D Ix1 a, Array DL Ix2 a)
dropColumnMaybe' i arr = do
  column <- arr <!? i
  let Sz2 m n = size arr
  left <- extractM (0 :. 0) (Sz2 m i) arr
  right <- extractM (0 :. i + 1) (Sz2 m (n-i-1)) arr
  popped <- appendM 1 left right
  pure (column, popped)

forceDropped (column, popped) =
  (computeAs U (resize' (Sz (n :. 1)) column), computeAs U popped)
  where n = unSz $ size column
