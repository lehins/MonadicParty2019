{-# LANGUAGE FlexibleContexts #-}
module Main where

import Criterion.Main
import MassivTalk.Integral
import Prelude as P

gaussian1 :: Floating a => a -> a -> a
gaussian1 stdDev x = exp (- (x ^ (2 :: Int)) / var2) / sqrt (var2 * pi)
  where
    var2 = 2 * stdDev ^ (2 :: Int)

main :: IO ()
main = do
  let g = gaussian1 2
      b = 1000
      k = 100000
  defaultMain
    [ bgroup
        "Naive"
        [ bench "list" $ whnf (integrate k g 0) b
        , bench "trapezoidRule" $ whnf (integrateTrapezoid k g 0) b
        , bench "array Seq" $ whnf (integrateNaive k g 0) b
        , bench "array Par" $ whnf (integrateNaivePar k g 0) b
        ]
    , bgroup
        "NoDuplicate"
        [ bench "Seq Bad" $ whnf (integrateNoDuplicateBad k g 0) b
        , bench "Seq" $ whnf (integrateNoDuplicate k g 0) b
        , bench "Par" $ whnf (integrateNoDuplicatePar k g 0) b
        , bench "List" $ whnf (integrateNoDuplicateList k g 0) b
        ]
    , bgroup
        "NoAllocate"
        [ bench "Seq" $ whnf (integrateNoAllocate k g 0) b
        , bench "Par 8" $ whnf (integrateNoAllocateN8 k g 0) b
        ]
    ]
