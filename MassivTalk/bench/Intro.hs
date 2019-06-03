{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.IORef
import Criterion.Main
import Data.Massiv.Array
import MassivTalk.Day1.Intro01_Representations
import MassivTalk.Day1.Intro08_Random
import System.Random as System
import System.Random.MWC as MWC
import System.Random.SplitMix as SplitMix
import System.Random.Mersenne.Pure64 as MT
import Control.Scheduler
import Data.Random as Fu
import Data.Random.Source.DevRandom

main :: IO ()
main = do
  let !k = 500000
      !n = 5000
      !sz = Sz1 100000
  sysGen <- getStdGen
  mwcGen <- MWC.create
  mwcSeed <- MWC.save mwcGen
  gensArr <- initGensMWC Par
  mtGen <- newPureMT
  let r = rvarT StdUniform :: RVarT m Double
      runArrayRVarT' ::
           RandomSource IO s => SchedulerState s -> IO (Array P Ix1 Double)
      runArrayRVarT' state = randomArrayRVarT state r sz
  mtState <- initSchedulerState Par (\_ -> newPureMT >>= newIORef)
  mtStateSeq <- initSchedulerState Seq (\_ -> newPureMT >>= newIORef)
  mwcState <- initSchedulerState Par (\_ -> MWC.createSystemRandom)
  mwcStateSeq <- initSchedulerState Seq (\_ -> MWC.createSystemRandom)
  defaultMain
    [ bgroup
        "Intro01"
        [ bgroup
            "Double"
            [ bench "primitive" $ whnf primitive k
            , bench "unboxed" $ whnf unboxed k
            , bench "storable" $ whnf storable k
            , bench "boxedWHNF" $ whnf boxedWHNF k
            , bench "boxedNF" $ whnf boxedNF k
            ]
        , bgroup
            "Maybe"
            [ bench "B - whnf" $ whnf boxedMaybeWHNF n
            , bench "N - whnf" $ whnf boxedMaybeNF n
            ]
        ]
    , bgroup
        "Intro08"
        [ bgroup
            "Seq"
            [ bench "random" $ whnf (systemRandomS sysGen) sz
            , env newSMGen $ \gen -> bench "splitmix" $ whnf (splitMixS gen) sz
            , bench "random-mwc" $ whnf (mwcS mwcSeed) sz
            , bench "random-fu (mwc)" $
              whnfIO
                (randomDistribution StdUniform mwcGen Seq sz :: IO (Array P Ix1 Double))
            , bench "random-fu (/dev/urandom)" $
              whnfIO
                (randomDistribution StdUniform DevURandom Seq sz :: IO (Array P Ix1 Double))
            ]
        , bgroup
            "Par"
            [ bench "random" $ whnf (systemRandomP sysGen) sz
            , env newSMGen $ \gen -> bench "splitmix" $ whnf (splitMixP gen) sz
            , bench "random-mwc" $ nfIO (mwcP (indexM gensArr) sz)
            ]
        , bgroup
            "Fu"
            [ bench "mersenne-random-pure64 Fu Par" $ whnfIO (runArrayRVarT' mtState)
            , bench "mersenne-random-pure64 Fu Seq" $ whnfIO (runArrayRVarT' mtStateSeq)
            , bench "mersenne-random-pure64 Seq" $
              whnf
                (snd . randomArrayS mtGen MT.randomDouble :: Sz Ix1 -> Array P Ix1 Double)
                sz
            , bench "mwc Fu Par" $ whnfIO (runArrayRVarT' mwcState)
            , bench "mwc Fu Seq" $ whnfIO (runArrayRVarT' mwcStateSeq)
            , bench "random-mwc" $ whnf (mwcS mwcSeed) sz
            ]
        ]
    ]
