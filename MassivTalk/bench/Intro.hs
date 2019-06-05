{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

--import Control.Monad as M (replicateM)
import Control.Scheduler
import Criterion.Main
--import Data.IORef
import Data.Massiv.Array
--import Data.Random as Fu
--import Data.Random.Source.DevRandom
import MassivTalk.Day1.Intro01_Representations
import MassivTalk.Day1.Intro08_Random
import System.Random as System
import System.Random.Mersenne.Pure64 as MT
import System.Random.MWC as MWC
import System.Random.PCG.Pure as PCG
import System.Random.SplitMix as SplitMix
import System.Random.TF as TF

--randomArrayPureSeq

main :: IO ()
main = do
  let !k = 500000
      !n = 5000
      !sz = Sz1 10485760
  sysGen <- System.getStdGen
  smGen <- SplitMix.newSMGen
  tfGen <- TF.newTFGen
  mwcGen <- MWC.create
  mwcSeed <- MWC.save mwcGen
  pcgGen <- PCG.create
  pcgSeed <- PCG.save pcgGen
  mtGen <- newPureMT
  -- let r = rvarT StdUniform :: RVarT m Double
  --     runArrayRVarT' ::
  --          RandomSource IO s => WorkerStates s -> IO (Array P Ix1 Double)
  --     runArrayRVarT' state = randomArrayRVarT state sz r
  -- mtState <- initWorkerStates Par (\_ -> newPureMT >>= newIORef)
  -- mtStateSeq <- initWorkerStates Seq (\_ -> newPureMT >>= newIORef)
  mwcState <- initWorkerStates Par (\_ -> MWC.createSystemRandom)
  mwcStateSeq <- initWorkerStates Seq (\_ -> MWC.createSystemRandom)
  pcgState <- initWorkerStates Par (\(WorkerId w) -> PCG.initialize (fromIntegral w) 5)
  pcgStateSeq <- initWorkerStates Seq (\_ -> PCG.createSystemRandom)
  defaultMain
    [ bgroup
        "Intro01" -- Representations
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
        "Intro08" -- Random
        [ bgroup
            "Pure"
            [ bgroup
                "Seq"
                [ bench "random" $
                  whnf
                    (computeAs P . randomArrayPureSeq sysGen System.random)
                    sz
                , bench "tf-random" $
                  whnf (computeAs P . randomArrayPureSeq tfGen System.random) sz
                , bench "pcg-random" $
                  whnf
                    (computeAs P . randomArrayPureSeq pcgSeed System.random)
                    sz
                , bench "mersenne-random-pure64" $
                  whnf
                    (computeAs P . randomArrayPureSeq mtGen MT.randomDouble)
                    sz
                , bench "splitmix" $
                  whnf
                    (computeAs P . randomArrayPureSeq smGen SplitMix.nextDouble)
                    sz
                ]
            , bgroup
                "Par"
                [ bench "random" $
                  whnf
                    (computeAs P .
                     randomArrayPurePar sysGen System.split System.random)
                    sz
                , bench "tf-random" $
                  whnf
                    (computeAs P .
                     randomArrayPurePar tfGen System.split System.random)
                    sz
                , bench "pcg-random" $
                  whnf
                    (computeAs P .
                     randomArrayPurePar pcgSeed System.split System.random)
                    sz
                , bench "splitmix" $
                  whnf
                    (computeAs P .
                     randomArrayPurePar
                       smGen
                       SplitMix.splitSMGen
                       SplitMix.nextDouble)
                    sz
                ]
            ]
        , bgroup
            "ST"
            [ bgroup
                "Seq"
                  -- bench "random" $
                --   whnf (snd . randomArrayPureST sysGen System.random) sz
                -- ,
                  -- bench "tf-random" $
                --   whnf (snd . randomArrayPureST tfGen System.random) sz
                -- ,
                [ bench "splitmix" $
                  whnf (snd . randomArrayPureST smGen SplitMix.nextDouble) sz
                , bench "mersenne-random-pure64" $
                  whnf (snd . randomArrayPureST mtGen MT.randomDouble) sz
                , bench "mwc-random" $
                  whnf (snd . randomArrayMWC mwcSeed MWC.uniform) sz
                , bench "pcg-random" $
                  whnf (snd . randomArrayPCG pcgSeed PCG.uniform) sz
                ]
            ]
        , bgroup
            "IO"
            [ bgroup
                "Seq"
                [ bench "mwc-random" $
                  nfIO (randomArrayIO mwcStateSeq sz MWC.uniform)
                , bench "pcg-random" $
                  nfIO (randomArrayIO pcgStateSeq sz PCG.uniform)
                ]
            , bgroup
                "Par"
                [ bench "mwc-random" $
                  nfIO (randomArrayIO mwcState sz MWC.uniform)
                , bench "pcg-random" $
                  nfIO (randomArrayIO pcgState sz PCG.uniform)
                ]
            ]
        ]
    ]
        --     "Seq"
        --     [ bench "random" $ whnf (systemRandomS sysGen) sz
        --     , env newSMGen $ \gen -> bench "splitmix" $ whnf (splitMixS gen) sz
        --     , bench "mwc-random" $ whnf (mwcS mwcSeed) sz
        --     , bench "mwc-random (List)" $
        --       nfIO (M.replicateM (unSz sz) (MWC.uniform mwcGen) :: IO [Double])
        --     , bench "mwc-random Seq" $ nfIO (mwc mwcStateSeq sz)
        --     , bench "random-fu (mwc)" $
        --       whnfIO
        --         (randomDistribution StdUniform mwcGen Seq sz :: IO (Array P Ix1 Double))
        --     , bench "random-fu (/dev/urandom)" $
        --       whnfIO
        --         (randomDistribution StdUniform DevURandom Seq sz :: IO (Array P Ix1 Double))
        --     ]
        -- , bgroup
        --     "Par"
        --     [ bench "random" $ whnf (systemRandomP sysGen) sz
        --     , env newSMGen $ \gen -> bench "splitmix" $ whnf (splitMixP gen) sz
        --     , bench "mwc-random" $ nfIO (mwc mwcState sz)
        --     ]
        -- , bgroup
        --     "Fu"
        --     [ bench "mersenne-random-pure64 Fu Par" $
        --       whnfIO (runArrayRVarT' mtState)
        --     , bench "mersenne-random-pure64 Fu Seq" $
        --       whnfIO (runArrayRVarT' mtStateSeq)
        --     -- , bench "mersenne-random-pure64 Seq" $
        --     --   whnf
        --     --     (\s -> snd . randomArrayS mtGen s MT.randomDouble :: Sz Ix1 -> Array P Ix1 Double)
        --     --     sz
        --     , bench "mwc Fu Par" $ whnfIO (runArrayRVarT' mwcState)
        --     , bench "mwc Fu Seq" $ whnfIO (runArrayRVarT' mwcStateSeq)
        --     , bench "mwc-random" $ whnf (mwcS mwcSeed) sz
        --     ]
