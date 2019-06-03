{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MassivTalk.Day1.Intro08_Random where

import Prelude as P
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe
import Control.Scheduler
import Control.Monad (void)
import Control.Monad.ST (ST, runST)

import System.Random as System
import System.Random.MWC as MWC
import System.Random.SplitMix as SplitMix
import Data.Random as Fu
import Data.RVar as RVar
import Data.Random.Source

randomArrayDL :: Index ix => g -> (g -> (e, g)) -> Sz ix -> Array DL ix e
randomArrayDL gen nextRandom sz = unfoldrS_ Seq sz nextRandom gen
{-# INLINE randomArrayDL #-}

-- >>> gen = System.mkStdGen 217
-- >>> randomArrayDL gen System.random (Sz2 2 3) :: Array DL Ix2 Double


randomArrayS ::
     Mutable r ix e => g -> (g -> (e, g)) -> Sz ix -> (g, Array r ix e)
randomArrayS gen nextRandom sz = runST $ unfoldrPrimM Seq sz (pure . nextRandom) gen
{-# INLINE randomArrayS #-}

-- >>> gen = System.mkStdGen 217
-- >>> snd $ randomArrayS gen System.random (Sz2 2 3) :: Array P Ix2 Double




-- >>> gen <- System.newStdGen
-- >>> randomArray gen System.split System.random Seq (Sz2 2 3) :: Array DL Ix2 Double


-- >>> gen <- MWC.createSystemRandom
-- >>> generateArray Par (Sz2 2 3) (const (MWC.uniform gen)) :: IO (Array P Ix2 Double)

randomArrayST
  :: Mutable r ix e =>
     (forall s . ST s (ST s e)) -> Sz ix -> Array r ix e
randomArrayST mkRandomGenerator sz = runST $ do
  mkRandom <- mkRandomGenerator
  generateArrayS Seq sz (const mkRandom)
{-# INLINE randomArrayST #-}


-- >>> randomArrayST (MWC.uniform <$> MWC.create) (Sz2 2 3) :: Array P Ix2 Double

-- >>> gen <- MWC.createSystemRandom
-- >>> seed <- MWC.save gen
-- >>> randomArrayST (MWC.uniform <$> MWC.restore seed) (Sz2 2 3) :: Array P Ix2 Double


-- | Parallelizable generation of arrays with random elements.
randomArrayIO
  :: (MonadUnliftIO m, PrimMonad m, Mutable r ix e) =>
    (Ix1 -> m g) -> (g -> m e) -> Comp -> Sz ix -> m ([g], Array r ix e)
randomArrayIO mkGen mkRandom comp sz =
  createArray comp sz $ \scheduler marr ->
    splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
      let writeRandom gen k = do
            e <- mkRandom gen
            unsafeLinearWrite marr k e
      loopM_ 0 (< slackStart) (+ chunkLength) $ \start ->
        scheduleWork scheduler $ do
          gen <- mkGen (start `div` chunkLength)
          void $ loopM_ start (< (start + chunkLength)) (+ 1) (writeRandom gen)
          pure gen
      scheduleWork scheduler $ do
        gen <- mkGen (totalLength `div` chunkLength)
        loopM_ slackStart (< totalLength) (+ 1) (writeRandom gen)
        pure gen
      pure []
  where
    totalLength = totalElem sz
{-# INLINE randomArrayIO #-}




initGensMWC :: Comp -> IO (Array B Ix1 GenIO)
initGensMWC comp = do
  n <- getCompWorkers comp
  (gs, _ :: Array P Ix1 Int) <-
    randomArrayIO (const MWC.createSystemRandom) MWC.uniform comp (Sz1 n)
  pure $ A.fromList Seq gs


---------------------------------
-- Benchmark Random Generators --
---------------------------------

-- `random` package

systemRandomS ::
     (Index ix, RandomGen g) => g -> Sz ix -> Array P ix Double
systemRandomS gen = compute . randomArrayDL gen System.random
{-# INLINE systemRandomS #-}

systemRandomP ::
     (Index ix, RandomGen g) => g -> Sz ix -> Array P ix Double
systemRandomP gen = compute . randomArray gen System.split System.random Par
{-# INLINE systemRandomP #-}


-- `split-mix` package

splitMixS :: Index ix => SMGen -> Sz ix -> Array P ix Double
splitMixS gen = compute . randomArrayDL gen SplitMix.nextDouble
{-# INLINE splitMixS #-}

splitMixP :: Index ix => SMGen -> Sz ix -> Array P ix Double
splitMixP gen =
  compute . randomArray gen SplitMix.splitSMGen SplitMix.nextDouble Par
{-# INLINE splitMixP #-}


-- `random-mwc` package
--
-- /Note/ - It uses mutable state so same generator shouldn't be used from the different
-- threads simultaneously.

mwcS :: Index ix => Seed -> Sz ix -> Array P ix Double
mwcS seed = randomArrayST (MWC.uniform <$> MWC.restore seed)
{-# INLINE mwcS #-}


mwcP :: Index ix => (Int -> IO MWC.GenIO) -> Sz ix -> IO (Array P ix Double)
mwcP mkGen = fmap snd . randomArrayIO mkGen MWC.uniform Par
{-# INLINE mwcP #-}


-- | Parallelizable generation of arrays with random elements.
randomDistribution ::
     ( RandomSource m s
     , MonadUnliftIO m
     , PrimMonad m
     , Distribution d e
     , Mutable r ix e
     )
  => d e
  -> s
  -> Comp
  -> Sz ix
  -> m (Array r ix e)
randomDistribution dist s comp sz =
  createArray_ comp sz $ \scheduler marr ->
    splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
      let writeRandom var k = do
            e <- RVar.runRVarT var s
            unsafeLinearWrite marr k e
      loopM_ 0 (< slackStart) (+ chunkLength) $ \start ->
        scheduleWork scheduler $ do
          let gen = rvarT dist
          void $ loopM_ start (< (start + chunkLength)) (+ 1) (writeRandom gen)
      scheduleWork scheduler $ do
        let gen = rvarT dist
        loopM_ slackStart (< totalLength) (+ 1) (writeRandom gen)
  where
    totalLength = totalElem sz
{-# INLINE randomDistribution #-}


data SchedulerState s = SchedulerState
  { schedulerComp :: !Comp
  , schedulerState :: !(Array B Ix1 s)
  }

initSchedulerState :: MonadIO m => Comp -> (Int -> m s) -> m (SchedulerState s)
initSchedulerState comp initState = do
  nWorkers <- getCompWorkers comp
  stateArr <- A.forM (0 ..: nWorkers) initState
  pure SchedulerState {schedulerComp = comp, schedulerState = stateArr}

withStatefulScheduler ::
     (MonadUnliftIO m)
  => SchedulerState s
  -> (Scheduler m a -> ((s -> m a) -> m ()) -> m b)
     -- ^ Action that will be scheduling all the work.
  -> m [a]
withStatefulScheduler state action =
  withScheduler (schedulerComp state) $ \scheduler -> do
    let scheduleWithState withState =
          scheduleWorkId scheduler $ \i ->
            withState (index' (schedulerState state) i)
    action scheduler scheduleWithState

withStatefulScheduler_ ::
     MonadUnliftIO m
  => SchedulerState s
  -> (Scheduler m () -> ((s -> m ()) -> m ()) -> m b)
     -- ^ Action that will be scheduling all the work.
  -> m ()
withStatefulScheduler_ state action =
  withScheduler_ (schedulerComp state) $ \scheduler -> do
    let scheduleWithState withState =
          scheduleWorkId_ scheduler $ \i ->
            withState (index' (schedulerState state) i)
    action scheduler scheduleWithState
{-# INLINE withStatefulScheduler_ #-}


splitLinearlyWithStatefulM_ ::
     Monad m
  => Int
  -> ((s -> m ()) -> m ())
  -> Int
  -> (s -> Int -> m b)
  -> (s -> Int -> b -> m c)
  -> m ()
splitLinearlyWithStatefulM_ nWorkers schedule totalLength make store =
  splitLinearly nWorkers totalLength $ \chunkLength slackStart -> do
    loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
      schedule $ \ s ->
        loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k -> make s k >>= store s k
    schedule $ \ s ->
      loopM_ slackStart (< totalLength) (+ 1) $ \ !k -> make s k >>= store s k
{-# INLINE splitLinearlyWithStatefulM_ #-}


randomArrayRVarT ::
     (Mutable r ix e, MonadUnliftIO m, PrimMonad m, RandomSource m s)
  => SchedulerState s
  -> RVarT m e
  -> Sz ix
  -> m (Array r ix e)
randomArrayRVarT state var sz = do
  marr <- unsafeNew sz
  withStatefulScheduler_ state $ \scheduler scheduleWithState ->
    splitLinearlyWithStatefulM_
      (numWorkers scheduler)
      scheduleWithState
      (totalElem sz)
      (\s _ -> RVar.runRVarT var s)
      (\_s i -> unsafeLinearWrite marr i)
  unsafeFreeze (schedulerComp state) marr
{-# INLINE randomArrayRVarT #-}


mapStateIO ::
     (Source r' ix e, Mutable r ix b, MonadUnliftIO m, PrimMonad m)
  => SchedulerState s
  -> (s -> e -> m b)
  -> Array r' ix e
  -> m (Array r ix b)
mapStateIO state f arr = do
  marr <- unsafeNew (size arr)
  withStatefulScheduler_ state $ \scheduler scheduleWithState ->
    splitLinearlyWithStatefulM_
      (numWorkers scheduler)
      scheduleWithState
      (totalElem (size arr))
      (\s i -> f s (unsafeLinearIndex arr i))
      (\_s i -> unsafeLinearWrite marr i)
  unsafeFreeze (getComp arr) marr
{-# INLINE mapStateIO #-}


runArrayRVarT ::
     ( Source r' ix (RVarT m e)
     , Mutable r ix e
     , MonadUnliftIO m
     , PrimMonad m
     , RandomSource m s
     )
  => SchedulerState s
  -> Array r' ix (RVarT m e)
  -> m (Array r ix e)
runArrayRVarT state = mapStateIO state (flip RVar.runRVarT)
{-# INLINE runArrayRVarT #-}
