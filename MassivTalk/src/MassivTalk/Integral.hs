module MassivTalk.Integral
  ( integrate
  , integrateTrapezoid
  , integrateNaive
  , integrateNaivePar
  , integrateNoDuplicateBad
  , integrateNoDuplicate
  , integrateNoDuplicatePar
  , integrateNoDuplicateList
  , integrateNoAllocate
  , integrateNoAllocateN8
  ) where

import Control.Applicative
import Data.Massiv.Array as A
import Data.Massiv.Array.Numeric.Integral
import Prelude as P

-- StackOverflow question:
-- https://stackoverflow.com/questions/56332713/how-do-i-add-parallel-computation-to-this-example

-- Sanity check:
-- >>> integrate 1 id 0 10

integrate :: Int -> (Double -> Double) -> Double -> Double -> Double
integrate n f a b =
  let step     = (b - a) / fromIntegral n
      segments = [a + fromIntegral x * step | x <- [0 .. n-1]]
      area x   = step * (f x + f (x + step)) / 2
  in P.sum $ P.map area segments


-- Something more complicated, how about a parabola
-- >>> integrate 1000 (\x -> x * x) 10 20


-- >>> let f x = x ** 3 / 3 :: Double
-- >>> f 20 - f 10


-- | Built-in solution:


integrateTrapezoid :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateTrapezoid n f a b =
  trapezoidRule Seq P (\scale x -> f (scale x)) a (b - a) (Sz1 1) n ! 0

-- >>> integrateTrapezoid 1000 (\x -> x * x) 10 20



-- Normal use cases is a lot more complex:
-- >>> let f x y = exp (- (x ** 2 + y ** 2)) :: Float
-- >>> trapezoidRule Seq P (\scale (i :. j) -> f (scale i) (scale j)) (-2) 1 (Sz2 4 4) 100




-- | Direct naive translation into massiv arrays

-- range sugar:
-- >>> Ix1 0 ... 10

-- Which is a synonym for
-- >>> rangeInclusive Seq (Ix1 0) 10

-- range exclusive or simply `range`:
-- >>> Ix1 0 ..: 10

-- Works for all dimensions
-- >>>  0 :. 1 ... 5


integrateNaive :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNaive n f a b =
  let step     = (b - a) / fromIntegral n
      segments = fmap (\x -> a + fromIntegral x * step) (0 ..: n)
      area x   = step * (f x + f (x + step)) / 2
  in P.sum $ fmap area segments

-- >>> integrateNaive 1000 (\x -> x * x) 10 20



-- | What is needed for parallelization:

integrateNaivePar :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNaivePar n f a b =
  let step     = (b - a) / fromIntegral n
      segments = A.map (\x -> a + fromIntegral x * step) (range Par 0 n)
      area x   = step * (f x + f (x + step)) / 2
  in A.sum $ A.map area segments


-- >>> integrateNaivePar 1000 (\x -> x * x) 10 20

-- Checkout benchmarks:
-- :! stack bench :integral --ba '--match prefix Naive'


-- Simplify a bit first
-- >>> A.map (\x -> 10 + fromIntegral x * 0.1) (range Seq 0 5) :: Array D Ix1 Double

-- >>> enumFromStepN Seq 10 0.1 5 :: Array D Ix1 Double


-- | First optimization. Avoid duplicate calls to `f`

-- For simplicity we'll use n = 10

-- >>> n = 10 :: Int
-- >>> (a, b) = (10, 20) :: (Double, Double)
-- >>> step = (b - a) / fromIntegral n
-- >>> A.map (\x -> a + fromIntegral x * step) (range Par 0 n) :: Array D Ix1 Double
-- >>> enumFromStepN Seq a step (Sz n + 1) :: Array D Ix1 Double

integrateNoDuplicateBad :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoDuplicateBad n f a b =
  let step = (b - a) / fromIntegral n
      sz = size segments - 1
      -- this is still a delayed array
      segments = fmap f (enumFromStepN Seq a step (Sz (n + 1)))
      area y0 y1 = step * (y0 + y1) / 2
      areas = liftA2 area (extract' 0 sz segments) (extract' 1 sz segments)
   in P.sum areas

-- integrateNoDuplicate :: Int -> (Double -> Double) -> Double -> Double -> Double
-- integrateNoDuplicate n f a b = undefined -- fix the above

integrateNoDuplicate :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoDuplicate n f a b =
  let step = (b - a) / fromIntegral n
      sz = size segments - 1
      -- this is still a delayed array
      segments = computeAs P $ fmap f (enumFromStepN Seq a step (Sz (n + 1)))
      area y0 y1 = step * (y0 + y1) / 2
      areas = A.zipWith area (extract' 0 sz segments) (extract' 1 sz segments)
   in A.sum areas

-- >>> integrateNoDuplicate 1000 (\x -> x * x) 10 20

-- check the benchmarks:
-- :! stack bench :integral --ba '--match prefix NoDuplicate/Seq'





-- Fixed and parallelized:


integrateNoDuplicatePar :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoDuplicatePar n f a b =
  let step = (b - a) / fromIntegral n
      sz = size segments - 1
      -- this is still a delayed array
      segments = computeAs P $ fmap f (enumFromStepN Par a step (Sz (n + 1)))
      area y0 y1 = step * (y0 + y1) / 2
      areas = A.zipWith area (extract' 0 sz segments) (extract' 1 sz segments)
   in A.sum areas



integrateNoDuplicateList :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoDuplicateList n f a b =
  let step       = (b - a) / fromIntegral n
      ys         = [f (a + fromIntegral x * step) | x <- [0 .. n]]
      area y0 y1 = step * (y0 + y1) / 2
  in P.sum $ P.zipWith area ys (tail ys)

-- >>> integrateNoDuplicateList 1000 (\x -> x * x) 10 20


-- :! stack bench :integral --ba '--match prefix NoDuplicate'


-- Moral:
-- * List fusion is easily broken. Which results in allocations and slowdowns
-- * Parallelization is easy
-- * Important to identify duplication of work. Easy solution is to `compute`.


-- | Can we avoid allocation completely? Yes we can!


integrateNoAllocate :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoAllocate n f a b =
  let step = (b - a) / fromIntegral n
      segments = A.map f (enumFromStepN Seq (a + step) step (Sz n))
      area y0 y1 = step * (y0 + y1) / 2
      sumWith (acc, y0) y1 =
        let acc' = acc + area y0 y1
         in acc' `seq` (acc', y1) -- folds are strict, but only to WHNF
   in fst $ A.foldlS sumWith (0, f a) segments


-- :! stack bench :integral --ba '--match prefix No'
-- Waaat?


-- Can we do even better?
-- Not too straighforward, but there is a bit of uglyness we can come up with.

-- | Parallelization for 8 capabilities (quad core with hyperthreading). If `n` is not
-- divisble by 8, this function will return bogus.
integrateNoAllocateN8 :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoAllocateN8 n f a b =
  let k = 8
      n' = n `div` k
      step = (b - a) / fromIntegral n
      segments =
        makeArrayR D (ParN (fromIntegral k)) (Sz1 k) $ \i ->
          let start = a + step * fromIntegral n' * fromIntegral i + step
           in (f start, A.map f (enumFromStepN Seq (start + step) step (Sz (n' - 1))))
      area y0 y1 = step * (y0 + y1) / 2
      sumWith (acc, y0) y1 =
        let acc' = acc + area y0 y1
         in acc' `seq` (acc', y1)
      partialResults =
        computeAs U $ A.map (\(y0, arr) -> (y0, A.foldlS sumWith (0, y0) arr)) segments
      combine (acc, y0) (y1, (acci, yn)) =
        let acc' = acc + acci + area y0 y1
         in acc' `seq` (acc', yn)
   in fst $ foldlS combine (0, f a) partialResults

-- >>> f x = x * x :: Double
-- >>> k = 4 :: Int
-- >>> n = 16 :: Int
-- >>> n' = n `div` k
-- >>> (a, b) = (10, 20) :: (Double, Double)
-- >>> step = (b - a) / fromIntegral n
-- >>> mkStart i = a + step * fromIntegral n' * fromIntegral i + step
-- >>> mkArr start = A.map f (enumFromStepN Seq (start + step) step (Sz (n' - 1)))
-- >>> mkSubArr i = let start = mkStart i in (f start, mkArr start)
-- >>> comp = ParN (fromIntegral k)
-- >>> segments = makeArrayR D comp (Sz1 k) mkSubArr
-- >>> A.mapM_ print segments
-- >>> A.map f (enumFromStepN Seq a step (Sz n + 1))




-- List memory footprint
-- [1,2,3] :: Int

-- P (P 1) : P ( P (P 2 ) : P (P (P 3) : P _))
-- (I# 1#) : ((I# 2#) : ((I# 3#) : []))
