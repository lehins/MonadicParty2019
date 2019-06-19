module MassivTalk.Day1.Intro01_Representations where

import Prelude as P
import Data.Massiv.Array as A


-- |
-- data family Array r ix e :: *
--
-- >>> :t makeArray
-- makeArray
--   :: Construct r ix e => Comp -> Sz ix -> (ix -> e) -> Array r ix e

-- | Array with all even integers up to the supplied `n`
evens :: Int -> Array P Ix1 Int
evens n = makeArray Seq (Sz (n `div` 2)) $ \ i -> i * 2

--
-- >>> evens 10
-- Array P Seq (Sz1 5)
--   [ 0, 2, 4, 6, 8 ]

areEvens :: Int -> Array S Ix1 Bool
areEvens n = makeArray Seq (Sz n) even

--
-- >>> areEvens 10
-- Array S Seq (Sz1 10)
--   [ True, False, True, False, True, False, True, False, True, False ]

-- | Array with all integers up to supplied `n` and indicator if it is even or
-- not
evensAre :: Int -> Array U Ix1 (Int, Bool)
evensAre n = makeArray Seq (Sz n) $ \ i -> (i, even i)

--
-- >>> evensAre 6

-- | Array with all even integers up to supplied `n` as Just and odds are Nothing
evensMaybe :: Int -> Array B Ix1 (Maybe Int)
evensMaybe n =
  makeArray Seq (Sz n) $ \i ->
    if even i
      then Just i
      else Nothing

--
-- >>> evensMaybe 6
-- Array B Seq (Sz1 6)
--   [ Just 0, Nothing, Just 2, Nothing, Just 4, Nothing ]

---------------------------
-- Strictness properties --
---------------------------

--
-- >>> [undefined, 'c', error "Whoops"] !! 1
-- 'c'


badEvens :: Array U Ix1 (Int, Bool)
badEvens = makeArray Seq 10 $ \ i -> (i, even i && undefined)

--
-- >>> fst (badEvens ! 0)

--
-- >>> arr = fromList Seq [undefined, 'c', error "Whoops"] :: Array B Ix1 Char
-- >>> arr ! 1
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:78:14 in base:GHC.Err
--   undefined, called at <interactive>:20:22 in interactive:Ghci1

--
-- >>> arr = fromList Seq ['a', 'c', error "Whoops"] :: Array B Ix1 Char
-- >>> arr ! 1
-- *** Exception: Whoops
-- CallStack (from HasCallStack):
--   error, called at <interactive>:24:32 in interactive:Ghci1

--
-- >>> arr = fromList Seq [Just undefined, Just 'c', Nothing] :: Array B Ix1 (Maybe Char)
-- >>> arr ! 1
-- Just 'c'

--
-- >>> arr = fromList Seq ["foo" ++ undefined, "bar", ""] :: Array B Ix1 String
-- >>> arr ! 1
-- "bar"

--
-- >>> arr = fromList Seq ["foo" ++ undefined, "bar", ""] :: Array N Ix1 String
-- >>> arr ! 1


--------------------------------
-- Performance considerations --
--------------------------------

parabola :: Int -> Double
parabola x = fromIntegral x ^ (2 :: Int)


boxedWHNF :: Int -> Array B Ix1 Double
boxedWHNF n = makeArray Seq (Sz n) parabola

boxedNF :: Int -> Array N Ix1 Double
boxedNF n = makeArray Seq (Sz n) parabola

primitive :: Int -> Array P Ix1 Double
primitive n = makeArray Seq (Sz n) parabola

unboxed :: Int -> Array U Ix1 Double
unboxed n = makeArray Seq (Sz n) parabola

storable :: Int -> Array S Ix1 Double
storable n = makeArray Seq (Sz n) parabola

-- :! stack bench :intro --ba '--match prefix Intro01/Double'

-- An expensive function
chaos :: Int -> Int -> Double
chaos nMax x0 = go nMax
  where
    go n
      | n > 0 =
        let xn = go (n - 1)
         in xn `seq` (0.000000001 * xn * (1 - xn))
      | otherwise = fromIntegral x0


boxedMaybeWHNF :: Int -> Array B Ix1 (Maybe Double)
boxedMaybeWHNF n = makeArray Seq (Sz n) (Just . chaos n)

boxedMaybeNF :: Int -> Array N Ix1 (Maybe Double)
boxedMaybeNF n = makeArray Seq (Sz n) (Just . chaos n)

boxedMaybeParNF :: Int -> Array N Ix1 (Maybe Double)
boxedMaybeParNF n = makeArray Par (Sz n) (Just . chaos n)


-- :! stack bench :intro --ba '--match prefix Intro01/Maybe'

