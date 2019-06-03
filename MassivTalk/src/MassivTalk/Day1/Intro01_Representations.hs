module MassivTalk.Day1.Intro01_Representations where

import Prelude as P
import Data.Massiv.Array as A

-- Array with all even integers up to the supplied `n`
evens :: Int -> Array P Ix1 Int
evens n = makeArray Seq (Sz (n `div` 2)) $ \ i -> i * 2


-- λ> evens 10
-- Array P Seq (Sz1 5)
--   [ 0, 2, 4, 6, 8 ]
-- λ> es = evens 10
-- λ> es ! 0
-- 0
-- λ> es ! 3
-- 6
-- λ> es ! 8
-- *** Exception: IndexOutOfBoundsException: 8 not safe for (Sz1 5)

areEvens :: Int -> Array S Ix1 Bool
areEvens n = makeArray Seq (Sz n) even

-- >>> areEvens 10

-- Array with all integers up to supplied `n` and indicator if it is even or not
-- evensAre :: Int -> Array U Ix1 (Int, Bool)
-- evensAre n =

evensAre :: Int -> Array U Ix1 (Int, Bool)
evensAre n = makeArray Seq (Sz n) $ \ i -> (i, even i)

-- >>> evensAre 6

-- Array with all even integers up to supplied `n` as Just and odds are Nothing
-- evensMaybe :: Int -> Array P Ix1 (Maybe Int)
-- evensMaybe n =

evensMaybe :: Int -> Array B Ix1 (Maybe Int)
evensMaybe n =
  makeArray Seq (Sz n) $ \i ->
    if even i
      then Just i
      else Nothing

---------------------------
-- Strictness properties --
---------------------------

-- >>> [undefined, 'c', error "Whoops"] !! 1


badEvens :: Array U Ix1 (Int, Bool)
badEvens = makeArray Seq 10 $ \ i -> (i, even i && undefined)

-- >>> fst (badEvens ! 0)


-- >>> arr = fromList Seq [undefined, 'c', error "Whoops"] :: Array B Ix1 Char
-- >>> arr ! 1


-- >>> arr = fromList Seq ['a', 'c', error "Whoops"] :: Array B Ix1 Char
-- >>> arr ! 1

-- >>> arr = fromList Seq [Just undefined, Just 'c', Nothing] :: Array B Ix1 (Maybe Char)
-- >>> arr ! 1


-- >>> arr = fromList Seq ["foo" ++ undefined, "bar", ""] :: Array B Ix1 String
-- >>> arr ! 1


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