module MassivTalk.Day1.Intro07_Computation where

import Prelude as P
import Data.Massiv.Array as A


-- | The process of computation:
--
-- * Take a delayed array
-- * Create a mutable array of the same size
-- * Load each element of the delayed array into the new mutable
-- * Freeze the mutable array and get as a result pure manifest array.


