module Main where

import Data.Massiv.Array
import System.Environment
import MassivTalk.Day1.Intro09_Stencil

main :: IO ()
main = do
  [r, c, n] <- fmap Prelude.read <$> getArgs
  runLangton (Sz (r * 11 :. c * 11)) n
