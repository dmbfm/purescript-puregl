module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.PureGL.Math.Matrix (matrixSpec)
import Test.PureGL.Math.Matrix.Fast (matrixFastSpec)
import Test.PureGL.Math.Vector (vectorSpec)
import Test.PureGL.Math.Vector.Fast (vectorFastSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do 
  vectorSpec
  matrixSpec
  vectorFastSpec
  matrixFastSpec

