module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import PureGL.WebGL.Types (WebGLEffRows)
import Test.PureGL.Geometry (geometrySpec)
import Test.PureGL.Math.Matrix (matrixSpec)
import Test.PureGL.Math.Quaternion (quaternionSpec)
import Test.PureGL.Math.Vector (vectorSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: forall e. Eff (RunnerEffects (WebGLEffRows e)) Unit
main = run [consoleReporter] do 
  vectorSpec
  quaternionSpec
  matrixSpec
  geometrySpec

