module PureGL.Examples.Basic.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Unit (Unit)
import PureGL.Math.Vector (mkVector2, testAdd)
import PureGL.Utils.Log (logObject)

main :: Eff (console :: CONSOLE) Unit
main = do
  logObject "testAdd" (testAdd (mkVector2 1.0 2.0) (mkVector2 3.0 4.0))