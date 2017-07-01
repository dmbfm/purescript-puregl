module PureGL.Examples.Basic.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Unit (Unit)
import PureGL.Math.Vector (mkVector2)
import PureGL.Utils.Log (logObject)
import PureGL.WebGL.Raw

main :: Eff (console :: CONSOLE) Unit
main = log "Hesllo!"