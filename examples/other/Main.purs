module PureGL.Examples.Other.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Unit (Unit)

main :: Eff (console :: CONSOLE) Unit
main = log "Other example!"

