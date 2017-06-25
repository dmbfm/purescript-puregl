module PureGL.Utils.Log where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

foreign import logObject :: forall a eff. String -> a -> Eff (console :: CONSOLE | eff) Unit
