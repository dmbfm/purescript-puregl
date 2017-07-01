module PureGL.Resource where

import Data.Either (Either)
import PureGL.Context (ContextR)

class Resource a b | a -> b where
  loadResource :: forall eff. a -> ContextR eff (Either String b)
  getId :: a -> Int

