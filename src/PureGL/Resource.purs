module PureGL.Resource where

import Prelude
import Data.Either (Either)
import PureGL.Context (ContextR)

class Resource a b | a -> b where
  loadResource :: forall eff. a -> ContextR eff (Either String b)
  updateResource :: forall eff. a -> b -> ContextR eff (Either String Unit)
  setId :: a -> Int -> a
  getId :: a -> Int
  
