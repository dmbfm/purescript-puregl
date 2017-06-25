module PureGL.Math.Vector.Fast where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, newSTRef, pureST, readSTRef)

foreign import data Vector2 :: Type

foreign import mkVector2 :: Number -> Number -> Vector2
foreign import addVector2 :: forall h eff. Vector2 -> Vector2 -> (STRef h Vector2) -> Eff (st :: ST h | eff) Unit

testAdd :: Vector2 -> Vector2 -> Vector2
testAdd v1 v2 = pureST do
  r <- newSTRef (mkVector2 0.0 0.0)
  addVector2 v1 v2 r
  readSTRef r
