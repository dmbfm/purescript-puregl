module PureGL.Math.Vector.Fast where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef)
import PureGL.Data.TypedArrays (class ToTypedArray, Float32Array)
import PureGL.Math.Vector (class InnerProduct)

-- | A two-component vector, implemented as a Javascript 
-- | `Float32Array`
foreign import data FVector2 :: Type

-- | A three-component vector, implemented as a Javascript 
-- | `Float32Array`
foreign import data FVector3 :: Type

-- | A four-component vector, implemented as a Javascript 
-- | `Float32Array`
foreign import data FVector4 :: Type

-- | Create a `FVector2`
foreign import mkFVector2 :: Number -> Number -> FVector2

-- | Create a `FVector3`
foreign import mkFVector3 :: Number -> Number -> Number -> FVector3

-- | Create a `FVector4`
foreign import mkFVector4 :: Number -> Number -> Number -> Number -> FVector4

-- Eq and Show instances for FVector{2,3,4}
instance fvector2Eq :: Eq FVector2 where
  eq = eqFVector2

instance fvector3Eq :: Eq FVector3 where
  eq = eqFVector3

instance fvector4Eq :: Eq FVector4 where
  eq = eqFVector4

instance fvector2Show :: Show FVector2 where
  show = toStringFVector2

instance fvector3Show :: Show FVector3 where
  show = toStringFVector3

instance fvector4Show :: Show FVector4 where
  show = toStringFVector4

-- | The `FVector` class is similar to the `Vector`
-- | class, but defined as effectful operations that mutate
-- | a pre-allocated output vector, for improved performance,
-- | via `STRef`s.
-- |
-- | ```purescript
-- | let result = pureST do
-- |    ref <- newSTRef (mkFVector3 0.0 0.0 0.0)
-- |    add v1 v2 ref
-- |    readSTRef ref
-- | ``` 
class FVector a where
  add :: forall h eff. a -> a -> STRef h a -> Eff (st :: ST h | eff) Unit
  sub :: forall h eff. a -> a -> STRef h a -> Eff (st :: ST h | eff) Unit
  zero :: forall c. c -> a
  inv :: forall h eff.  a -> STRef h a -> Eff (st :: ST h | eff) Unit
  mul :: forall h eff. Number -> a -> STRef h a -> Eff (st :: ST h | eff) Unit
  toFloat32Array :: a -> Float32Array

-- FVector and InnerProduct instances for FVector{2,3,4}
instance fvectorFVector2 :: FVector FVector2 where
  add = addFVector2
  sub = subFVector2
  zero _ = mkFVector2 0.0 0.0
  inv = invFVector2
  mul = mulFVector2
  toFloat32Array = toFloat32ArrayFVector2

instance fvector2InnerProduct :: InnerProduct FVector2 where
  dot = dotFVector2

instance fvectorFVector3 :: FVector FVector3 where
  add = addFVector3
  sub = subFVector3
  zero _ = mkFVector3 0.0 0.0 0.0
  inv = invFVector3
  mul = mulFVector3
  toFloat32Array = toFloat32ArrayFVector3

instance fvector3InnerProduct :: InnerProduct FVector3 where
  dot = dotFVector3

instance fvectorFVector4 :: FVector FVector4 where
  add = addFVector4
  sub = subFVector4
  zero _ = mkFVector4 0.0 0.0 0.0 0.0
  inv = invFVector4
  mul = mulFVector4
  toFloat32Array = toFloat32ArrayFVector4

instance fVector4InnerProduct :: InnerProduct FVector4 where
  dot = dotFVector4

instance toTypedArrayFVector2 :: ToTypedArray FVector2 Float32Array Number where
  toTypedArray = toFloat32ArrayFVector2

instance toTypedArrayFVector3 :: ToTypedArray FVector3 Float32Array Number where
  toTypedArray = toFloat32ArrayFVector3

instance toTypedArrayFVector4 :: ToTypedArray FVector4 Float32Array Number where
  toTypedArray = toFloat32ArrayFVector4


-- foreign imports
foreign import eqFVector2 :: FVector2 -> FVector2 -> Boolean 
foreign import eqFVector3 :: FVector3 -> FVector3 -> Boolean
foreign import eqFVector4 :: FVector4 -> FVector4 -> Boolean

foreign import addFVector2 :: forall h eff. FVector2 -> FVector2 -> STRef h FVector2 -> Eff (st :: ST h | eff) Unit
foreign import subFVector2 :: forall h eff. FVector2 -> FVector2 -> STRef h FVector2 -> Eff (st :: ST h | eff) Unit
foreign import invFVector2 :: forall h eff. FVector2 -> STRef h FVector2 -> Eff (st :: ST h | eff) Unit
foreign import mulFVector2 :: forall h eff. Number -> FVector2 -> STRef h FVector2 -> Eff (st :: ST h | eff) Unit
foreign import toFloat32ArrayFVector2 :: FVector2 -> Float32Array
foreign import toStringFVector2 :: FVector2 -> String
foreign import dotFVector2 :: FVector2 -> FVector2 -> Number

foreign import addFVector3 :: forall h eff. FVector3 -> FVector3 -> STRef h FVector3 -> Eff (st :: ST h | eff) Unit
foreign import subFVector3 :: forall h eff. FVector3 -> FVector3 -> STRef h FVector3 -> Eff (st :: ST h | eff) Unit
foreign import invFVector3 :: forall h eff. FVector3 -> STRef h FVector3 -> Eff (st :: ST h | eff) Unit
foreign import mulFVector3 :: forall h eff. Number -> FVector3 -> STRef h FVector3 -> Eff (st :: ST h | eff) Unit
foreign import toFloat32ArrayFVector3 :: FVector3 -> Float32Array
foreign import toStringFVector3 :: FVector3 -> String
foreign import dotFVector3 :: FVector3 -> FVector3 -> Number


foreign import addFVector4 :: forall h eff. FVector4 -> FVector4 -> STRef h FVector4 -> Eff (st :: ST h | eff) Unit
foreign import subFVector4 :: forall h eff. FVector4 -> FVector4 -> STRef h FVector4 -> Eff (st :: ST h | eff) Unit
foreign import invFVector4 :: forall h eff. FVector4 -> STRef h FVector4 -> Eff (st :: ST h | eff) Unit
foreign import mulFVector4 :: forall h eff. Number -> FVector4 -> STRef h FVector4 -> Eff (st :: ST h | eff) Unit
foreign import toFloat32ArrayFVector4 :: FVector4 -> Float32Array
foreign import toStringFVector4 :: FVector4 -> String
foreign import dotFVector4 :: FVector4 -> FVector4 -> Number
