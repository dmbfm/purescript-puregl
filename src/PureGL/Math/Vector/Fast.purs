module PureGL.Math.Vector.Fast 
  ( class FVector
  , add
  , sub
  , zero
  , inv
  , mul
  , FVector2
  , FVector3
  , FVector4
  , class FromVector
  , fromVector
  , toVector
  , mkFVector2
  , mkFVector3
  , mkFVector4
  , cross
  , class FInnerProduct
  , dot
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import PureGL.Data.TypedArrays (class ToTypedArray, ARRAY_BUFFER, Float32Array)
import PureGL.Math.Vector (Vector2, Vector3, Vector4)
import PureGL.WebGL.Types (WebGLEff)

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
foreign import mkFVector2 :: forall e. Number -> Number -> WebGLEff e FVector2

-- | Create a `FVector3`
foreign import mkFVector3 :: forall e. Number -> Number -> Number -> WebGLEff e FVector3

-- | Create a `FVector4`
foreign import mkFVector4 :: forall e. Number -> Number -> Number -> Number -> WebGLEff e FVector4

class FVector a where
  add :: forall e. a -> a -> a -> WebGLEff e Unit
  sub :: forall e. a -> a -> a -> WebGLEff e Unit
  zero :: forall e. WebGLEff e a
  inv :: forall e.  a -> a -> WebGLEff e Unit
  mul :: forall e. Number -> a -> a -> WebGLEff e Unit

class FromVector a b | a -> b where 
  fromVector :: forall e. b -> WebGLEff e a
  toVector :: forall e. a -> WebGLEff e b

class FInnerProduct a where
  dot :: forall e. a -> a -> WebGLEff e Number

-- FVector and InnerProduct instances for FVector{2,3,4}
instance fvectorFVector2 :: FVector FVector2 where
  add = addFVector2
  sub = subFVector2
  zero = mkFVector2 0.0 0.0
  inv = invFVector2
  mul = mulFVector2 

instance fromVectorFVector2 :: FromVector FVector2 Vector2 where
  fromVector = fromVector2
  toVector = toVector2  

instance fvector2InnerProduct :: FInnerProduct FVector2 where
  dot = dotFVector2

instance fvectorFVector3 :: FVector FVector3 where
  add = addFVector3
  sub = subFVector3
  zero = mkFVector3 0.0 0.0 0.0
  inv = invFVector3
  mul = mulFVector3

instance fromVectorFVector3 :: FromVector FVector3 Vector3 where
  fromVector = fromVector3
  toVector = toVector3

instance fvector3InnerProduct :: FInnerProduct FVector3 where
  dot = dotFVector3

instance fvectorFVector4 :: FVector FVector4 where
  add = addFVector4
  sub = subFVector4
  zero = mkFVector4 0.0 0.0 0.0 0.0
  inv = invFVector4
  mul = mulFVector4

instance fromVectorFVector4 :: FromVector FVector4 Vector4 where
  fromVector = fromVector4
  toVector = toVector4

instance fVector4InnerProduct :: FInnerProduct FVector4 where
  dot = dotFVector4

instance toTypedArrayFVector2 :: ToTypedArray FVector2 Float32Array Number where
  toTypedArray = toFloat32ArrayFVector2

instance toTypedArrayFVector3 :: ToTypedArray FVector3 Float32Array Number where
  toTypedArray = toFloat32ArrayFVector3

instance toTypedArrayFVector4 :: ToTypedArray FVector4 Float32Array Number where
  toTypedArray = toFloat32ArrayFVector4 

-- foreign imports
foreign import cross :: forall e. FVector3 -> FVector3 -> FVector3 -> WebGLEff e Unit

foreign import eqFVector2 :: forall e. FVector2 -> FVector2 -> WebGLEff e Boolean 
foreign import eqFVector3 :: forall e. FVector3 -> FVector3 -> WebGLEff e Boolean
foreign import eqFVector4 :: forall e. FVector4 -> FVector4 -> WebGLEff e Boolean

foreign import addFVector2 :: forall e. FVector2 -> FVector2 -> FVector2 -> WebGLEff e Unit
foreign import subFVector2 :: forall e. FVector2 -> FVector2 -> FVector2 -> WebGLEff e Unit
foreign import invFVector2 :: forall e. FVector2 -> FVector2 -> WebGLEff e Unit
foreign import mulFVector2 :: forall e. Number -> FVector2 -> FVector2 -> WebGLEff e Unit
foreign import toFloat32ArrayFVector2 :: forall e. FVector2 -> Eff (arrayBuffer :: ARRAY_BUFFER | e) Float32Array
foreign import toStringFVector2 :: forall e. FVector2 -> WebGLEff e String
foreign import dotFVector2 :: forall e. FVector2 -> FVector2 -> WebGLEff e Number

foreign import addFVector3 :: forall h e. FVector3 -> FVector3 -> FVector3 -> WebGLEff e Unit
foreign import subFVector3 :: forall h e. FVector3 -> FVector3 -> FVector3 -> WebGLEff e Unit
foreign import invFVector3 :: forall h e. FVector3 -> FVector3 -> WebGLEff e Unit
foreign import mulFVector3 :: forall h e. Number -> FVector3 -> FVector3 -> WebGLEff e Unit
foreign import toFloat32ArrayFVector3 :: forall e. FVector3 -> Eff (arrayBuffer :: ARRAY_BUFFER | e) Float32Array
foreign import toStringFVector3 :: forall e. FVector3 -> WebGLEff e String
foreign import dotFVector3 :: forall e. FVector3 -> FVector3 -> WebGLEff e Number

foreign import addFVector4 :: forall e. FVector4 -> FVector4 -> FVector4 -> WebGLEff e Unit
foreign import subFVector4 :: forall e. FVector4 -> FVector4 -> FVector4 -> WebGLEff e Unit
foreign import invFVector4 :: forall e. FVector4 -> FVector4 -> WebGLEff e Unit
foreign import mulFVector4 :: forall e. Number -> FVector4 -> FVector4 -> WebGLEff e Unit
foreign import toFloat32ArrayFVector4 :: forall e. FVector4 -> Eff (arrayBuffer :: ARRAY_BUFFER | e) Float32Array
foreign import toStringFVector4 :: forall e. FVector4 -> WebGLEff e String
foreign import dotFVector4 :: forall e. FVector4 -> FVector4 -> WebGLEff e Number

foreign import fromVector2 :: forall e. Vector2 -> WebGLEff e FVector2 
foreign import fromVector3 :: forall e. Vector3 -> WebGLEff e FVector3
foreign import fromVector4 :: forall e. Vector4 -> WebGLEff e FVector4

foreign import toVector2 :: forall e. FVector2 -> WebGLEff e Vector2 
foreign import toVector3 :: forall e. FVector3 -> WebGLEff e Vector3
foreign import toVector4 :: forall e. FVector4 -> WebGLEff e Vector4