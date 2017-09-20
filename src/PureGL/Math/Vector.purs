module PureGL.Math.Vector where

import Prelude
import Math (sqrt)
import PureGL.Data.TypedArrays (class ToTypedArray, Float32Array, fromArray)
import PureGL.Utils.Math (class ApproxEq, approxEq, (~=))

-- | A two component vector, defined as a record
newtype Vector2 = Vector2 {x :: Number, y :: Number}

-- | A three component vector, defined as a record
newtype Vector3 = Vector3 {x :: Number, y :: Number, z :: Number}

-- | A four component vector, defined as a record
newtype Vector4 = Vector4 {x :: Number, y :: Number, z :: Number, w :: Number}

-- | Create a `Vector2` from two given numbers
mkVector2 :: Number -> Number -> Vector2
mkVector2 x y = Vector2 {x: x, y: y}

-- | Create a `Vector3` from three given numbers
mkVector3 :: Number -> Number -> Number -> Vector3
mkVector3 x y z = Vector3 {x: x, y: y, z: z}

-- | Create a `Vector4` from four given numbers
mkVector4 :: Number -> Number -> Number -> Number -> Vector4
mkVector4 x y z w = Vector4 {x: x, y: y, z: z, w: w}

-- Eq, ApproxEq, and show instances for Vector{2,3,4}
instance vec2Eq :: Eq Vector2 where
  eq (Vector2 v1) (Vector2 v2) = (v1.x == v2.x) && (v1.y == v2.y)

instance vec3Eq :: Eq Vector3 where
  eq (Vector3 v1) (Vector3 v2) = (v1.x == v2.x) && (v1.y == v2.y) && (v1.z == v2.z)

instance vec4Eq :: Eq Vector4 where
  eq (Vector4 v1) (Vector4 v2) = (v1.x == v2.x) && (v1.y == v2.y) && (v1.z == v2.z) && (v1.w == v2.w)

instance approxEq :: ApproxEq Vector2 where
  approxEq (Vector2 v1) (Vector2 v2) = (v1.x ~= v2.x) && (v1.y ~= v2.y)

instance approxVec3Eq :: ApproxEq Vector3 where
  approxEq (Vector3 v1) (Vector3 v2) = (v1.x ~= v2.x) && (v1.y ~= v2.y) && (v1.z ~= v2.z)

instance approxVec4Eq :: ApproxEq Vector4 where
  approxEq (Vector4 v1) (Vector4 v2) = (v1.x ~= v2.x) && (v1.y ~= v2.y) && (v1.z ~= v2.z) && (v1.w ~= v2.w)  

instance vec2Show :: Show Vector2 where
  show (Vector2 v) = "(" <> (show v.x) <> ", " <> (show v.y) <> ")"

instance vec3Show :: Show Vector3 where
  show (Vector3 v) = "(" <> (show v.x) <> ", " <> (show v.y) <> ", " <> (show v.z) <> ")"

instance vec4Show :: Show Vector4 where
  show (Vector4 v) = "(" <> (show v.x) <> ", " <> (show v.y) <> ", " <> (show v.z) <> ", " <> (show v.w) <> ")"    

-- | The `Vector` class provides the basic operations in a vector space: 
-- |  - Vector addition: `add` 
-- |  - Subtraction: `sub`
-- |  - Zero element: `zero` 
-- |  - Inverse: `inv`  
-- |  - Scalar multiplication: `mul`
-- | 
class Vector a where
  add :: a -> a -> a
  sub :: a -> a -> a 
  zero :: a 
  inv :: a -> a 
  mul :: Number -> a -> a  

-- | This class implements the dot, or inner product, between elements
-- | in an Inner product space
class InnerProduct a where
  dot :: a -> a -> Number

-- | Computes the norm of an inner product space element
norm :: forall a. InnerProduct a => a -> Number
norm v = sqrt $ dot v v

normalize :: forall a. Vector a => InnerProduct a => a -> a
normalize v = mul (1.0 / (norm v)) v

-- Vector and InnerProduct instances for Vector{2,3,4}
instance vector2Vector :: Vector Vector2 where
  add (Vector2 v1) (Vector2 v2) = Vector2 {x: v1.x + v2.x, y: v1.y + v2.y}
  sub (Vector2 v1) (Vector2 v2) = Vector2 {x: v1.x - v2.x, y: v1.y - v2.y}
  zero = Vector2 {x: 0.0, y: 0.0}
  inv (Vector2 v) = Vector2 {x: -v.x, y: -v.y}
  mul a (Vector2 v) = Vector2 {x: a * v.x, y: a * v.y}

instance vector2InnerProduct :: InnerProduct Vector2 where
  dot (Vector2 v1) (Vector2 v2) = v1.x * v2.x + v1.y * v2.y 

instance vector3Vector :: Vector Vector3 where
  add (Vector3 v1) (Vector3 v2) = Vector3 {x: v1.x + v2.x, y: v1.y + v2.y, z: v1.z + v2.z}
  sub (Vector3 v1) (Vector3 v2) = Vector3 {x: v1.x - v2.x, y: v1.y - v2.y, z: v1.z - v2.z}
  zero = Vector3 {x: 0.0, y: 0.0, z: 0.0}
  inv (Vector3 v) = Vector3 {x: -v.x, y: -v.y, z: -v.z}
  mul a (Vector3 v) = Vector3 {x: a * v.x, y: a * v.y, z: a * v.z}

instance vector3InnerProduct :: InnerProduct Vector3 where
  dot (Vector3 v1) (Vector3 v2) = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z

instance vector4Vector :: Vector Vector4 where
  add (Vector4 v1) (Vector4 v2) = Vector4 {x: v1.x + v2.x, y: v1.y + v2.y, z: v1.z + v2.z, w: v1.w + v2.w}
  sub (Vector4 v1) (Vector4 v2) = Vector4 {x: v1.x - v2.x, y: v1.y - v2.y, z: v1.z - v2.z, w: v1.w - v2.w}
  zero = Vector4 {x: 0.0, y: 0.0, z: 0.0, w: 0.0}
  inv (Vector4 v) = Vector4 {x: -v.x, y: -v.y, z: -v.z, w: -v.w}
  mul a (Vector4 v) = Vector4 {x: a * v.x, y: a * v.y, z: a * v.z, w: a * v.w}

instance vector4InnerProduct :: InnerProduct Vector4 where
  dot (Vector4 v1) (Vector4 v2) = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w

instance toTypedArrayVector2 :: ToTypedArray Vector2 Float32Array Number where
  toTypedArray (Vector2 v) = fromArray [v.x, v.y]

instance toTypedArrayVector3 :: ToTypedArray Vector3 Float32Array Number where
  toTypedArray (Vector3 v) = fromArray [v.x, v.y, v.z]

instance toTypedArrayVector4 :: ToTypedArray Vector4 Float32Array Number where
  toTypedArray (Vector4 v) = fromArray [v.x, v.y, v.z, v.w]

-- | Computes the cross product between to `Vector3`
cross :: Vector3 -> Vector3 -> Vector3 
cross (Vector3 v1) (Vector3 v2) = 
  Vector3 { x: v1.y * v2.z - v1.z * v2.y 
          , y: v1.z * v2.x - v1.x * v2.z
          , z: v1.x * v2.y - v1.y * v2.x }
