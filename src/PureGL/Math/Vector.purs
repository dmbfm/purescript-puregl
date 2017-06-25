module PureGL.Math.Vector where

import Prelude

import Math (sqrt)
import PureGL.TypedArrays (Float32Array, fromArray, toArrayFloat32Array)

newtype Vector2 = Vector2 {x :: Number, y :: Number}
newtype Vector3 = Vector3 {x :: Number, y :: Number, z :: Number}
newtype Vector4 = Vector4 {x :: Number, y :: Number, z :: Number, w :: Number}

mkVector2 :: Number -> Number -> Vector2
mkVector2 x y = Vector2 {x: x, y: y}

mkVector3 :: Number -> Number -> Number -> Vector3
mkVector3 x y z = Vector3 {x: x, y: y, z: z}

mkVector4 :: Number -> Number -> Number -> Number -> Vector4
mkVector4 x y z w = Vector4 {x: x, y: y, z: z, w: w}

instance vec2Eq :: Eq Vector2 where
  eq (Vector2 v1) (Vector2 v2) = (v1.x == v2.x) && (v1.y == v2.y)

instance vec3Eq :: Eq Vector3 where
  eq (Vector3 v1) (Vector3 v2) = (v1.x == v2.x) && (v1.y == v2.y) && (v1.z == v2.z)

instance vec4Eq :: Eq Vector4 where
  eq (Vector4 v1) (Vector4 v2) = (v1.x == v2.x) && (v1.y == v2.y) && (v1.z == v2.z) && (v1.w == v2.w)

instance vec2Show :: Show Vector2 where
  show (Vector2 v) = "(" <> (show v.x) <> ", " <> (show v.y) <> ")"

instance vec3Show :: Show Vector3 where
  show (Vector3 v) = "(" <> (show v.x) <> ", " <> (show v.y) <> ", " <> (show v.z) <> ")"

instance vec4Show :: Show Vector4 where
  show (Vector4 v) = "(" <> (show v.x) <> ", " <> (show v.y) <> ", " <> (show v.z) <> ", " <> (show v.w) <> ")"    

class Vector a where
  add :: a -> a -> a
  sub :: a -> a -> a 
  zero :: a 
  inv :: a -> a 
  mul :: Number -> a -> a
  toFloat32Array :: a -> Float32Array

class Vector a <= InnerProduct a where
  dot :: a -> a -> Number

norm :: forall a. InnerProduct a => a -> Number
norm v = sqrt $ dot v v 

instance vector2Vector :: Vector Vector2 where
  add (Vector2 v1) (Vector2 v2) = Vector2 {x: v1.x + v2.x, y: v1.y + v2.y}
  sub (Vector2 v1) (Vector2 v2) = Vector2 {x: v1.x - v2.x, y: v1.y - v2.y}
  zero = Vector2 {x: 0.0, y: 0.0}
  inv (Vector2 v) = Vector2 {x: -v.x, y: -v.y}
  mul a (Vector2 v) = Vector2 {x: a * v.x, y: a * v.y}
  toFloat32Array (Vector2 v) = fromArray [v.x, v.y]

instance vector2InnerProduct :: InnerProduct Vector2 where
  dot (Vector2 v1) (Vector2 v2) = v1.x * v2.x + v1.y * v2.y 

instance vector3Vector :: Vector Vector3 where
  add (Vector3 v1) (Vector3 v2) = Vector3 {x: v1.x + v2.x, y: v1.y + v2.y, z: v1.z + v2.z}
  sub (Vector3 v1) (Vector3 v2) = Vector3 {x: v1.x - v2.x, y: v1.y - v2.y, z: v1.z - v2.z}
  zero = Vector3 {x: 0.0, y: 0.0, z: 0.0}
  inv (Vector3 v) = Vector3 {x: -v.x, y: -v.y, z: -v.z}
  mul a (Vector3 v) = Vector3 {x: a * v.x, y: a * v.y, z: a * v.z}
  toFloat32Array (Vector3 v) = fromArray [v.x, v.y, v.z]

instance vector3InnerProduct :: InnerProduct Vector3 where
  dot (Vector3 v1) (Vector3 v2) = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z

instance vector4Vector :: Vector Vector4 where
  add (Vector4 v1) (Vector4 v2) = Vector4 {x: v1.x + v2.x, y: v1.y + v2.y, z: v1.z + v2.z, w: v1.w + v2.w}
  sub (Vector4 v1) (Vector4 v2) = Vector4 {x: v1.x - v2.x, y: v1.y - v2.y, z: v1.z - v2.z, w: v1.w - v2.w}
  zero = Vector4 {x: 0.0, y: 0.0, z: 0.0, w: 0.0}
  inv (Vector4 v) = Vector4 {x: -v.x, y: -v.y, z: -v.z, w: -v.w}
  mul a (Vector4 v) = Vector4 {x: a * v.x, y: a * v.y, z: a * v.z, w: a * v.w}
  toFloat32Array (Vector4 v) = fromArray [v.x, v.y, v.z, v.w]

instance vector4InnerProduct :: InnerProduct Vector4 where
  dot (Vector4 v1) (Vector4 v2) = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w

cross :: Vector3 -> Vector3 -> Vector3 
cross (Vector3 v1) (Vector3 v2) = 
  Vector3 { x: v1.y * v2.z - v1.z * v2.y 
          , y: v1.z * v2.x - v1.x * v2.z
          , z: v1.x * v2.y - v1.y * v2.x }

