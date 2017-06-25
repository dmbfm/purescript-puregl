module PureGL.Math.Matrix where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import PureGL.Math.Vector (class Vector)
import PureGL.TypedArrays (Float32Array)

foreign import data Matrix2 :: Type
foreign import data Matrix3 :: Type 
foreign import data Matrix4 :: Type

class Vector a <= SquareMatrix a  where
  identity :: a
  transpose :: a -> a
  mulMatrix :: a -> a -> a
  determinant :: a -> Number
  invert :: a -> Maybe a
  fromArray :: Array Number -> a

instance matrix2Eq :: Eq Matrix2 where
  eq = eqMatrix2

instance matrix2Show :: Show Matrix2 where
  show = _toStringMatrix

instance matrix2Vector :: Vector Matrix2 where
  add = addMatrix2
  sub = subMatrix2
  zero = mkMatrix2 0.0 0.0 0.0 0.0
  inv = scalarMulMatrix2 (-1.0)
  mul = scalarMulMatrix2
  toFloat32Array = _toFloat32Array

instance matrix2SquareMatrix :: SquareMatrix Matrix2 where
  identity = mkMatrix2 1.0 0.0 0.0 1.0
  transpose = transposeMatrix2
  mulMatrix = mulMatrix2
  determinant = determinantMatrix2
  invert = toMaybe <<< invertMatrix2
  fromArray = fromArrayMatrix2

instance matrix3Eq :: Eq Matrix3 where
  eq = eqMatrix3

instance matrix3Show :: Show Matrix3 where
  show = _toStringMatrix

instance matrix3Vector :: Vector Matrix3 where
  add = addMatrix3
  sub = subMatrix3
  zero = mkMatrix3 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
  inv = scalarMulMatrix3 (-1.0)
  mul = scalarMulMatrix3
  toFloat32Array = _toFloat32Array

instance matrix3SquareMatrix :: SquareMatrix Matrix3 where
  identity = mkMatrix3 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0
  transpose = transposeMatrix3
  mulMatrix = mulMatrix3
  determinant = determinantMatrix3
  invert = toMaybe <<< invertMatrix3
  fromArray = fromArrayMatrix3


instance matrix4Eq :: Eq Matrix4 where
  eq = eqMatrix4

instance matrix4Show :: Show Matrix4 where
  show = _toStringMatrix

instance matrix4Vector :: Vector Matrix4 where
  add = addMatrix4
  sub = subMatrix4
  zero = mkMatrix4 0.0 0.0 0.0 0.0
                   0.0 0.0 0.0 0.0
                   0.0 0.0 0.0 0.0
                   0.0 0.0 0.0 0.0
  inv = scalarMulMatrix4 (-1.0)
  mul = scalarMulMatrix4
  toFloat32Array = _toFloat32Array

instance matrix4SquareMatrix :: SquareMatrix Matrix4 where
  identity = mkMatrix4 1.0 0.0 0.0 0.0 
                       0.0 1.0 0.0 0.0
                       0.0 0.0 1.0 0.0
                       0.0 0.0 0.0 1.0
  transpose = transposeMatrix4
  mulMatrix = mulMatrix4
  determinant = determinantMatrix4
  invert = toMaybe <<< invertMatrix4
  fromArray = fromArrayMatrix4

foreign import _toFloat32Array :: forall m. m -> Float32Array
foreign import _toStringMatrix :: forall m. m -> String

foreign import mkMatrix2 :: Number -> Number -> Number -> Number -> Matrix2
foreign import eqMatrix2 :: Matrix2 -> Matrix2 -> Boolean
foreign import addMatrix2 :: Matrix2 -> Matrix2 -> Matrix2
foreign import subMatrix2 :: Matrix2 -> Matrix2 -> Matrix2
foreign import transposeMatrix2 :: Matrix2  -> Matrix2
foreign import getArrayMatrix2 :: Matrix2 -> Array Number
foreign import scalarMulMatrix2 :: Number -> Matrix2 -> Matrix2
foreign import mulMatrix2 :: Matrix2 -> Matrix2 -> Matrix2 
foreign import determinantMatrix2 :: Matrix2 -> Number
foreign import invertMatrix2 :: Matrix2 -> Nullable Matrix2
foreign import fromArrayMatrix2 :: Array Number -> Matrix2

foreign import mkMatrix3 :: Number -> Number -> Number -> Number -> 
                            Number -> Number -> Number -> Number -> Number -> Matrix3
foreign import eqMatrix3 :: Matrix3 -> Matrix3 -> Boolean
foreign import addMatrix3 :: Matrix3 -> Matrix3 -> Matrix3
foreign import subMatrix3 :: Matrix3 -> Matrix3 -> Matrix3
foreign import transposeMatrix3 :: Matrix3  -> Matrix3
foreign import getArrayMatrix3 :: Matrix3 -> Array Number
foreign import scalarMulMatrix3 :: Number -> Matrix3 -> Matrix3
foreign import mulMatrix3 :: Matrix3 -> Matrix3 -> Matrix3 
foreign import determinantMatrix3 :: Matrix3 -> Number
foreign import invertMatrix3 :: Matrix3 -> Nullable Matrix3
foreign import fromArrayMatrix3 :: Array Number -> Matrix3

foreign import mkMatrix4 :: Number -> Number -> Number -> Number -> 
                            Number -> Number -> Number -> Number -> 
                            Number -> Number -> Number -> Number -> 
                            Number -> Number -> Number -> Number -> Matrix4
foreign import eqMatrix4 :: Matrix4 -> Matrix4 -> Boolean
foreign import addMatrix4 :: Matrix4 -> Matrix4 -> Matrix4
foreign import subMatrix4 :: Matrix4 -> Matrix4 -> Matrix4
foreign import transposeMatrix4 :: Matrix4  -> Matrix4
foreign import getArrayMatrix4 :: Matrix4 -> Array Number
foreign import scalarMulMatrix4 :: Number -> Matrix4 -> Matrix4
foreign import mulMatrix4 :: Matrix4 -> Matrix4 -> Matrix4 
foreign import determinantMatrix4 :: Matrix4 -> Number
foreign import invertMatrix4 :: Matrix4 -> Nullable Matrix4
foreign import fromArrayMatrix4 :: Array Number -> Matrix4

