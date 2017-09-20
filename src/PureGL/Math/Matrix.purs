module PureGL.Math.Matrix 
  ( Matrix2
  , Matrix3
  , Matrix4
  , mkMatrix2
  , mkMatrix3
  , mkMatrix4
  , class SquareMatrix
  , identity
  , transpose
  , mulMatrix
  , determinant
  , invert
  , fromArray
  , mkOrtho
  , mkOrtho'
  , mkPerspective
  , mkPerspective'
  , mkPerspective''
  , mkTranslation
  , mkTranslation'
  , mkScale
  , mkScale'
  , mkRotateX
  , mkRotateY
  , mkRotateZ
  , mkRotation
  , mkRotation'
  , mkLookAt
  , applyTransform
  , translate
  , translate'
  , rotate
  , rotate'
  , scale
  , projectOrtho
  , projectOrtho'
  , projectPerspective
  , projectPerspective'
  , projectPerspective''
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Math (cos, sin)
import PureGL.Data.TypedArrays (class ToTypedArray, ARRAY_BUFFER, Float32Array)
import PureGL.Math.Quaternion (Quaternion(..))
import PureGL.Math.Vector (class Vector, Vector3(..), Vector4(..), cross, dot, mkVector3, normalize, sub)
import PureGL.Utils.Math (toRadians)
import Unsafe.Coerce (unsafeCoerce)

-- | A 2x2 Matrix (Implemented as a javascript array, in column-major order) 
foreign import data Matrix2 :: Type

-- | A 3x3 Matrix (Implemented as a javascript array, in column-major order)
foreign import data Matrix3 :: Type 

-- | A 4x4 Matrix (Implemented as a javascript array, in column-major order)
foreign import data Matrix4 :: Type

-- | Create a `Matrix2` from given numbers (row-major order, 
-- | e.g. `mkMatrix2 a00 a01 a10 a11`)
foreign import mkMatrix2 :: Number -> Number -> Number -> Number -> Matrix2

-- | Create a `Matrix3` from given numbers (row-major order, 
-- | e.g. `mkMatrix3 a00 a01 a10 a11 ...`)
foreign import mkMatrix3 :: Number -> Number -> Number -> Number -> 
                            Number -> Number -> Number -> Number -> Number -> Matrix3

-- | Create a `Matrix4` from given numbers (row-major order, 
-- | e.g. `mkMatrix4 a00 a01 a10 a11 ...`)
foreign import mkMatrix4 :: Number -> Number -> Number -> Number -> 
                            Number -> Number -> Number -> Number -> 
                            Number -> Number -> Number -> Number -> 
                            Number -> Number -> Number -> Number -> Matrix4

-- | The `SquareMatrix` class extends the `Vector` class | by adding the
-- following Matrix operations for square | matrices:
-- |
-- | - The identity matrix: `identity` 
-- | - Matrix transpose: `transpose` 
-- | - Matrix multiplication: `mulMatrix` 
-- | - Matrix determinant: `determinant` 
-- | - Matrix inversion: `invert`
-- |
-- | and a method, `fromArray`, to create a matrix from a | purescript array
-- | (unlinke the `mkMatrix` methods, the `fromArray` function expects the
-- | elements in the array to be in column-major order).
-- |
class Vector a <= SquareMatrix a  where
  identity :: a
  transpose :: a -> a
  mulMatrix :: a -> a -> a
  determinant :: a -> Number
  invert :: a -> Maybe a
  fromArray :: Array Number -> a

-- Eq, Show and SquareMatrixinstances for Matrix{2,3,4}
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

instance toTypedArrayMatrix2 :: ToTypedArray Matrix2 Float32Array Number where
  toTypedArray = _toFloat32Array

instance toTypedArrayMatrix3 :: ToTypedArray Matrix3 Float32Array Number where
  toTypedArray = _toFloat32Array

instance toTypedArrayMatrix4 :: ToTypedArray Matrix4 Float32Array Number where
  toTypedArray = _toFloat32Array

-- | Create a Orthographic projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkOrtho right left top bottom near far`
-- | 
foreign import mkOrtho :: Number -> Number -> Number -> Number -> Number -> Number -> Matrix4

-- | Create a Orthographic projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkOrtho width height near far`
-- | 
mkOrtho' :: Number -> Number -> Number -> Number -> Matrix4
mkOrtho' = mkOrtho2

-- | Create a perspective projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkPerspective right left top bottom near far`
-- |
foreign import mkPerspective :: Number -> Number -> Number -> Number -> Number -> Number -> Matrix4

-- | Create a perspective projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkPerspective width height near far`
-- |
mkPerspective' :: Number -> Number -> Number -> Number -> Matrix4
mkPerspective' = mkPerspective2

-- | Create a perspective projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkPerspective aspect fov near far`
-- |
-- | where `aspect = width / height` is the aspect ratio 
-- | of the view plane, `fov` is the horizontal field of
-- | view angle in radians. 
mkPerspective'' :: Number -> Number -> Number -> Number -> Matrix4
mkPerspective'' = mkPerspective3

-- | Create a translation `Matrix4` from `x`, `y` and `z` coordinates.
mkTranslation :: Number -> Number -> Number -> Matrix4
mkTranslation x y z = fromArray [ 1.0, 0.0, 0.0, 0.0
                                , 0.0, 1.0, 0.0, 0.0
                                , 0.0, 0.0, 1.0, 0.0
                                , x, y, z, 1.0
                                ]                                

-- | Create a translation `Matrix4` from a `Vector3`
mkTranslation' :: Vector3 -> Matrix4
mkTranslation' (Vector3 v) = mkTranslation v.x v.y v.z

-- | Create a scaling `Matrix4` from a `Vector3`
mkScale :: Number -> Number -> Number -> Matrix4
mkScale sx sy sz = fromArray $ [  sx, 0.0, 0.0, 0.0,
                                 0.0,  sy, 0.0, 0.0,
                                 0.0, 0.0,  sz, 0.0,
                                 0.0, 0.0, 0.0, 1.0 ]

-- | Create a scaling `Matrix4` from `sx`, `sy` and `sz` factors. 
mkScale' :: Vector3 -> Matrix4                               
mkScale' (Vector3 v) = mkScale v.x v.y v.z

-- | Create a `Matrix4` for rotations about the `z` axis.
mkRotateZ :: Number -> Matrix4
mkRotateZ a = fromArray $ [ c, s, 0.0, 0.0
                          , ((-1.0) * s), c, 0.0, 0.0
                          , 0.0, 0.0, 1.0, 0.0
                          , 0.0, 0.0, 0.0, 1.0
                          ]
  where
    s = sin $ toRadians a
    c = cos $ toRadians a

-- | Create a `Matrix4` for rotations about the `y` axis.
mkRotateY :: Number -> Matrix4
mkRotateY a = fromArray [ c, 0.0, ((-1.0) * s), 0.0
                        , 0.0, 1.0, 0.0, 0.0
                        , s, 0.0, c, 0.0
                        , 0.0, 0.0, 0.0, 1.0
                        ]
  where
    s = sin $ toRadians a
    c = cos $ toRadians a

-- | Create a `Matrix4` for rotations about the `x` axis.
mkRotateX :: Number -> Matrix4
mkRotateX a = fromArray $ [ 1.0, 0.0, 0.0, 0.0
                          , 0.0, c, s, 0.0
                          , 0.0, ((-1.0) * s), c, 0.0
                          , 0.0, 0.0, 0.0, 1.0
                          ]
  where
    s = sin $ toRadians a
    c = cos $ toRadians a

-- | Create a rotation `Matrix4`for rotations around an arbitrary axis, with
-- | the angle in degrees. 
mkRotation :: Vector3 -> Number -> Matrix4
mkRotation v a = fromArray $ [ (c + (1.0 - c) * u.x * u.x)
                             , ((1.0 - c) * u.x * u.y + s * u.z)
                             , ((1.0 - c) * u.x * u.z - s * u.y)
                             , 0.0

                             , ((1.0 - c) * u.x * u.y - s * u.z)
                             , (c + (1.0 -c) * u.y * u.y)
                             , ((1.0 - c) * u.y * u.z + s * u.x)
                             , 0.0

                             , ((1.0 - c) * u.x * u.z + s * u.y)
                             , ((1.0 - c) * u.y * u.z - s * u.x)
                             , (c + (1.0 - c) * u.z * u.z)
                             , 0.0

                             , 0.0, 0.0, 0.0, 1.0
                             ]
  where
    s = sin $ toRadians a
    c = cos $ toRadians a
    (Vector3 u) = normalize v

mkRotation' :: Quaternion -> Matrix4
mkRotation' q = fromArray [ 1.0 - 2.0 * u.c * u.c - 2.0 * u.d * u.d
                          , 2.0 * u.b * u.c - 2.0 * u.a * u. d
                          , 2.0 * u.b * u.d + 2.0 * u.a * u.c
                          , 0.0

                          , 2.0 * u.b * u.c + 2.0 * u.a * u.d
                          , 1.0 - 2.0 * u.b * u.b - 2.0 * u.d * u.d
                          , 2.0 * u.c * u.d - 2.0 * u.a * u.b
                          , 0.0

                          , 2.0 * u.b * u.d - 2.0 * u.a * u.c
                          , 2.0 * u.c * u.d + 2.0 * u.a * u.b
                          , 1.0 - 2.0 * u.b * u.b - 2.0 * u.c * u.c
                          , 0.0

                          , 0.0, 0.0, 0.0, 1.0
                          ]
  where
    (Quaternion u) = normalize q

-- | Create a Look At `Matrix4` from eye, point and up `Vector3`s.
mkLookAt :: Vector3 -> Vector3 -> Vector3 -> Matrix4
mkLookAt eye point up = 
  let _z' = normalize (sub eye point)
      _x' = normalize (cross up _z')
      _y' = normalize (cross _z' _x')
      (Vector3 z') = _z'
      (Vector3 y') = _y'
      (Vector3 x') = _x'
      (Vector3 e) = eye
      
  in fromArray [ x'.x, y'.x, z'.x, 0.0  
               , x'.y, y'.y, z'.y, 0.0
               , x'.z, y'.z, z'.z, 0.0
               , - (dot  _x' eye), -(dot _y' eye), -(dot _z' eye), 1.0
               ]

-- | Apply a `Matrix4` transform to a `Vector4`.
foreign import applyTransform :: Matrix4 -> Vector4 -> Vector4

-- | Left multiply a `Matrix4` by a translation `Matrix4` obtained from the
-- | `Vector3`.
foreign import translate :: Vector3 -> Matrix4 -> Matrix4 

foreign import rotate :: Vector3 -> Number -> Matrix4 -> Matrix4

foreign import scale :: Vector3 -> Matrix4 -> Matrix4

foreign import projectOrtho :: Number -> Number -> Number -> Number -> Number -> Number -> Matrix4 -> Matrix4


-- | `projectOrtho'  width height near far`
-- | 
projectOrtho' :: Number -> Number -> Number -> Number -> Matrix4 
projectOrtho' = mkOrtho2

foreign import projectPerspective :: Number -> Number -> Number -> Number -> Number -> Number -> Matrix4 -> Matrix4

-- | `projectPerspective' width height near far`
-- |
projectPerspective' :: Number -> Number -> Number -> Number -> Matrix4  -> Matrix4
projectPerspective' = projectPerspective2

-- | `projectPerspective' aspect fov near far`
-- |
-- | where `aspect = width / height` is the aspect ratio 
-- | of the view plane, `fov` is the horizontal field of
-- | view angle in radians. 
projectPerspective'' :: Number -> Number -> Number -> Number -> Matrix4 -> Matrix4
projectPerspective'' = projectPerspective3

-- | Left multiply a `Matrix4` by a `Quaternion` rotation `Matrix4`
rotate' :: Quaternion -> Matrix4 -> Matrix4
rotate' = rotate2

-- | Left multiply a `Matrix4` by a translation `Matrix4` obtained from the
-- | xyz coordinates.
translate' :: Number -> Number -> Number -> Matrix4 -> Matrix4
translate' x y z = translate (Vector3 {x: x, y: y, z: z})


-- other foreign imports
foreign import rotate2 :: Quaternion -> Matrix4 -> Matrix4
foreign import projectOrtho2 :: Number -> Number -> Number -> Number -> Matrix4  -> Matrix4
foreign import projectPerspective2 :: Number -> Number -> Number -> Number -> Matrix4 -> Matrix4
foreign import projectPerspective3 :: Number -> Number -> Number -> Number -> Matrix4 -> Matrix4
foreign import _toFloat32Array :: forall e m. m -> Eff (arrayBuffer :: ARRAY_BUFFER | e) Float32Array
foreign import _toStringMatrix :: forall m. m -> String

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

foreign import mkOrtho2 :: Number -> Number -> Number -> Number -> Matrix4
foreign import mkPerspective2 :: Number -> Number -> Number -> Number -> Matrix4
foreign import mkPerspective3 :: Number -> Number -> Number -> Number -> Matrix4