module PureGL.Math.Matrix.Fast 
  ( FMatrix2
  , FMatrix3
  , FMatrix4
  , mkFMatrix2
  , mkFMatrix3
  , mkFMatrix4
  , class FSquareMatrix
  , identity
  , transpose
  , mulMatrix
  , determinant
  , invert
  , fromArray
  , orthographic
  , orthographic'
  , perspective
  , perspective'
  , perspective''
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.ST (ST, STRef)
import PureGL.Data.TypedArrays (class ToTypedArray, class ToTypedArrayUnsafe, ARRAY_BUFFER, Float32Array, toTypedArray, unsafeToTypedArray)
import PureGL.Math.Matrix (identity, mkRotateX, mkTranslation)
import PureGL.Math.Vector.Fast (class FVector, FVector3)
import PureGL.WebGL.Types (WebGLEff)

-- | A 2x2 Matrix implemented as a mutable Javascript `Float32Array`
foreign import data FMatrix2 :: Type

-- | A 3x3 Matrix implemented as a mutable Javascript `Float32Array
foreign import data FMatrix3 :: Type

-- | A 4x4 Matrix implemented as a mutable Javascript `Float32Array
foreign import data FMatrix4 :: Type

-- | Create a `FMatrix2` from given numbers (row-major order,  sss
-- | e.g. `mkFMatrix2 a00 a01 a10 a11`)
foreign import mkFMatrix2 :: forall e. Number -> Number -> Number -> Number -> WebGLEff e FMatrix2

-- | Create a `FMatrix2` from given numbers (row-major order, 
-- | e.g. `mkFMatrix3 a00 a01 a10 a11 ...`)
foreign import mkFMatrix3 :: forall e. Number -> Number -> Number -> Number -> 
                             Number -> Number -> Number -> Number -> Number -> WebGLEff e FMatrix3

-- | Create a `FMatrix2` from given numbers (row-major order, 
-- | e.g. `mkFMatrix a00 a01 a10 a11 ...`)
foreign import mkFMatrix4 :: forall e. Number -> Number -> Number -> Number -> 
                             Number -> Number -> Number -> Number -> 
                             Number -> Number -> Number -> Number -> 
                             Number -> Number -> Number -> Number -> WebGLEff e FMatrix4

-- | This class has the same operations as the `SquareMatrix` class,
-- | but with effectful mutable effects.
-- | 
-- | In the case of the `invert` methods, you should check it's
-- | `Boolean` return value; it returns `false` if the matrix is
-- | not invertible.
-- | 
class FVector a <= FSquareMatrix a where
  identity :: forall eff. a -> WebGLEff eff Unit
  transpose :: forall eff. a -> a -> WebGLEff eff Unit
  mulMatrix :: forall eff. a -> a -> a -> WebGLEff eff Unit  
  determinant :: forall eff. a -> WebGLEff eff Number
  invert :: forall eff. a -> a -> WebGLEff eff Boolean
  fromArray :: forall eff. Array Number -> WebGLEff eff a    

-- | Create a Orthographic projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkOrtho right left top bottom near far`
-- | 
foreign import orthographic :: forall e. Number -> Number -> Number -> Number -> Number -> Number -> WebGLEff e FMatrix4

-- | Create a Orthographic projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkOrtho width height near far`
-- | 
orthographic' :: forall e. Number -> Number -> Number -> Number -> WebGLEff e  FMatrix4
orthographic' = orthographic2

-- | Create a perspective projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkPerspective right left top bottom near far`
-- |
foreign import perspective :: forall e. Number -> Number -> Number -> Number -> Number -> Number -> WebGLEff e FMatrix4

-- | Create a perspective projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkPerspective width height near far`
-- |
perspective' :: forall e. Number -> Number -> Number -> Number -> WebGLEff e FMatrix4
perspective' = perspective2

-- | Create a perspective projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkPerspective aspect fov near far`
-- |
-- | where `aspect = width / height` is the aspect ratio 
-- | of the view plane, `fov` is the horizontal field of
-- | view angle in radians. 
perspective'' :: forall e. Number -> Number -> Number -> Number -> WebGLEff e FMatrix4
perspective'' = perspective3  

foreign import mkTranslation :: forall e. Number -> Number -> Number -> WebGLEff e FMatrix4

foreign import translation :: forall e. Number -> Number -> Number -> FMatrix4 -> WebGLEff e Unit

translation' :: forall e. FVector3 -> FMatrix4 -> WebGLEff e Unit
translation' = translation2

translate :: forall e. Number -> Number -> Number -> FMatrix4 -> FMatrix4 -> WebGLEff e Unit
translate x y z temp m = do
  translation x y z temp
  mulMatrix temp m m

-- FMatrix{2,3,4} instances
instance showFMatrix2 :: Show FMatrix2 where
  show = _toStringFMatrix

instance fvectorFMatrix2 :: FVector FMatrix2 where
  add = addFMatrix2
  sub = subFMatrix2
  zero = mkFMatrix2 0.0 0.0 0.0 0.0
  inv = invFMatrix2
  mul = mulFMatrix2

instance fsquareMatrixFMatrix2 :: FSquareMatrix FMatrix2 where
  identity = identityFMatrix2
  transpose = transposeFMatrix2
  mulMatrix = mulMatrixFMatrix2
  determinant = determinantFMatrix2
  invert = invertFMatrix2
  fromArray = _fromArrayFMatrix

instance showFMatrix3 :: Show FMatrix3 where
  show = _toStringFMatrix

instance fvectorFMatrix3 :: FVector FMatrix3 where
  add = addFMatrix3
  sub = subFMatrix3
  zero = mkFMatrix3 0.0 0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 0.0
  inv = invFMatrix3
  mul = mulFMatrix3

instance fsquareMatrixFMatrix3 :: FSquareMatrix FMatrix3 where
  identity = identityFMatrix3
  transpose = transposeFMatrix3
  mulMatrix = mulMatrixFMatrix3
  determinant = determinantFMatrix3
  invert = invertFMatrix3
  fromArray = _fromArrayFMatrix

instance showFMatrix4 :: Show FMatrix4 where
  show = _toStringFMatrix

instance fvectorFMatrix4 :: FVector FMatrix4 where
  add = addFMatrix4
  sub = subFMatrix4
  zero = mkFMatrix4 0.0 0.0 0.0 0.0
                    0.0 0.0 0.0 0.0
                    0.0 0.0 0.0 0.0
                    0.0 0.0 0.0 0.0
  inv = invFMatrix4
  mul = mulFMatrix4

instance fsquareMatrixFMatrix4 :: FSquareMatrix FMatrix4 where
  identity = identityFMatrix4
  transpose = transposeFMatrix4
  mulMatrix = mulMatrixFMatrix4
  determinant = determinantFMatrix4
  invert = invertFMatrix4
  fromArray = _fromArrayFMatrix

instance toTypedArrayFMatrix2 :: ToTypedArray FMatrix2 Float32Array Number where
  toTypedArray = _toFloat32ArrayFMatrix

instance toTypedArrayFMatrix3 :: ToTypedArray FMatrix3 Float32Array Number where
  toTypedArray = _toFloat32ArrayFMatrix

instance toTypedArrayFMatrix4 :: ToTypedArray FMatrix4 Float32Array Number where
  toTypedArray = _toFloat32ArrayFMatrix

instance toTypedArrayUnsFMatrix2 :: ToTypedArrayUnsafe FMatrix2 Float32Array Number where
  unsafeToTypedArray = _toFloat32ArrayFMatrixUnsafe

instance toTypedArrayUnsFMatrix3 :: ToTypedArrayUnsafe FMatrix3 Float32Array Number where
  unsafeToTypedArray = _toFloat32ArrayFMatrixUnsafe

instance toTypedArrayUnsFMatrix4 :: ToTypedArrayUnsafe FMatrix4 Float32Array Number where
  unsafeToTypedArray = _toFloat32ArrayFMatrixUnsafe
  

-- other foreign imports
foreign import _toStringFMatrix :: forall a. a -> String
foreign import _toFloat32ArrayFMatrix :: forall a e. a -> Eff (arrayBuffer :: ARRAY_BUFFER | e) Float32Array
foreign import _toFloat32ArrayFMatrixUnsafe :: forall a e. a -> Eff (arrayBuffer :: ARRAY_BUFFER | e) Float32Array
foreign import _fromArrayFMatrix :: forall a e. Array Number -> WebGLEff e a

foreign import orthographic2 :: forall e. Number -> Number -> Number -> Number -> WebGLEff e FMatrix4
foreign import perspective2 :: forall e. Number -> Number -> Number -> Number -> WebGLEff e FMatrix4
foreign import perspective3 :: forall e. Number -> Number -> Number -> Number -> WebGLEff e FMatrix4

-- FMatrix2 foreign imports
foreign import addFMatrix2 :: forall eff. FMatrix2 -> FMatrix2 -> FMatrix2 -> WebGLEff eff Unit
foreign import subFMatrix2 :: forall eff. FMatrix2 -> FMatrix2 -> FMatrix2 -> WebGLEff eff Unit
foreign import invFMatrix2 :: forall eff. FMatrix2 -> FMatrix2 -> WebGLEff eff Unit
foreign import mulFMatrix2 :: forall eff. Number -> FMatrix2 -> FMatrix2 -> WebGLEff eff Unit
foreign import identityFMatrix2 :: forall eff. FMatrix2 -> WebGLEff eff Unit
foreign import transposeFMatrix2 :: forall eff. FMatrix2 -> FMatrix2 -> WebGLEff eff Unit
foreign import mulMatrixFMatrix2 :: forall eff. FMatrix2 -> FMatrix2 -> 
                                                 FMatrix2 -> WebGLEff eff Unit
foreign import determinantFMatrix2 :: forall eff. FMatrix2 -> WebGLEff eff Number
foreign import invertFMatrix2 :: forall eff. FMatrix2 -> FMatrix2 -> WebGLEff eff Boolean

-- FMatrix3 foreign imports
foreign import addFMatrix3 :: forall eff. FMatrix3 -> FMatrix3 -> FMatrix3 -> WebGLEff eff Unit
foreign import subFMatrix3 :: forall eff. FMatrix3 -> FMatrix3 -> FMatrix3 -> WebGLEff eff Unit
foreign import invFMatrix3 :: forall eff. FMatrix3 -> FMatrix3 -> WebGLEff eff Unit
foreign import mulFMatrix3 :: forall eff. Number -> FMatrix3 -> FMatrix3 -> WebGLEff eff Unit
foreign import identityFMatrix3 :: forall eff. FMatrix3 -> WebGLEff eff Unit
foreign import transposeFMatrix3 :: forall eff. FMatrix3 -> FMatrix3 -> WebGLEff eff Unit
foreign import mulMatrixFMatrix3 :: forall eff. FMatrix3 -> FMatrix3 -> 
                                                 FMatrix3 -> WebGLEff eff Unit
foreign import determinantFMatrix3 :: forall eff. FMatrix3 -> WebGLEff eff Number
foreign import invertFMatrix3 :: forall eff. FMatrix3 -> FMatrix3 -> WebGLEff eff Boolean


-- FMatrix4 foreign imports
foreign import addFMatrix4 :: forall eff. FMatrix4 -> FMatrix4 -> FMatrix4 -> WebGLEff eff Unit
foreign import subFMatrix4 :: forall eff. FMatrix4 -> FMatrix4 -> FMatrix4 -> WebGLEff eff Unit
foreign import invFMatrix4 :: forall eff. FMatrix4 -> FMatrix4 -> WebGLEff eff Unit
foreign import mulFMatrix4 :: forall eff. Number -> FMatrix4 -> FMatrix4 -> WebGLEff eff Unit
foreign import identityFMatrix4 :: forall eff. FMatrix4 -> WebGLEff eff Unit
foreign import transposeFMatrix4 :: forall eff. FMatrix4 -> FMatrix4 -> WebGLEff eff Unit
foreign import mulMatrixFMatrix4 :: forall eff. FMatrix4 -> FMatrix4 -> 
                                                 FMatrix4 -> WebGLEff eff Unit
foreign import determinantFMatrix4 :: forall eff. FMatrix4 -> WebGLEff eff Number
foreign import invertFMatrix4 :: forall eff. FMatrix4 -> FMatrix4 -> WebGLEff eff Boolean

-- Transforms
foreign import translation2 :: forall e. FVector3 -> FMatrix4 -> WebGLEff e Unit
-- foreign import mkTranslation :: Number -> Number -> Number -> FMatrix4
-- foreign import _mkTranslation2 :: FVector3 -> FMatrix4

-- foreign import mkScale :: Number -> Number -> Number -> FMatrix4
-- foreign import _mkScale2 :: FVector3 -> FMatrix4

-- foreign import mkRotateX :: Number -> FMatrix4
-- foreign import mkRotateY :: Number -> FMatrix4
-- foreign import mkRotateZ :: Number -> FMatrix4

-- foreign import mkRotation :: FVector3 -> Number -> FMatrix4

-- foreign import translate :: forall eff h. Number -> Number -> Number -> STRef h FMatrix4 -> Eff (st :: ST h | eff) Unit
-- foreign import _translate2 :: forall eff h. FVector3 -> STRef h FMatrix4 -> Eff (st :: ST h | eff) Unit