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
  , mkOrtho
  , mkOrtho'
  , mkPerspective
  , mkPerspective'
  , mkPerspective''
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef)
import PureGL.Math.Vector.Fast (class FVector)
import PureGL.Data.TypedArrays (Float32Array)

-- | A 2x2 Matrix implemented as a Javascript `Float32Array`
foreign import data FMatrix2 :: Type

-- | A 3x3 Matrix implemented as a Javascript `Float32Array
foreign import data FMatrix3 :: Type

-- | A 4x4 Matrix implemented as a Javascript `Float32Array
foreign import data FMatrix4 :: Type

-- | Create a `FMatrix2` from given numbers (row-major order, 
-- | e.g. `mkFMatrix2 a00 a01 a10 a11`)
foreign import mkFMatrix2 :: Number -> Number -> Number -> Number -> FMatrix2

-- | Create a `FMatrix2` from given numbers (row-major order, 
-- | e.g. `mkFMatrix3 a00 a01 a10 a11 ...`)
foreign import mkFMatrix3 :: Number -> Number -> Number -> Number -> 
                             Number -> Number -> Number -> Number -> Number -> FMatrix3

-- | Create a `FMatrix2` from given numbers (row-major order, 
-- | e.g. `mkFMatrix a00 a01 a10 a11 ...`)
foreign import mkFMatrix4 :: Number -> Number -> Number -> Number -> 
                             Number -> Number -> Number -> Number -> 
                             Number -> Number -> Number -> Number -> 
                             Number -> Number -> Number -> Number -> FMatrix4

-- | This class has the same operations as the `SquareMatrix` class,
-- | but implemented via effectful mutations via `STRef`s.
-- | 
-- | In the case of the `invert` methods, you should check it's
-- | `Boolean` return value; it returns `false` if the matrix is
-- | not invertible.
class FVector a <= FSquareMatrix a where
  identity :: forall c. c -> a
  transpose :: forall eff h. a -> STRef h a -> Eff (st :: ST h | eff) Unit
  mulMatrix :: forall eff h. a -> a -> STRef h a -> Eff (st :: ST h | eff) Unit
  determinant :: a -> Number
  invert :: forall eff h. a -> STRef h a -> Eff (st :: ST h | eff) Boolean
  fromArray :: Array Number -> a

-- | Create a Orthographic projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkOrtho right left top bottom near far`
-- | 
foreign import mkOrtho :: Number -> Number -> Number -> Number -> Number -> Number -> FMatrix4

-- | Create a Orthographic projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkOrtho width height near far`
-- | 
mkOrtho' :: Number -> Number -> Number -> Number -> FMatrix4
mkOrtho' = mkOrtho2

-- | Create a perspective projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkPerspective right left top bottom near far`
-- |
foreign import mkPerspective :: Number -> Number -> Number -> Number -> Number -> Number -> FMatrix4

-- | Create a perspective projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkPerspective width height near far`
-- |
mkPerspective' :: Number -> Number -> Number -> Number -> FMatrix4
mkPerspective' = mkPerspective2

-- | Create a perspective projection matrix with the give
-- | frustum parameters:
-- |
-- | `mkPerspective aspect fov near far`
-- |
-- | where `aspect = width / height` is the aspect ratio 
-- | of the view plane, `fov` is the horizontal field of
-- | view angle in radians. 
mkPerspective'' :: Number -> Number -> Number -> Number -> FMatrix4
mkPerspective'' = mkPerspective3  

-- FMatrix{2,3,4} instances
instance eqFMatrix2Inst :: Eq FMatrix2 where
  eq = eqFMatrix2

instance showFMatrix2 :: Show FMatrix2 where
  show = _toStringFMatrix

instance fvectorFMatrix2 :: FVector FMatrix2 where
  add = addFMatrix2
  sub = subFMatrix2
  zero _ = mkFMatrix2 0.0 0.0 0.0 0.0
  inv = invFMatrix2
  mul = mulFMatrix2
  toFloat32Array = _toFloat32ArrayFMatrix

instance fsquareMatrixFMatrix2 :: FSquareMatrix FMatrix2 where
  identity _ = mkFMatrix2 1.0 0.0 0.0 1.0
  transpose = transposeFMatrix2
  mulMatrix = mulMatrixFMatrix2
  determinant = determinantFMatrix2
  invert = invertFMatrix2
  fromArray = _fromArrayFMatrix

instance eqFMatrix3Inst :: Eq FMatrix3 where
  eq = eqFMatrix3

instance showFMatrix3 :: Show FMatrix3 where
  show = _toStringFMatrix

instance fvectorFMatrix3 :: FVector FMatrix3 where
  add = addFMatrix3
  sub = subFMatrix3
  zero _ = mkFMatrix3 0.0 0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 0.0
  inv = invFMatrix3
  mul = mulFMatrix3
  toFloat32Array = _toFloat32ArrayFMatrix

instance fsquareMatrixFMatrix3 :: FSquareMatrix FMatrix3 where
  identity _ = mkFMatrix3 1.0 0.0 0.0
                          0.0 1.0 0.0
                          0.0 0.0 1.0
  transpose = transposeFMatrix3
  mulMatrix = mulMatrixFMatrix3
  determinant = determinantFMatrix3
  invert = invertFMatrix3
  fromArray = _fromArrayFMatrix

instance eqFMatrix4Inst :: Eq FMatrix4 where
  eq = eqFMatrix4

instance showFMatrix4 :: Show FMatrix4 where
  show = _toStringFMatrix

instance fvectorFMatrix4 :: FVector FMatrix4 where
  add = addFMatrix4
  sub = subFMatrix4
  zero _ = mkFMatrix4 0.0 0.0 0.0 0.0
                    0.0 0.0 0.0 0.0
                    0.0 0.0 0.0 0.0
                    0.0 0.0 0.0 0.0
  inv = invFMatrix4
  mul = mulFMatrix4
  toFloat32Array = _toFloat32ArrayFMatrix

instance fsquareMatrixFMatrix4 :: FSquareMatrix FMatrix4 where
  identity _ = mkFMatrix4 1.0 0.0 0.0 0.0
                          0.0 1.0 0.0 0.0
                          0.0 0.0 1.0 0.0
                          0.0 0.0 0.0 1.0
  transpose = transposeFMatrix4
  mulMatrix = mulMatrixFMatrix4
  determinant = determinantFMatrix4
  invert = invertFMatrix4
  fromArray = _fromArrayFMatrix

-- other foreign imports
foreign import eqFMatrix2 :: FMatrix2 -> FMatrix2 -> Boolean
foreign import eqFMatrix3 :: FMatrix3 -> FMatrix3 -> Boolean 
foreign import eqFMatrix4 :: FMatrix4 -> FMatrix4 -> Boolean
foreign import  _toStringFMatrix :: forall a. a -> String
foreign import _toFloat32ArrayFMatrix :: forall a. a -> Float32Array
foreign import _fromArrayFMatrix :: forall a. Array Number -> a
foreign import mkOrtho2 :: Number -> Number -> Number -> Number -> FMatrix4
foreign import mkPerspective2 :: Number -> Number -> Number -> Number -> FMatrix4
foreign import mkPerspective3 :: Number -> Number -> Number -> Number -> FMatrix4

-- FMatrix2 foreign imports
foreign import addFMatrix2 :: forall eff h. FMatrix2 -> FMatrix2 -> STRef h FMatrix2 -> Eff (st :: ST h | eff) Unit
foreign import subFMatrix2 :: forall eff h. FMatrix2 -> FMatrix2 -> STRef h FMatrix2 -> Eff (st :: ST h | eff) Unit
foreign import zeroFMatrix2 :: FMatrix2
foreign import invFMatrix2 :: forall eff h. FMatrix2 -> STRef h FMatrix2 -> Eff (st :: ST h | eff) Unit
foreign import mulFMatrix2 :: forall eff h. Number -> FMatrix2 -> STRef h FMatrix2 -> Eff (st :: ST h | eff) Unit
foreign import identityFMatrix2 :: FMatrix2
foreign import transposeFMatrix2 :: forall eff h. FMatrix2 -> STRef h FMatrix2 -> Eff (st :: ST h | eff) Unit
foreign import mulMatrixFMatrix2 :: forall eff h. FMatrix2 -> FMatrix2 -> 
                                                  STRef h FMatrix2 -> Eff (st :: ST h | eff) Unit
foreign import determinantFMatrix2 :: FMatrix2 -> Number
foreign import invertFMatrix2 :: forall eff h. FMatrix2 -> STRef h FMatrix2 -> Eff (st :: ST h | eff) Boolean

-- FMatrix3 foreign imports
foreign import addFMatrix3 :: forall eff h. FMatrix3 -> FMatrix3 -> STRef h FMatrix3 -> Eff (st :: ST h | eff) Unit
foreign import subFMatrix3 :: forall eff h. FMatrix3 -> FMatrix3 -> STRef h FMatrix3 -> Eff (st :: ST h | eff) Unit
foreign import zeroFMatrix3 :: FMatrix3
foreign import invFMatrix3 :: forall eff h. FMatrix3 -> STRef h FMatrix3 -> Eff (st :: ST h | eff) Unit
foreign import mulFMatrix3 :: forall eff h. Number -> FMatrix3 -> STRef h FMatrix3 -> Eff (st :: ST h | eff) Unit
foreign import identityFMatrix3 :: FMatrix3
foreign import transposeFMatrix3 :: forall eff h. FMatrix3 -> STRef h FMatrix3 -> Eff (st :: ST h | eff) Unit
foreign import mulMatrixFMatrix3 :: forall eff h. FMatrix3 -> FMatrix3 -> 
                                                  STRef h FMatrix3 -> Eff (st :: ST h | eff) Unit
foreign import determinantFMatrix3 :: FMatrix3 -> Number
foreign import invertFMatrix3 :: forall eff h. FMatrix3 -> STRef h FMatrix3 -> Eff (st :: ST h | eff) Boolean


-- FMatrix4 foreign imports
foreign import addFMatrix4 :: forall eff h. FMatrix4 -> FMatrix4 -> STRef h FMatrix4 -> Eff (st :: ST h | eff) Unit
foreign import subFMatrix4 :: forall eff h. FMatrix4 -> FMatrix4 -> STRef h FMatrix4 -> Eff (st :: ST h | eff) Unit
foreign import zeroFMatrix4 :: FMatrix4
foreign import invFMatrix4 :: forall eff h. FMatrix4 -> STRef h FMatrix4 -> Eff (st :: ST h | eff) Unit
foreign import mulFMatrix4 :: forall eff h. Number -> FMatrix4 -> STRef h FMatrix4 -> Eff (st :: ST h | eff) Unit
foreign import identityFMatrix4 :: FMatrix4
foreign import transposeFMatrix4 :: forall eff h. FMatrix4 -> STRef h FMatrix4 -> Eff (st :: ST h | eff) Unit
foreign import mulMatrixFMatrix4 :: forall eff h. FMatrix4 -> FMatrix4 -> 
                                                  STRef h FMatrix4 -> Eff (st :: ST h | eff) Unit
foreign import determinantFMatrix4 :: FMatrix4 -> Number
foreign import invertFMatrix4 :: forall eff h. FMatrix4 -> STRef h FMatrix4 -> Eff (st :: ST h | eff) Boolean
