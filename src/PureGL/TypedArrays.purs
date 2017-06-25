module PureGL.TypedArrays where


import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Type.Proxy (Proxy)

-- | Raw binary array buffer
foreign import data ArrayBuffer :: Type

-- | Typed Arrays/Array Buffer Views
foreign import data Int8Array :: Type
foreign import data Int16Array :: Type
foreign import data Int32Array :: Type
foreign import data Uint8Array :: Type
foreign import data Uint16Array :: Type
foreign import data Uint32Array :: Type
foreign import data Uint8ClampedArray :: Type
foreign import data Float32Array :: Type
foreign import data Float64Array :: Type

-- | Effect for Array Buffers
foreign import data ARRAY_BUFFER :: Effect

-- | Create a `ArrayBuffer` with a given size in bytes
foreign import mkArrayBuffer :: Int -> ArrayBuffer

type ByteOffset = Int
type ByteLength = Int

class TypedArray a b | a -> b where
  fromArray :: Array b -> a
  fromBuffer :: ArrayBuffer -> a
  fromBuffer' :: ArrayBuffer -> ByteOffset -> ByteLength -> a
  toArray :: a -> Array b
  bytesPerElement :: Proxy a -> Int
  name :: Proxy a -> String
  length :: a -> Int
  byteLength :: a -> Int
  getBuffer :: a -> ArrayBuffer
  fill :: forall eff. a -> b -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) a
  getAt :: a -> Int -> Maybe b
  setAt :: forall eff. a -> Int -> b -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Unit

instance float32ArrayTypedArray :: TypedArray Float32Array Number where
  fromArray = fromArrayFloat32Array
  fromBuffer = fromBufferFloat32Array
  fromBuffer' = fromBufferFloat32Array2
  toArray = toArrayFloat32Array
  bytesPerElement _ = 4
  name _ = "Float32Array"
  length = lengthFloat32Array  
  byteLength = byteLengthFloat32Array
  getBuffer = getBufferFloat32Array
  fill = fillFloat32Array
  getAt arr = toMaybe <<< getAtFloat32Array arr
  setAt = setAtFloat32Array

instance float32ArrayShow :: Show Float32Array where
  show  = toStringFloat32Array

instance int8ArrayTypedArray :: TypedArray Int8Array Number where
  fromArray = fromArrayInt8Array
  fromBuffer = fromBufferInt8Array
  fromBuffer' = fromBufferInt8Array2
  toArray = toArrayInt8Array
  bytesPerElement _ = 1
  name _ = "Int8Array"
  length = lengthInt8Array  
  byteLength = byteLengthInt8Array
  getBuffer = getBufferInt8Array
  fill = fillInt8Array
  getAt arr = toMaybe <<< getAtInt8Array arr
  setAt = setAtInt8Array

instance int8ArrayShow :: Show Int8Array where
  show  = toStringInt8Array

instance int16ArrayTypedArray :: TypedArray Int16Array Number where
  fromArray = fromArrayInt16Array
  fromBuffer = fromBufferInt16Array
  fromBuffer' = fromBufferInt16Array2
  toArray = toArrayInt16Array
  bytesPerElement _ = 4
  name _ = "Int16Array"
  length = lengthInt16Array  
  byteLength = byteLengthInt16Array
  getBuffer = getBufferInt16Array
  fill = fillInt16Array
  getAt arr = toMaybe <<< getAtInt16Array arr
  setAt = setAtInt16Array

instance int16ArrayShow :: Show Int16Array where
  show  = toStringInt16Array  

foreign import fromArrayFloat32Array :: Array Number -> Float32Array
foreign import toArrayFloat32Array :: Float32Array -> Array Number
foreign import lengthFloat32Array :: Float32Array -> Int
foreign import getBufferFloat32Array :: Float32Array -> ArrayBuffer
foreign import byteLengthFloat32Array :: Float32Array -> Int
foreign import fillFloat32Array :: forall eff. Float32Array -> Number -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Float32Array
foreign import toStringFloat32Array :: Float32Array -> String
foreign import getAtFloat32Array :: Float32Array -> Int -> Nullable Number
foreign import setAtFloat32Array :: forall eff. Float32Array -> Int -> Number -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Unit
foreign import fromBufferFloat32Array2 :: ArrayBuffer -> ByteOffset -> ByteLength -> Float32Array
foreign import fromBufferFloat32Array :: ArrayBuffer -> Float32Array

foreign import fromArrayInt8Array :: Array Number -> Int8Array
foreign import toArrayInt8Array :: Int8Array -> Array Number
foreign import lengthInt8Array :: Int8Array -> Int
foreign import getBufferInt8Array :: Int8Array -> ArrayBuffer
foreign import byteLengthInt8Array :: Int8Array -> Int
foreign import fillInt8Array :: forall eff. Int8Array -> Number -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Int8Array
foreign import toStringInt8Array :: Int8Array -> String
foreign import getAtInt8Array :: Int8Array -> Int -> Nullable Number
foreign import setAtInt8Array :: forall eff. Int8Array -> Int -> Number -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Unit
foreign import fromBufferInt8Array2 :: ArrayBuffer -> ByteOffset -> ByteLength -> Int8Array
foreign import fromBufferInt8Array :: ArrayBuffer -> Int8Array

foreign import fromArrayInt16Array :: Array Number -> Int16Array
foreign import toArrayInt16Array :: Int16Array -> Array Number
foreign import lengthInt16Array :: Int16Array -> Int
foreign import getBufferInt16Array :: Int16Array -> ArrayBuffer
foreign import byteLengthInt16Array :: Int16Array -> Int
foreign import fillInt16Array :: forall eff. Int16Array -> Number -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Int16Array
foreign import toStringInt16Array :: Int16Array -> String
foreign import getAtInt16Array :: Int16Array -> Int -> Nullable Number
foreign import setAtInt16Array :: forall eff. Int16Array -> Int -> Number -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Unit
foreign import fromBufferInt16Array2 :: ArrayBuffer -> ByteOffset -> ByteLength -> Int16Array
foreign import fromBufferInt16Array :: ArrayBuffer -> Int16Array