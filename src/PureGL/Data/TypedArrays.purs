module PureGL.Data.TypedArrays where


import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Type.Proxy (Proxy)

-- | Raw binary array buffer
foreign import data ArrayBuffer :: Type

-- Typed Arrays/Array Buffer Views
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

-- | This class represents a View on a underlying `ArrayBufer`. This should
-- | include all the `TypedArray` types, plus the `DataView` type.
class ArrayBufferView a where
  -- | Create a `TypedArray` view for an `ArrayBuffer`
  fromBuffer :: ArrayBuffer -> a
  -- | Create a `TypedArray` view for an `ArrayBuffer` with given offset and lenght
  fromBuffer' :: ArrayBuffer -> ByteOffset -> ByteLength -> a
  -- | Get the `ArrayBuffer` for the `TypedArray`
  getBuffer :: a -> ArrayBuffer
  -- | Get the `TypedArray` name, e.g., "Float32Array"
  name :: Proxy a -> String
  -- | Get the length, in bytes, of the `TypedArray`
  byteLength :: a -> ByteLength
  -- | Get the length, in bytes, of the `TypedArray`
  byteOffset :: a -> ByteOffset

-- | Type-class for all TypedArray buffers, like Float32Array, Int32Array, etc.
class ArrayBufferView a <= TypedArray a b | a -> b where
  -- | Create a `TypedArray` from an `Array` of elements
  fromArray :: Array b -> a    
  -- | Convert the `TypedArray` to a regular `Array`
  toArray :: a -> Array b
  -- | Get `TypedArray` element size, in bytes
  bytesPerElement :: Proxy a -> Int  
  -- | Get the number of elements in the `TypedArray`
  length :: a -> Int  
  -- | Fill the `TypedArray` with a given value
  fill :: forall eff. a -> b -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) a
  -- | Get the element at a given index
  getAt :: a -> Int -> Maybe b
  -- | Set the element at a given index
  setAt :: forall eff. a -> Int -> b -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Unit

class TypedArray b c <= ToTypedArray a b c | a -> b, b-> c where
  toTypedArray :: a -> b

-- | Emtpy class, representing all buffers, i.e., the `ArrayBuffer` type 
-- | and all `ArrayBufferView` instance types.
class BufferSource a

-- Instances 
instance arrayBufferBufferSource :: BufferSource ArrayBuffer

instance float32ArrayBufferSource :: BufferSource Float32Array

instance float32ArrayArrayBufferView :: ArrayBufferView Float32Array where
  fromBuffer = fromBufferFloat32Array
  fromBuffer' = fromBufferFloat32Array2
  getBuffer = getBufferFloat32Array
  name _ = "Float32Array"
  byteLength = byteLengthFloat32Array  
  byteOffset = _byteOffset

instance float32ArrayTypedArray :: TypedArray Float32Array Number where
  fromArray = fromArrayFloat32Array  
  toArray = toArrayFloat32Array
  bytesPerElement _ = 4  
  length = lengthFloat32Array  
  fill = fillFloat32Array
  getAt arr = toMaybe <<< getAtFloat32Array arr
  setAt = setAtFloat32Array

instance float32ArrayShow :: Show Float32Array where
  show  = toStringFloat32Array

instance int8ArrayBufferSource :: BufferSource Int8Array

instance int8ArrayArrayBufferView :: ArrayBufferView Int8Array where
  fromBuffer = fromBufferInt8Array
  fromBuffer' = fromBufferInt8Array2
  name _ = "Int8Array"
  byteLength = byteLengthInt8Array
  byteOffset = _byteOffset
  getBuffer = getBufferInt8Array

instance int8ArrayTypedArray :: TypedArray Int8Array Number where
  fromArray = fromArrayInt8Array  
  toArray = toArrayInt8Array
  bytesPerElement _ = 1  
  length = lengthInt8Array  
  fill = fillInt8Array
  getAt arr = toMaybe <<< getAtInt8Array arr
  setAt = setAtInt8Array

instance int8ArrayShow :: Show Int8Array where
  show  = toStringInt8Array

instance int16ArrayBufferSource :: BufferSource Int16Array

instance int16ArrayArrayBufferView :: ArrayBufferView Int16Array where
  fromBuffer = fromBufferInt16Array
  fromBuffer' = fromBufferInt16Array2
  name _ = "Int16Array"
  byteLength = byteLengthInt16Array
  getBuffer = getBufferInt16Array
  byteOffset = _byteOffset

instance int16ArrayTypedArray :: TypedArray Int16Array Number where
  fromArray = fromArrayInt16Array
  toArray = toArrayInt16Array
  bytesPerElement _ = 2
  length = lengthInt16Array    
  fill = fillInt16Array
  getAt arr = toMaybe <<< getAtInt16Array arr
  setAt = setAtInt16Array

instance int16ArrayShow :: Show Int16Array where
  show  = toStringInt16Array  

instance int32ArrayArrayBufferView :: ArrayBufferView Int32Array where
  fromBuffer = fromBufferInt32Array
  fromBuffer' = fromBufferInt32Array2
  name _ = "Int32Array"
  byteLength = byteLengthInt32Array
  getBuffer = getBufferInt32Array
  byteOffset = _byteOffset

instance int32ArrayTypedArray :: TypedArray Int32Array Number where
  fromArray = fromArrayInt32Array
  toArray = toArrayInt32Array
  bytesPerElement _ = 2
  length = lengthInt32Array    
  fill = fillInt32Array
  getAt arr = toMaybe <<< getAtInt32Array arr
  setAt = setAtInt32Array

instance int32ArrayShow :: Show Int32Array where
  show  = toStringInt32Array  

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

foreign import fromArrayInt32Array :: Array Number -> Int32Array
foreign import toArrayInt32Array :: Int32Array -> Array Number
foreign import lengthInt32Array :: Int32Array -> Int
foreign import getBufferInt32Array :: Int32Array -> ArrayBuffer
foreign import byteLengthInt32Array :: Int32Array -> Int
foreign import fillInt32Array :: forall eff. Int32Array -> Number -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Int32Array
foreign import toStringInt32Array :: Int32Array -> String
foreign import getAtInt32Array :: Int32Array -> Int -> Nullable Number
foreign import setAtInt32Array :: forall eff. Int32Array -> Int -> Number -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Unit
foreign import fromBufferInt32Array2 :: ArrayBuffer -> ByteOffset -> ByteLength -> Int32Array
foreign import fromBufferInt32Array :: ArrayBuffer -> Int32Array

foreign import _byteOffset :: forall a. a -> ByteOffset