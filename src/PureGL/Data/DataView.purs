module PureGL.Data.DataView where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import PureGL.Data.TypedArrays (ARRAY_BUFFER, ArrayBuffer, ByteLength, ByteOffset)

foreign import data DataView :: Type
type Endianness = Boolean 

foreign import mkDataView :: ArrayBuffer -> DataView
mkDataView' :: ArrayBuffer -> ByteOffset -> ByteLength -> DataView
mkDataView' = _mkDataView2

type Setter a = forall eff. DataView -> ByteOffset -> a -> Eff(arrayBuffer :: ARRAY_BUFFER | eff) Unit
type Getter a = forall eff. DataView -> ByteOffset -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) (Maybe a)

setInt8 :: Setter Int
setInt8 = _setter "setInt8" true

setInt16le :: Setter Int
setInt16le = _setter "setInt16" true

setInt16be :: Setter Int
setInt16be = _setter "setInt16" false

setInt16 :: Setter Int
setInt16 = setInt16le

setInt32le :: Setter Int
setInt32le = _setter "setInt32" true

setInt32be :: Setter Int
setInt32be = _setter "setInt32" false

setInt32 :: Setter Int
setInt32 = setInt32le

setUInt8 :: Setter Int
setUInt8 = _setter "setUInt8" true

setUInt16le :: Setter Int
setUInt16le = _setter "setUInt16" true

setUInt16be :: Setter Int
setUInt16be = _setter "setUInt16" false

setUInt16 :: Setter Int
setUInt16 = setUInt16le

setUInt32le :: Setter Int
setUInt32le = _setter "setUInt32" true

setUInt32be :: Setter Int
setUInt32be = _setter "setUInt32" false

setUInt32 :: Setter Int
setUInt32 = setUInt32le

setFloat32le :: Setter Number
setFloat32le = _setter "setFloat32" true

setFloat32be :: Setter Number
setFloat32be = _setter "setFloat32" false

setFloat32 :: Setter Number
setFloat32 = setFloat32le


getInt8 :: Getter Int
getInt8 v o = toMaybe <$> _getter "getInt8" 1 true v o 

getInt16le :: Getter Int
getInt16le v o = toMaybe <$> _getter "getInt16" 1 true v o 

getInt16be :: Getter Int
getInt16be v o = toMaybe <$> _getter "getInt16" 1 false v o 

getInt16 :: Getter Int
getInt16 = getInt16le

getInt32le :: Getter Int
getInt32le v o = toMaybe <$> _getter "getInt32" 1 true v o 

getInt32be :: Getter Int
getInt32be v o = toMaybe <$> _getter "getInt32" 1 false v o 

getInt32 :: Getter Int
getInt32 = getInt32le

getUint8 :: Getter Int
getUint8 v o = toMaybe <$> _getter "getUint8" 1 true v o 

getUint16le :: Getter Int
getUint16le v o = toMaybe <$> _getter "getUint16" 1 true v o 

getUint16be :: Getter Int
getUint16be v o = toMaybe <$> _getter "getUint16" 1 false v o 

getUint16 :: Getter Int
getUint16 = getUint16le

getUint32le :: Getter Int
getUint32le v o = toMaybe <$> _getter "getUint32" 1 true v o 

getUint32be :: Getter Int
getUint32be v o = toMaybe <$> _getter "getUint32" 1 false v o 

getUint32 :: Getter Int
getUint32 = getUint32le

getFloat32le :: Getter Number
getFloat32le v o = toMaybe <$> _getter "getFloat32" 1 true v o 

getFloat32be :: Getter Number
getFloat32be v o = toMaybe <$> _getter "getFloat32" 1 false v o 

getFloat32 :: Getter Number
getFloat32 = getFloat32le

foreign import _mkDataView2 :: ArrayBuffer -> ByteOffset -> ByteLength -> DataView
foreign import _setter :: forall a eff. String -> Endianness -> DataView -> ByteOffset -> a -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) Unit
foreign import _getter :: forall a eff. String -> ByteLength -> Endianness -> DataView -> ByteOffset -> Eff (arrayBuffer :: ARRAY_BUFFER | eff) (Nullable a)

