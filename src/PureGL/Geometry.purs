module PureGL.Geometry where

import Prelude

import Data.Array (init)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (scanl)
import PureGL.Data.TypedArrays (Float32Array, Int32Array, byteLength, length)
import PureGL.WebGL.Types (WebGLBuffer, WebGLVertexArrayObject)

newtype Geometry = Geometry { attributes :: Array VertexAttribute
                            , indices :: Maybe Int32Array
                            , offsets :: Array Int
                            , vertexData :: Float32Array
                            , vertexSize :: Int
                            , vertexCount :: Int
                            }

newtype LoadedGeometry = LoadedGeometry { buffer :: WebGLBuffer
                                        , vao :: WebGLVertexArrayObject
                                        , vertexCount :: Int
                                        }

data VertexAttribute = FloatAttribute Int

type Attributes = Array VertexAttribute

attr3P3N2UV :: Attributes
attr3P3N2UV = [ FloatAttribute 3
              , FloatAttribute 3
              , FloatAttribute 2
              ]

attributeOffsets :: Attributes -> Array Int
attributeOffsets attrs = case (init attrs) of
  Nothing -> []
  Just arr -> [0] <> scanl (\acc (FloatAttribute size) -> acc + size * 4) 0 arr

vertexByteSize :: Attributes -> Int
vertexByteSize a = foldl (\acc (FloatAttribute size) -> acc + size * 4) 0 a

mkGeometry :: Float32Array -> Attributes -> Geometry
mkGeometry v a = Geometry { attributes: a
                          , vertexData: v
                          , indices: Nothing
                          , offsets: attributeOffsets a
                          , vertexSize: vertexByteSize a
                          , vertexCount: (byteLength v) / (vertexByteSize a)
                          }

mkGeometry' :: Float32Array -> Int32Array -> Attributes -> Geometry
mkGeometry' v i a = Geometry { attributes: a
                             , vertexData: v
                             , indices: Just i
                             , offsets: attributeOffsets a
                             , vertexSize: vertexByteSize a
                             , vertexCount: length i
                             }