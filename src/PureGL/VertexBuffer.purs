module PureGL.VertexBuffer where

import Prelude

import PureGL.Buffer (Buffer)
import PureGL.WebGL (class GLConstant)
import PureGL.WebGL.Constants (gl_BYTE, gl_FLOAT, gl_SHORT, gl_UNSIGNED_BYTE, gl_UNSIGNED_SHORT)
import PureGL.WebGL.Types (GLboolean, GLint, GLsizei, GLintptr)

-- | The `VertexBuffer` type abstracts a `Buffer` with 
-- | a given set of Vertex `Attribute`s
newtype VertexBuffer = VertexBuffer { buffer :: Buffer
                                    , attributes :: Array Attribute
                                    }

-- | The `Attribute` type describes a vertex attribute inside
-- | a WebGL buffer
newtype Attribute = Attribute { name :: String 
                              , size :: GLint
                              , attributeType :: AttributeType
                              , normalized :: GLboolean
                              , stride :: GLsizei
                              , offset :: GLintptr
                              }

-- | Creates a `Attribute` object. 
-- | 
-- | `mkAttribute name size attributeType normalize strid offset`
mkAttribute :: String -> GLint -> AttributeType -> GLboolean -> GLsizei -> GLintptr -> Attribute
mkAttribute n s a n' s' o = Attribute { name: n
                                      , size: s
                                      , attributeType: a
                                      , normalized: n'
                                      , stride: s'
                                      , offset: o
                                      }

-- | Vertex attributes for a interleaved buffer of vertex data of the type
-- | (x, y, z, nx, ny, nz, u, v)
attrs3P3N2UV :: Array Attribute
attrs3P3N2UV = [ mkAttribute "Position" 3 FloatAttribute false 32 0
               , mkAttribute "Position" 3 FloatAttribute false 32 12
               , mkAttribute "Position" 2 FloatAttribute false 32 24
               ]

-- | Create a `VertexBuffer` from a `Buffer` and `Attribute`s
mkVertexBuffer :: Buffer -> Array Attribute -> VertexBuffer
mkVertexBuffer b a = VertexBuffer { buffer: b
                                     , attributes: a
                                     }

-- | Create a P3N2UV `VertexBuffer`
mkVertexBufferP3N2UV :: Buffer -> VertexBuffer
mkVertexBufferP3N2UV b = mkVertexBuffer b attrs3P3N2UV                              

data AttributeType = 
    ByteAttribute
  | ShortAttribute
  | UByteAttribute
  | UShortAttribute
  | FloatAttribute

instance showAttributeType :: Show AttributeType where
  show ByteAttribute = "[Byte]"
  show ShortAttribute = "[Short]"
  show UByteAttribute = "[UnsignedByte]"
  show UShortAttribute = "[UnsignedShort]"
  show FloatAttribute = "[Float]"

instance attributeTypeGLConstant :: GLConstant AttributeType Int where
  getValue ByteAttribute = gl_BYTE
  getValue ShortAttribute = gl_SHORT
  getValue UByteAttribute = gl_UNSIGNED_BYTE
  getValue UShortAttribute = gl_UNSIGNED_SHORT
  getValue FloatAttribute = gl_FLOAT

instance showAttribute :: Show Attribute where
  show (Attribute a) =
       "Attibute: { "
    <> "name: " <> a.name
    <> ", size: " <> (show a.size)
    <> ", attributeType: " <> (show a.attributeType)
    <> ", normalized: " <> (show a.normalized)
    <> ", stride: " <> (show a.stride)
    <> ", offset: " <> (show a.offset)
    <> " }"

instance showVertexBuffer :: Show VertexBuffer where
  show (VertexBuffer vb) = 
       "VertexBuffer: { "
    <> "buffer: " <> (show vb.buffer)
    <> ", attributes: " <> (show vb.attributes)
    <> "}"