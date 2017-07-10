module PureGL.Buffer where

import Prelude

import Data.Either (Either(..))
import PureGL.Context (ContextR)
import PureGL.Data.TypedArrays (Float32Array)
import PureGL.Resource (class Resource)
import PureGL.WebGL (class GLConstant, bindBuffer, bufferData, bufferSubData, createBuffer, getValue)
import PureGL.WebGL.Constants (gl_ARRAY_BUFFER, gl_DYNAMIC_DRAW, gl_ELEMENT_ARRAY_BUFFER, gl_STATIC_DRAW, gl_STREAM_DRAW)
import PureGL.WebGL.Raw (nullBufferObject)
import PureGL.WebGL.Types (WebGLBuffer)

-- | This represents the local data of a `WebGLBuffer` (for now only Float data allowed).
newtype Buffer = Buffer { bufferData :: Float32Array
                        , target :: BufferTarget
                        , usage :: BufferUsage
                        , id :: Int
                        }

-- | This represents a `WebGLBuffer`, i.e., the GPU data
newtype BufferResource = BufferResource { buffer :: WebGLBuffer
                                        , update :: Boolean
                                        }

data BufferTarget = ArrayBuffer | ElementArrayBuffer
data BufferUsage = StaticDraw | DynamicDraw | StreamDraw

instance bufferTargetGLConstant :: GLConstant BufferTarget Int where
  getValue ArrayBuffer = gl_ARRAY_BUFFER
  getValue ElementArrayBuffer = gl_ELEMENT_ARRAY_BUFFER

instance bufferUsageGLConstant :: GLConstant BufferUsage Int where
  getValue StaticDraw = gl_STATIC_DRAW
  getValue DynamicDraw = gl_DYNAMIC_DRAW
  getValue StreamDraw = gl_STREAM_DRAW

instance showBufferTarget :: Show BufferTarget where
  show ArrayBuffer = "ArrayBuffer"
  show ElementArrayBuffer = "ElementArrayBuffer"

instance showBufferUsage :: Show BufferUsage where
  show StaticDraw = "StaticDraw"
  show DynamicDraw = "DynamicDraw"
  show StreamDraw = "StramDraw"

instance showBuffer :: Show Buffer where
  show (Buffer b) = "Buffer: { bufferData: " <> (show b.bufferData) <> ", target: " 
                                             <> (show b.target) <> ", usage: " 
                                             <> (show b.usage) <> ", uuid:" 
                                             <> (show b.id) <> " }"

instance bufferResource :: Resource Buffer BufferResource where
  loadResource = loadBufferResource
  updateResource = updateBufferResource
  getId (Buffer b) = b.id
  setId (Buffer b) id = (Buffer b {id = id})

mkBuffer :: Float32Array -> BufferTarget -> BufferUsage -> Int -> Buffer
mkBuffer src t u id = Buffer { bufferData: src
                             , target: t
                             , usage: u
                             , id: id}

loadBufferResource :: forall eff. Buffer -> ContextR eff (Either String BufferResource)
loadBufferResource (Buffer b) = do
  buffer <- createBuffer
  bindBuffer (getValue b.target) buffer
  bufferData (getValue b.target) b.bufferData (getValue b.usage)
  bindBuffer (getValue b.target) nullBufferObject
  pure $ Right $ BufferResource { buffer: buffer 
                                , update: true }

updateBufferResource :: forall eff. Buffer -> BufferResource -> ContextR eff (Either String Unit)
updateBufferResource (Buffer b) (BufferResource r) = do
  bindBuffer (getValue b.target) r.buffer
  bufferSubData (getValue b.target) b.bufferData
  bindBuffer (getValue b.target) nullBufferObject
  pure $ Right unit

