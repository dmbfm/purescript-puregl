module PureGL.Buffer where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..), either)
import PureGL.Context (Context(..), ContextR, GLVersion(..))
import PureGL.Data.TypedArrays (Float32Array, Int32Array)
import PureGL.Resource (class Resource)
import PureGL.WebGL.Types (class GLContext, WebGLBuffer, WebGLEff)
import PureGL.WebGL (createBuffer, runGLContext)

newtype Buffer = Buffer { bufferData :: Float32Array
                        , target :: BufferTarget
                        , usage :: BufferUsage
                        , id :: Int
                        }

newtype BufferResource = BufferResource { buffer :: WebGLBuffer
                                        , update :: Boolean
                                        }

data BufferData = Float32Buffer Float32Array | Int32Buffer Int32Array
data BufferTarget = ArrayBuffer | ElementArrayBuffer
data BufferUsage = StaticDraw | DynamicDraw | StreamDraw

instance showBufferData :: Show BufferData where
  show (Float32Buffer _) = "[Float32Buffer]"
  show (Int32Buffer _) = "[Int32Buffer]"

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


-- instance bufferResource :: Resource Buffer BufferResource where
--   loadResource = loadBufferResource
--   getId (Buffer b) = b.id

-- loadBufferResource :: forall eff. Buffer -> ContextR eff (Either String BufferResource)
-- loadBufferResource b = do
--   (Context ctxz) <- ask
--   buffer <- liftEff $ either f f  (ctxz.glContext)
--   pure $ Right $ BufferResource { buffer: buffer, update: true }
--   where 
--     f :: forall e ctx. GLContext ctx => ctx -> WebGLEff e WebGLBuffer
--     f ctx = runGLContext ctx do
--       case ctxz.glVersion of
--         WEBGL_VERSION_1 -> createBuffer
--         WEBGL_VERSION_2 -> createBuffer
