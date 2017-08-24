module PureGL.WebGL.Types where

import Control.Monad.Eff (Eff, kind Effect)
import DOM (DOM)
import DOM.HTML.Types (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement)
import PureGL.Data.TypedArrays (ARRAY_BUFFER, Uint8Array)

-- | The Effect type for WebGL operations
foreign import data WEBGL :: Effect

type WebGLEffRows e = (webgl :: WEBGL, arrayBuffer :: ARRAY_BUFFER, dom :: DOM | e)

-- | Type alias for Eff Monad with WebGL Effects
type WebGLEff e = Eff (WebGLEffRows e)

-- | This type refers to the Javascript WebGLContext object returned
-- | by calling HTMLCanvasElement.getContext('webgl')
foreign import data WebGLContext :: Type

foreign import data ImageData :: Type

class TextureSource a 

instance htmlImageElementTextureSource :: TextureSource HTMLImageElement 
instance htmlCanvasElementTextureSource :: TextureSource HTMLCanvasElement
instance htmlVideoElementTextureSource :: TextureSource HTMLVideoElement
instance imageDataTextureSource :: TextureSource ImageData
instance uint8ArrayTextureSource :: TextureSource Uint8Array 

-- Other WebGL Raw Types
foreign import data WebGLShader :: Type
foreign import data WebGLProgram :: Type
foreign import data WebGLVertexArrayObject :: Type
foreign import data WebGLUniformLocation :: Type
foreign import data WebGLBuffer :: Type
foreign import data WebGLTexture :: Type
foreign import data WebGLFramebuffer :: Type
foreign import data WebGLRenderbuffer :: Type

-- Type alisases for WebGL primtive types
type GLenum       = Int
type GLboolean    = Boolean
type GLbitfield   = Int
type GLbyte       = Int
type GLshort      = Int
type GLint        = Int
type GLsizei      = Int
type GLintptr     = Int
type GLsizeiptr   = Int
type GLubyte      = Int
type GLushort     = Int
type GLuint       = Int
type GLfloat      = Number
type GLclampf     = Number
