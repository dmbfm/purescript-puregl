module PureGL.WebGL.Types where

import Control.Monad.Eff (Eff, kind Effect)

-- | The Effect type for WebGL operations
foreign import data WEBGL :: Effect

-- | Type alias for Eff Monad with WebGL Effects
type WebGLEff e = Eff (webgl :: WEBGL | e)

-- | This type refers to the Javascript WebGLContext object returned
-- | by calling HTMLCanvasElement.getContext('webgl')
foreign import data WebGLContext :: Type

-- | This type refers to the Javascript WebGLContext object returned
-- | by calling HTMLCanvasElement.getContext('webgl')
foreign import data WebGL2Context :: Type

-- | This (empty) type class encapsulate all the WebGL Contexts
class GLContext a

-- GLContext instances
instance glContextWebGLContext :: GLContext WebGLContext
instance glContextWebGL2Context :: GLContext WebGL2Context

-- Other WebGL Raw Types
foreign import data WebGLShader :: Type
foreign import data WebGLProgram :: Type
foreign import data WebGLVertexArrayObject :: Type
foreign import data WebGLUniformLocation :: Type
foreign import data WebGLBuffer :: Type

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
