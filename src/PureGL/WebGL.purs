module PureGL.WebGL where
  
import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (ask)
import PureGL.Context (Context(..), ContextR)
import PureGL.Data.TypedArrays (class BufferSource)
import PureGL.WebGL.Raw as RAW
import PureGL.WebGL.Types (GLboolean, GLenum, GLint, GLsizei, GLuint, WebGLBuffer, WebGLVertexArrayObject, GLintptr)

-- | Typeclass used to extract WebGL constants wraped
-- | in type constructors.
class GLConstant a b | a -> b where
  getValue :: a -> b

createBuffer :: forall eff. ContextR eff WebGLBuffer
createBuffer = ask >>= \(Context ctx) -> liftEff $ RAW.createBuffer ctx.glContext

deleteBuffer :: forall eff. WebGLBuffer -> ContextR eff Unit
deleteBuffer b = ask >>= \(Context ctx) -> liftEff $ RAW.deleteBuffer ctx.glContext b

bindBuffer :: forall eff. GLenum -> WebGLBuffer -> ContextR eff Unit
bindBuffer t b = ask >>= \(Context ctx) -> liftEff $ RAW.bindBuffer ctx.glContext t b

bufferData :: forall eff src. BufferSource src =>  GLenum -> src -> GLenum ->  ContextR eff Unit
bufferData target src usage = ask >>= \(Context ctx) -> liftEff $ RAW.bufferData ctx.glContext target src usage

bufferSubData :: forall eff src. BufferSource src =>  GLenum -> src ->  ContextR eff Unit
bufferSubData target src = ask >>= \(Context ctx) -> liftEff $ RAW.bufferSubData ctx.glContext target 0 src

createVertexArray :: forall eff. ContextR eff WebGLVertexArrayObject
createVertexArray = ask >>= \(Context ctx) -> liftEff $ RAW.createVertexArray ctx.glContext

bindVertexArray :: forall eff. WebGLVertexArrayObject -> ContextR eff Unit
bindVertexArray vao = ask >>= \(Context ctx) -> liftEff $ RAW.bindVertexArray ctx.glContext vao

enableVertexAttribArray :: forall eff. GLuint -> ContextR eff Unit
enableVertexAttribArray idx = ask >>= \(Context ctx) -> liftEff $ RAW.enableVertexAttribArray ctx.glContext idx

vertexAttribPointer :: forall eff. GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> ContextR eff Unit
vertexAttribPointer idx size t norm stride offset = 
  ask >>= \(Context ctx) -> liftEff $ RAW.vertexAttribPointer ctx.glContext idx size t norm stride offset