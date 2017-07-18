module PureGL.WebGL where
  
import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (ask)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import PureGL.Context (Context(..), ContextR)
import PureGL.Data.TypedArrays (class BufferSource)
import PureGL.WebGL.Raw as RAW
import PureGL.WebGL.Types (class TextureSource, GLbitfield, GLboolean, GLenum, GLint, GLintptr, GLsizei, GLuint, WebGLBuffer, WebGLProgram, WebGLShader, WebGLTexture, WebGLUniformLocation, WebGLVertexArrayObject)



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

createShader :: forall eff. GLenum -> ContextR eff WebGLShader
createShader t = ask >>= \(Context ctx) -> liftEff $ RAW.createShader ctx.glContext t

shaderSource :: forall eff. WebGLShader -> String -> ContextR eff Unit
shaderSource s src = ask >>= \(Context ctx) -> liftEff $ RAW.shaderSource ctx.glContext s src

compileShader :: forall eff. WebGLShader -> ContextR eff Unit
compileShader s = ask >>= \(Context ctx) -> liftEff $ RAW.compileShader ctx.glContext s

getShaderParameter :: forall eff. WebGLShader -> GLenum -> ContextR eff Foreign
getShaderParameter s p = ask >>= \(Context ctx) -> liftEff $ RAW.getShaderParameter ctx.glContext s p 

getShaderInfoLog :: forall eff. WebGLShader -> ContextR eff String 
getShaderInfoLog s = ask >>= \(Context ctx) -> liftEff $ RAW.getShaderInfoLog ctx.glContext s 

createProgram :: forall eff. ContextR eff WebGLProgram
createProgram = ask >>= \(Context ctx) -> liftEff $ RAW.createProgram ctx.glContext

attachShader :: forall eff. WebGLProgram -> WebGLShader -> ContextR eff Unit
attachShader p s = ask >>= \(Context ctx) -> liftEff $ RAW.attachShader ctx.glContext p s

linkProgram :: forall eff. WebGLProgram -> ContextR eff Unit
linkProgram p = ask >>= \(Context ctx) -> liftEff $ RAW.linkProgram ctx.glContext p

getProgramParameter :: forall eff. WebGLProgram -> GLenum -> ContextR eff Foreign
getProgramParameter s p = ask >>= \(Context ctx) -> liftEff $ RAW.getProgramParameter ctx.glContext s p 

getProgramInfoLog :: forall eff. WebGLProgram -> ContextR eff String 
getProgramInfoLog s = ask >>= \(Context ctx) -> liftEff $ RAW.getProgramInfoLog ctx.glContext s 

getUniformLocation :: forall eff. WebGLProgram -> String -> ContextR eff (Maybe WebGLUniformLocation)
getUniformLocation p n = ask >>= \(Context ctx) -> liftEff $ toMaybe <$> RAW.getUniformLocation ctx.glContext p n

getAttribLocation :: forall eff. WebGLProgram -> String -> ContextR eff GLint
getAttribLocation p n = ask >>= \(Context ctx) -> liftEff $ RAW.getAttribLocation ctx.glContext p n

useProgram :: forall eff. WebGLProgram -> ContextR eff Unit
useProgram p = ask >>= \(Context ctx) -> liftEff $ RAW.useProgram ctx.glContext p

drawArrays :: forall eff. GLenum -> GLint -> GLsizei -> ContextR eff Unit
drawArrays mode first count = ask >>= \(Context ctx) -> liftEff $ RAW.drawArrays ctx.glContext mode first count

clearColor :: forall eff. Number -> Number -> Number -> Number -> ContextR eff Unit
clearColor r g b a = ask >>= \(Context ctx) -> liftEff $ RAW.clearColor ctx.glContext r g b a

clear :: forall eff. GLbitfield -> ContextR eff Unit
clear b = ask >>= \(Context ctx) -> liftEff $ RAW.clear ctx.glContext b

getExtension :: forall eff a. String -> ContextR eff a
getExtension name = ask >>= \(Context ctx) -> liftEff $ RAW.getExtension ctx.glContext name

createTexture :: forall eff. ContextR eff WebGLTexture
createTexture = ask >>= \(Context ctx) -> liftEff $ RAW.createTexture ctx.glContext

bindTexture :: forall eff. GLenum -> WebGLTexture -> ContextR eff Unit
bindTexture target texture = ask >>= \(Context ctx) -> liftEff $ RAW.bindTexture ctx.glContext target texture

texParameteri :: forall eff. GLenum -> GLenum -> Int -> ContextR eff Unit
texParameteri target pname value = ask >>= \(Context ctx) -> liftEff $ RAW.texParameteri ctx.glContext target pname value

texParameterf :: forall eff. GLenum -> GLenum -> Number -> ContextR eff Unit
texParameterf target pname value = ask >>= \(Context ctx) -> liftEff $ RAW.texParameterf ctx.glContext target pname value

generateMipmap :: forall eff. GLenum -> ContextR eff Unit
generateMipmap target = ask >>= \(Context ctx) -> liftEff $ RAW.generateMipmap ctx.glContext target

activeTexture :: forall eff. GLenum -> ContextR eff Unit
activeTexture target = ask >>= \(Context ctx) -> liftEff $ RAW.activeTexture ctx.glContext target

texImage2D :: forall eff s. TextureSource s => 
                            GLenum -> 
                            GLint -> 
                            GLenum ->
                            GLenum -> 
                            GLenum -> 
                            s ->
                            ContextR eff Unit
texImage2D tgt lvl intf f tp pxs = 
  ask >>= \(Context ctx) -> liftEff $ RAW.texImage2D ctx.glContext tgt lvl intf f tp pxs