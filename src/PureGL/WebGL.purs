module PureGL.WebGL where
  
import Prelude
import PureGL.WebGL.Types

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader (ask)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import PureGL.Context (Context(..))
import PureGL.Data.TypedArrays (class BufferSource, Float32Array, Int32Array)
import PureGL.RenderState (RenderT)
import PureGL.WebGL.Raw as RAW

createBuffer :: forall eff. RenderT eff WebGLBuffer
createBuffer = ask >>= \(Context ctx) -> liftEff $ RAW.createBuffer ctx.glContext

deleteBuffer :: forall eff. WebGLBuffer -> RenderT eff Unit
deleteBuffer b = ask >>= \(Context ctx) -> liftEff $ RAW.deleteBuffer ctx.glContext b

bindBuffer :: forall eff. GLenum -> WebGLBuffer -> RenderT eff Unit
bindBuffer t b = ask >>= \(Context ctx) -> liftEff $ RAW.bindBuffer ctx.glContext t b

bufferData :: forall eff src. BufferSource src =>  GLenum -> src -> GLenum ->  RenderT eff Unit
bufferData target src usage = ask >>= \(Context ctx) -> liftEff $ RAW.bufferData ctx.glContext target src usage

bufferSubData :: forall eff src. BufferSource src =>  GLenum -> src ->  RenderT eff Unit
bufferSubData target src = ask >>= \(Context ctx) -> liftEff $ RAW.bufferSubData ctx.glContext target 0 src

createVertexArray :: forall eff. RenderT eff WebGLVertexArrayObject
createVertexArray = ask >>= \(Context ctx) -> liftEff $ RAW.createVertexArray ctx.glContext

bindVertexArray :: forall eff. WebGLVertexArrayObject -> RenderT eff Unit
bindVertexArray vao = ask >>= \(Context ctx) -> liftEff $ RAW.bindVertexArray ctx.glContext vao

enableVertexAttribArray :: forall eff. GLuint -> RenderT eff Unit
enableVertexAttribArray idx = ask >>= \(Context ctx) -> liftEff $ RAW.enableVertexAttribArray ctx.glContext idx

vertexAttribPointer :: forall eff. GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> RenderT eff Unit
vertexAttribPointer idx size t norm stride offset = 
  ask >>= \(Context ctx) -> liftEff $ RAW.vertexAttribPointer ctx.glContext idx size t norm stride offset

createShader :: forall eff. GLenum -> RenderT eff WebGLShader
createShader t = ask >>= \(Context ctx) -> liftEff $ RAW.createShader ctx.glContext t

shaderSource :: forall eff. WebGLShader -> String -> RenderT eff Unit
shaderSource s src = ask >>= \(Context ctx) -> liftEff $ RAW.shaderSource ctx.glContext s src

compileShader :: forall eff. WebGLShader -> RenderT eff Unit
compileShader s = ask >>= \(Context ctx) -> liftEff $ RAW.compileShader ctx.glContext s

getShaderParameter :: forall eff. WebGLShader -> GLenum -> RenderT eff Foreign
getShaderParameter s p = ask >>= \(Context ctx) -> liftEff $ RAW.getShaderParameter ctx.glContext s p 

getShaderInfoLog :: forall eff. WebGLShader -> RenderT eff String 
getShaderInfoLog s = ask >>= \(Context ctx) -> liftEff $ RAW.getShaderInfoLog ctx.glContext s 

createProgram :: forall eff. RenderT eff WebGLProgram
createProgram = ask >>= \(Context ctx) -> liftEff $ RAW.createProgram ctx.glContext

attachShader :: forall eff. WebGLProgram -> WebGLShader -> RenderT eff Unit
attachShader p s = ask >>= \(Context ctx) -> liftEff $ RAW.attachShader ctx.glContext p s

linkProgram :: forall eff. WebGLProgram -> RenderT eff Unit
linkProgram p = ask >>= \(Context ctx) -> liftEff $ RAW.linkProgram ctx.glContext p

getProgramParameter :: forall eff. WebGLProgram -> GLenum -> RenderT eff Foreign
getProgramParameter s p = ask >>= \(Context ctx) -> liftEff $ RAW.getProgramParameter ctx.glContext s p 

getProgramInfoLog :: forall eff. WebGLProgram -> RenderT eff String 
getProgramInfoLog s = ask >>= \(Context ctx) -> liftEff $ RAW.getProgramInfoLog ctx.glContext s 

getUniformLocation :: forall eff. WebGLProgram -> String -> RenderT eff (Maybe WebGLUniformLocation)
getUniformLocation p n = ask >>= \(Context ctx) -> liftEff $ toMaybe <$> RAW.getUniformLocation ctx.glContext p n

getAttribLocation :: forall eff. WebGLProgram -> String -> RenderT eff GLint
getAttribLocation p n = ask >>= \(Context ctx) -> liftEff $ RAW.getAttribLocation ctx.glContext p n

useProgram :: forall eff. WebGLProgram -> RenderT eff Unit
useProgram p = ask >>= \(Context ctx) -> liftEff $ RAW.useProgram ctx.glContext p

drawArrays :: forall eff. GLenum -> GLint -> GLsizei -> RenderT eff Unit
drawArrays mode first count = ask >>= \(Context ctx) -> liftEff $ RAW.drawArrays ctx.glContext mode first count

clearColor :: forall eff. Number -> Number -> Number -> Number -> RenderT eff Unit
clearColor r g b a = ask >>= \(Context ctx) -> liftEff $ RAW.clearColor ctx.glContext r g b a

clear :: forall eff. GLbitfield -> RenderT eff Unit
clear b = ask >>= \(Context ctx) -> liftEff $ RAW.clear ctx.glContext b

getExtension :: forall eff a. String -> RenderT eff a
getExtension name = ask >>= \(Context ctx) -> liftEff $ RAW.getExtension ctx.glContext name

createTexture :: forall eff. RenderT eff WebGLTexture
createTexture = ask >>= \(Context ctx) -> liftEff $ RAW.createTexture ctx.glContext

bindTexture :: forall eff. GLenum -> WebGLTexture -> RenderT eff Unit
bindTexture target texture = ask >>= \(Context ctx) -> liftEff $ RAW.bindTexture ctx.glContext target texture

texParameteri :: forall eff. GLenum -> GLenum -> Int -> RenderT eff Unit
texParameteri target pname value = ask >>= \(Context ctx) -> liftEff $ RAW.texParameteri ctx.glContext target pname value

texParameterf :: forall eff. GLenum -> GLenum -> Number -> RenderT eff Unit
texParameterf target pname value = ask >>= \(Context ctx) -> liftEff $ RAW.texParameterf ctx.glContext target pname value

generateMipmap :: forall eff. GLenum -> RenderT eff Unit
generateMipmap target = ask >>= \(Context ctx) -> liftEff $ RAW.generateMipmap ctx.glContext target

activeTexture :: forall eff. GLenum -> RenderT eff Unit
activeTexture target = ask >>= \(Context ctx) -> liftEff $ RAW.activeTexture ctx.glContext target

texImage2D :: forall eff s. TextureSource s => 
                            GLenum -> 
                            GLint -> 
                            GLenum ->
                            GLenum -> 
                            GLenum -> 
                            s ->
                            RenderT eff Unit
texImage2D tgt lvl intf f tp pxs = 
  ask >>= \(Context ctx) -> liftEff $ RAW.texImage2D ctx.glContext tgt lvl intf f tp pxs

texImage2D' :: forall eff.    GLenum -> 
                              GLint -> 
                              GLenum ->
                              Int -> 
                              Int -> 
                              GLenum -> 
                              GLenum -> 
                              RenderT eff Unit
texImage2D' tgt lvl intf w h f tp = 
  ask >>= \(Context ctx) -> liftEff $ RAW.texImage2D2 ctx.glContext tgt lvl intf w h f tp 

createFramebuffer :: forall eff. RenderT eff WebGLFramebuffer
createFramebuffer = ask >>= \(Context ctx) -> liftEff $ RAW.createFramebuffer ctx.glContext

bindFramebuffer :: forall eff. GLenum -> WebGLFramebuffer -> RenderT eff Unit
bindFramebuffer t f = ask >>= \(Context ctx) -> liftEff $ RAW.bindFramebuffer ctx.glContext t f

deleteFramebuffer :: forall eff. WebGLFramebuffer -> RenderT eff Unit
deleteFramebuffer f = ask >>= \(Context ctx) -> liftEff $ RAW.deleteFramebuffer ctx.glContext f

framebufferTexture2D :: forall eff. GLenum -> GLenum -> GLenum -> WebGLTexture -> GLint -> RenderT eff Unit
framebufferTexture2D target att textarget texture level = 
  ask >>= \(Context ctx) -> liftEff $ RAW.framebufferTexture2D ctx.glContext target att textarget texture level

framebufferRenderbuffer :: forall eff. GLenum -> GLenum -> GLenum -> WebGLRenderbuffer -> RenderT eff Unit
framebufferRenderbuffer target att rbtarget rb = 
  ask >>= \(Context ctx) -> liftEff $ RAW.framebufferRenderbuffer ctx.glContext target att rbtarget rb


createRenderbuffer :: forall eff. RenderT eff WebGLRenderbuffer
createRenderbuffer = ask >>= \(Context ctx) -> liftEff $ RAW.createRenderbuffer ctx.glContext

bindRenderbuffer :: forall eff. GLenum -> WebGLRenderbuffer -> RenderT eff Unit
bindRenderbuffer t f = ask >>= \(Context ctx) -> liftEff $ RAW.bindRenderbuffer ctx.glContext t f

deleteRenderbuffer :: forall eff. WebGLRenderbuffer -> RenderT eff Unit
deleteRenderbuffer f = ask >>= \(Context ctx) -> liftEff $ RAW.deleteRenderbuffer ctx.glContext f

renderbufferStorage :: forall eff. GLenum -> GLenum -> GLsizei -> GLsizei -> RenderT eff Unit
renderbufferStorage target format w h = 
  ask >>= \(Context ctx) -> liftEff $ RAW.renderbufferStorage ctx.glContext target format w h
  
uniform1f :: forall eff. WebGLUniformLocation -> Number -> RenderT eff Unit
uniform1f loc v = ask >>= \(Context ctx) -> liftEff $ RAW.uniform1f ctx.glContext loc v

uniform2f :: forall eff. WebGLUniformLocation -> Number -> Number -> RenderT eff Unit
uniform2f loc v1 v2 = ask >>= \(Context ctx) -> liftEff $ RAW.uniform2f ctx.glContext loc v1 v2

uniform3f :: forall eff. WebGLUniformLocation -> Number -> Number -> Number -> RenderT eff Unit
uniform3f loc v1 v2 v3 = ask >>= \(Context ctx) -> liftEff $ RAW.uniform3f ctx.glContext loc v1 v2 v3

uniform4f :: forall eff. WebGLUniformLocation -> Number -> Number -> Number -> Number -> RenderT eff Unit
uniform4f loc v1 v2 v3 v4 = ask >>= \(Context ctx) -> liftEff $ RAW.uniform4f ctx.glContext loc v1 v2 v3 v4

uniform1fv :: forall eff. WebGLUniformLocation -> Float32Array -> RenderT eff Unit
uniform1fv loc v = ask >>= \(Context ctx) -> liftEff $ RAW.uniform1fv ctx.glContext loc v

uniform2fv :: forall eff. WebGLUniformLocation -> Float32Array -> RenderT eff Unit
uniform2fv loc v = ask >>= \(Context ctx) -> liftEff $ RAW.uniform2fv ctx.glContext loc v

uniform3fv :: forall eff. WebGLUniformLocation -> Float32Array -> RenderT eff Unit
uniform3fv loc v = ask >>= \(Context ctx) -> liftEff $ RAW.uniform3fv ctx.glContext loc v

uniform4fv :: forall eff. WebGLUniformLocation -> Float32Array -> RenderT eff Unit
uniform4fv loc v = ask >>= \(Context ctx) -> liftEff $ RAW.uniform4fv ctx.glContext loc v

uniform1i :: forall eff. WebGLUniformLocation -> Int -> RenderT eff Unit
uniform1i loc v = ask >>= \(Context ctx) -> liftEff $ RAW.uniform1i ctx.glContext loc v

uniform2i :: forall eff. WebGLUniformLocation -> Int -> Int -> RenderT eff Unit
uniform2i loc v1 v2 = ask >>= \(Context ctx) -> liftEff $ RAW.uniform2i ctx.glContext loc v1 v2

uniform3i :: forall eff. WebGLUniformLocation -> Int -> Int -> Int -> RenderT eff Unit
uniform3i loc v1 v2 v3 = ask >>= \(Context ctx) -> liftEff $ RAW.uniform3i ctx.glContext loc v1 v2 v3

uniform4i :: forall eff. WebGLUniformLocation -> Int -> Int -> Int -> Int -> RenderT eff Unit
uniform4i loc v1 v2 v3 v4 = ask >>= \(Context ctx) -> liftEff $ RAW.uniform4i ctx.glContext loc v1 v2 v3 v4

uniform1iv :: forall eff. WebGLUniformLocation -> Int32Array -> RenderT eff Unit
uniform1iv loc v = ask >>= \(Context ctx) -> liftEff $ RAW.uniform1iv ctx.glContext loc v

uniform2iv :: forall eff. WebGLUniformLocation -> Int32Array -> RenderT eff Unit
uniform2iv loc v = ask >>= \(Context ctx) -> liftEff $ RAW.uniform2iv ctx.glContext loc v

uniform3iv :: forall eff. WebGLUniformLocation -> Int32Array -> RenderT eff Unit
uniform3iv loc v = ask >>= \(Context ctx) -> liftEff $ RAW.uniform3iv ctx.glContext loc v

uniform4iv :: forall eff. WebGLUniformLocation -> Int32Array -> RenderT eff Unit
uniform4iv loc v = ask >>= \(Context ctx) -> liftEff $ RAW.uniform4iv ctx.glContext loc v
