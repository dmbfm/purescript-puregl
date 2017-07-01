module PureGL.WebGL.Raw where

import Data.Foreign (Foreign, F)
import Data.Nullable (Nullable)
import Prelude (Unit)
import PureGL.Data.TypedArrays (class BufferSource, Float32Array, Int32Array, Uint32Array)
import PureGL.WebGL.Types (class GLContext, GLbitfield, GLboolean, GLenum, GLfloat, GLint, GLintptr, GLsizei, GLsizeiptr, GLuint, WebGL2Context, WebGLBuffer, WebGLContext, WebGLEff, WebGLProgram, WebGLShader, WebGLUniformLocation, WebGLVertexArrayObject)


foreign import nullBufferObject :: WebGLBuffer
foreign import nullVertexArrayObject :: WebGLVertexArrayObject

-- | WebGLBuffer gl.createBuffer();
foreign import createBuffer :: forall eff ctx. GLContext ctx => ctx -> WebGLEff eff WebGLBuffer

-- | void gl.bindBuffer(target, buffer);
foreign import bindBuffer :: forall eff ctx. GLContext ctx => ctx -> GLenum -> WebGLBuffer -> WebGLEff eff Unit

-- | void gl.deleteBuffer(buffer);
foreign import deleteBuffer :: forall eff ctx. GLContext ctx => ctx -> WebGLBuffer -> WebGLEff eff Unit

-- | void bufferData(enum target, Object data, enum usage)
foreign import bufferData :: forall eff src ctx.
                             GLContext ctx =>
                             BufferSource src =>
                             ctx ->
                             GLenum ->
                             src ->
                             GLenum ->
                             WebGLEff eff Unit

-- | void bufferData(enum target, long size, enum usage)
foreign import bufferData2 :: forall eff ctx. 
                              GLContext ctx => 
                              ctx -> 
                              GLenum -> 
                              GLsizeiptr -> 
                              GLenum -> 
                              WebGLEff eff Unit

-- | void bufferData(enum target, ArrayBufferView srcData, enum usage, uint srcO set[, uint length=0]);
foreign import bufferData3 :: forall eff src.
                               BufferSource src =>
                               WebGL2Context -> 
                               GLenum -> 
                               src -> 
                               GLenum ->
                               GLuint -> 
                               GLuint -> 
                               WebGLEff eff Unit

foreign import createShader :: forall eff ctx. GLContext ctx => ctx -> GLenum -> WebGLEff eff WebGLShader
foreign import createProgram :: forall eff ctx. GLContext ctx => ctx -> WebGLEff eff WebGLProgram
foreign import shaderSource :: forall eff ctx. GLContext ctx => ctx -> WebGLShader -> String -> WebGLEff eff Unit
foreign import compileShader :: forall eff ctx. (GLContext ctx) => ctx -> WebGLShader -> WebGLEff eff Unit
foreign import getShaderInfoLog :: forall eff ctx. (GLContext ctx) => ctx -> WebGLShader -> WebGLEff eff String
foreign import attachShader :: forall eff ctx. (GLContext ctx) => ctx -> WebGLProgram -> WebGLShader -> WebGLEff eff Unit
foreign import getProgramInfoLog :: forall eff ctx. (GLContext ctx) => ctx -> WebGLProgram -> WebGLEff eff String
foreign import deleteProgram :: forall eff ctx. (GLContext ctx) => ctx -> WebGLProgram -> WebGLEff eff Unit
foreign import deleteShader :: forall eff ctx. (GLContext ctx) => ctx -> WebGLShader -> WebGLEff eff Unit
foreign import linkProgram :: forall eff ctx. (GLContext ctx) => ctx -> WebGLProgram -> WebGLEff eff Unit
foreign import useProgram :: forall eff ctx. (GLContext ctx) => ctx -> WebGLProgram -> WebGLEff eff Unit
foreign import getShaderParameter :: forall eff ctx. (GLContext ctx) => ctx -> WebGLShader -> GLenum -> WebGLEff eff Foreign
foreign import getProgramParameter :: forall eff ctx. (GLContext ctx) => ctx -> WebGLProgram -> GLenum -> WebGLEff eff Foreign

foreign import createVertexArray :: forall eff ctx. GLContext ctx => ctx -> WebGLEff eff WebGLVertexArrayObject
foreign import bindVertexArray :: forall eff ctx. GLContext ctx => ctx -> WebGLVertexArrayObject -> WebGLEff eff Unit
foreign import deleteVertexArray :: forall eff ctx. GLContext ctx => ctx -> WebGLVertexArrayObject -> WebGLEff eff Unit

foreign import enableVertexAttribArray :: forall eff c. (GLContext c) => c -> GLuint -> WebGLEff eff Unit
foreign import disableVertexAttribArray :: forall eff c. (GLContext c) => c -> GLuint -> WebGLEff eff Unit
foreign import getAttribLocation :: forall eff c. (GLContext c) => c -> WebGLProgram -> String -> WebGLEff eff GLint
foreign import getUniformLocation :: forall eff c. (GLContext c) => c -> WebGLProgram -> String -> WebGLEff eff (Nullable WebGLUniformLocation)
foreign import vertexAttribPointer :: forall eff c. (GLContext c) => c -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> WebGLEff eff Unit
foreign import vertexAttribIPointer :: forall eff. WebGL2Context -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> WebGLEff eff Unit

foreign import uniform1f :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> GLfloat -> WebGLEff eff Unit
foreign import uniform1fv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import uniform1i :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int -> WebGLEff eff Unit
foreign import uniform1iv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import uniform2f :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> GLfloat -> GLfloat -> WebGLEff eff Unit
foreign import uniform2fv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import uniform2i :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int -> Int -> WebGLEff eff Unit
foreign import uniform2iv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import uniform3f :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> WebGLEff eff Unit
foreign import uniform3fv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import uniform3i :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int -> Int -> Int -> WebGLEff eff Unit
foreign import uniform3iv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import uniform4f :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> WebGLEff eff Unit
foreign import uniform4fv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import uniform4i :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int -> Int -> Int -> Int -> WebGLEff eff Unit
foreign import uniform4iv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import uniformMatrix2fv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> GLboolean -> Float32Array -> WebGLEff eff Unit
foreign import uniformMatrix3fv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> GLboolean -> Float32Array -> WebGLEff eff Unit
foreign import uniformMatrix4fv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> GLboolean -> Float32Array -> WebGLEff eff Unit

foreign import uniform1ui :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int -> WebGLEff eff Unit
foreign import uniform2ui :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int -> Int -> WebGLEff eff Unit
foreign import uniform3ui :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int -> Int -> Int -> WebGLEff eff Unit
foreign import uniform4ui :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Int -> Int -> Int -> Int -> WebGLEff eff Unit

foreign import uniform1uiv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit
foreign import uniform2uiv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit
foreign import uniform3uiv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit
foreign import uniform4uiv :: forall eff c. (GLContext c) => c -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit

foreign import drawArrays :: forall eff ctx. GLContext ctx => ctx -> GLenum -> GLint -> GLsizei -> WebGLEff eff Unit
foreign import drawElements :: forall eff ctx. GLContext ctx => ctx -> GLenum -> GLsizei -> GLenum -> GLintptr -> WebGLEff eff Unit

foreign import clearColor :: forall eff ctx. GLContext ctx => ctx -> Number -> Number -> Number -> Number -> WebGLEff eff Unit
foreign import clear :: forall eff ctx. GLContext ctx => ctx -> GLbitfield -> WebGLEff eff Unit
