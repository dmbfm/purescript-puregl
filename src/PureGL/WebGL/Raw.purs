module PureGL.WebGL.Raw where

import Data.Foreign (Foreign)
import Data.Nullable (Nullable)
import Prelude (Unit)
import PureGL.Data.TypedArrays (class BufferSource, Float32Array, Int32Array, Uint32Array)
import PureGL.WebGL.Types (class TextureSource, GLbitfield, GLboolean, GLenum, GLfloat, GLint, GLintptr, GLsizei, GLsizeiptr, GLuint, WebGLBuffer, WebGLContext, WebGLEff, WebGLFramebuffer, WebGLProgram, WebGLRenderbuffer, WebGLShader, WebGLTexture, WebGLUniformLocation, WebGLVertexArrayObject)


-- | Null `WebGLBuffer`, used for unbinding
foreign import nullBufferObject :: WebGLBuffer

-- | Null `WebGLVertexArrayObject`, used for unbinding
foreign import nullVertexArrayObject :: WebGLVertexArrayObject

foreign import nullFramebufferObject :: WebGLFramebuffer

-- | WebGLBuffer gl.createBuffer();
foreign import createBuffer :: forall eff.  WebGLContext -> WebGLEff eff WebGLBuffer

-- | void gl.bindBuffer(target, buffer);
foreign import bindBuffer :: forall eff. WebGLContext -> GLenum -> WebGLBuffer -> WebGLEff eff Unit

-- | void gl.deleteBuffer(buffer);
foreign import deleteBuffer :: forall eff. WebGLContext -> WebGLBuffer -> WebGLEff eff Unit

-- | void bufferData(enum target, Object data, enum usage)
foreign import bufferData :: forall eff src.
                             BufferSource src =>
                             WebGLContext ->
                             GLenum ->
                             src ->
                             GLenum ->
                             WebGLEff eff Unit

-- | void bufferData(enum target, long size, enum usage)
foreign import bufferData2 :: forall eff.
                              WebGLContext -> 
                              GLenum -> 
                              GLsizeiptr -> 
                              GLenum -> 
                              WebGLEff eff Unit

-- | void bufferData(enum target, ArrayBufferView srcData, enum usage, uint srcO set[, uint length=0]);
foreign import bufferData3 :: forall eff src.
                               BufferSource src =>
                               WebGLContext ->
                               GLenum -> 
                               src -> 
                               GLenum ->
                               GLuint -> 
                               GLuint -> 
                               WebGLEff eff Unit

-- | void bufferData(enum target, Object data, enum usage)
foreign import bufferSubData :: forall eff src.
                                BufferSource src =>
                                WebGLContext ->
                                GLenum ->
                                GLintptr ->
                                src ->
                                WebGLEff eff Unit

foreign import createShader :: forall eff. WebGLContext -> GLenum -> WebGLEff eff WebGLShader
foreign import createProgram :: forall eff. WebGLContext -> WebGLEff eff WebGLProgram
foreign import shaderSource :: forall eff. WebGLContext -> WebGLShader -> String -> WebGLEff eff Unit
foreign import compileShader :: forall eff. WebGLContext -> WebGLShader -> WebGLEff eff Unit
foreign import getShaderInfoLog :: forall eff. WebGLContext -> WebGLShader -> WebGLEff eff String
foreign import attachShader :: forall eff. WebGLContext -> WebGLProgram -> WebGLShader -> WebGLEff eff Unit
foreign import getProgramInfoLog :: forall eff. WebGLContext -> WebGLProgram -> WebGLEff eff String
foreign import deleteProgram :: forall eff. WebGLContext -> WebGLProgram -> WebGLEff eff Unit
foreign import deleteShader :: forall eff. WebGLContext -> WebGLShader -> WebGLEff eff Unit
foreign import linkProgram :: forall eff. WebGLContext -> WebGLProgram -> WebGLEff eff Unit
foreign import useProgram :: forall eff. WebGLContext -> WebGLProgram -> WebGLEff eff Unit
foreign import getShaderParameter :: forall eff. WebGLContext -> WebGLShader -> GLenum -> WebGLEff eff Foreign
foreign import getProgramParameter :: forall eff. WebGLContext -> WebGLProgram -> GLenum -> WebGLEff eff Foreign

foreign import createVertexArray :: forall eff. WebGLContext -> WebGLEff eff WebGLVertexArrayObject
foreign import bindVertexArray :: forall eff. WebGLContext -> WebGLVertexArrayObject -> WebGLEff eff Unit
foreign import deleteVertexArray :: forall eff. WebGLContext -> WebGLVertexArrayObject -> WebGLEff eff Unit

foreign import enableVertexAttribArray :: forall eff. WebGLContext -> GLuint -> WebGLEff eff Unit
foreign import disableVertexAttribArray :: forall eff. WebGLContext -> GLuint -> WebGLEff eff Unit
foreign import getAttribLocation :: forall eff. WebGLContext -> WebGLProgram -> String -> WebGLEff eff GLint
foreign import getUniformLocation :: forall eff. WebGLContext -> WebGLProgram -> String -> WebGLEff eff (Nullable WebGLUniformLocation)
foreign import vertexAttribPointer :: forall eff. WebGLContext -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> WebGLEff eff Unit
foreign import vertexAttribIPointer :: forall eff. WebGLContext -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> WebGLEff eff Unit

foreign import uniform1f :: forall eff. WebGLContext -> WebGLUniformLocation -> GLfloat -> WebGLEff eff Unit
foreign import uniform1fv :: forall eff. WebGLContext -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import uniform1i :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> WebGLEff eff Unit
foreign import uniform1iv :: forall eff. WebGLContext -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import uniform2f :: forall eff. WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> WebGLEff eff Unit
foreign import uniform2fv :: forall eff. WebGLContext -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import uniform2i :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> WebGLEff eff Unit
foreign import uniform2iv :: forall eff. WebGLContext -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import uniform3f :: forall eff. WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> WebGLEff eff Unit
foreign import uniform3fv :: forall eff. WebGLContext -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import uniform3i :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> Int -> WebGLEff eff Unit
foreign import uniform3iv :: forall eff. WebGLContext -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import uniform4f :: forall eff. WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> WebGLEff eff Unit
foreign import uniform4fv :: forall eff. WebGLContext -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import uniform4i :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> Int -> Int -> WebGLEff eff Unit
foreign import uniform4iv :: forall eff. WebGLContext -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import uniformMatrix2fv :: forall eff. WebGLContext -> WebGLUniformLocation -> GLboolean -> Float32Array -> WebGLEff eff Unit
foreign import uniformMatrix3fv :: forall eff. WebGLContext -> WebGLUniformLocation -> GLboolean -> Float32Array -> WebGLEff eff Unit
foreign import uniformMatrix4fv :: forall eff. WebGLContext -> WebGLUniformLocation -> GLboolean -> Float32Array -> WebGLEff eff Unit

foreign import uniform1ui :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> WebGLEff eff Unit
foreign import uniform2ui :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> WebGLEff eff Unit
foreign import uniform3ui :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> Int -> WebGLEff eff Unit
foreign import uniform4ui :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> Int -> Int -> WebGLEff eff Unit

foreign import uniform1uiv :: forall eff. WebGLContext -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit
foreign import uniform2uiv :: forall eff. WebGLContext -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit
foreign import uniform3uiv :: forall eff. WebGLContext -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit
foreign import uniform4uiv :: forall eff. WebGLContext -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit

foreign import drawArrays :: forall eff. WebGLContext -> GLenum -> GLint -> GLsizei -> WebGLEff eff Unit
foreign import drawElements :: forall eff. WebGLContext -> GLenum -> GLsizei -> GLenum -> GLintptr -> WebGLEff eff Unit

foreign import clearColor :: forall eff. WebGLContext -> Number -> Number -> Number -> Number -> WebGLEff eff Unit
foreign import clear :: forall eff. WebGLContext -> GLbitfield -> WebGLEff eff Unit

foreign import getExtension :: forall eff a. WebGLContext -> String -> WebGLEff eff a


foreign import createTexture :: forall eff. WebGLContext -> WebGLEff eff WebGLTexture
foreign import bindTexture :: forall eff. WebGLContext -> GLenum -> WebGLTexture -> WebGLEff eff Unit
foreign import texParameteri :: forall eff. WebGLContext -> GLenum -> GLenum -> Int -> WebGLEff eff Unit
foreign import texParameterf :: forall eff. WebGLContext -> GLenum -> GLenum -> Number -> WebGLEff eff Unit
foreign import generateMipmap :: forall eff. WebGLContext -> GLenum -> WebGLEff eff Unit
foreign import activeTexture :: forall eff. WebGLContext -> GLenum -> WebGLEff eff Unit
foreign import texImage2D :: forall eff s. 
                             TextureSource s =>
                             WebGLContext -> 
                             GLenum -> 
                             GLint -> 
                             GLenum ->
                             GLenum -> 
                             GLenum -> 
                             s ->
                             WebGLEff eff Unit

-- texImage2D(target, level, internalformat, width, height, 0, format, type, null);
foreign import texImage2D2 :: forall eff. 
                              WebGLContext -> 
                              GLenum -> 
                              GLint -> 
                              GLenum ->
                              Int -> 
                              Int -> 
                              GLenum -> 
                              GLenum -> 
                              WebGLEff eff Unit


foreign import createFramebuffer :: forall eff. WebGLContext -> WebGLEff eff WebGLFramebuffer
foreign import deleteFramebuffer :: forall eff. WebGLContext -> WebGLFramebuffer -> WebGLEff eff Unit
foreign import bindFramebuffer :: forall eff. WebGLContext -> GLenum -> WebGLFramebuffer -> WebGLEff eff Unit
foreign import framebufferTexture2D :: forall eff. WebGLContext -> GLenum -> GLenum -> GLenum -> WebGLTexture -> GLint -> WebGLEff eff Unit
foreign import framebufferRenderbuffer :: forall eff. WebGLContext -> GLenum -> GLenum -> GLenum -> WebGLRenderbuffer -> WebGLEff eff Unit

foreign import createRenderbuffer :: forall eff. WebGLContext -> WebGLEff eff WebGLRenderbuffer
foreign import bindRenderbuffer :: forall eff. WebGLContext -> GLenum -> WebGLRenderbuffer -> WebGLEff eff Unit
foreign import renderbufferStorage :: forall eff. WebGLContext -> GLenum -> GLenum -> GLsizei -> GLsizei -> WebGLEff eff Unit
foreign import deleteRenderbuffer :: forall eff. WebGLContext -> WebGLRenderbuffer -> WebGLEff eff Unit
