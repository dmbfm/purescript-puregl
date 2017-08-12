module PureGL.WebGL.Raw where

import Data.Foreign (Foreign)
import Data.Nullable (Nullable)
import Prelude (Unit)
import PureGL.Data.TypedArrays (class BufferSource, Float32Array, Int32Array, Uint32Array)
import PureGL.WebGL.Types (class TextureSource, GLbitfield, GLboolean, GLenum, GLfloat, GLint, GLintptr, GLsizei, GLsizeiptr, GLuint, WebGLBuffer, WebGLContext, WebGLEff, WebGLFramebuffer, WebGLProgram, WebGLRenderbuffer, WebGLShader, WebGLTexture, WebGLUniformLocation, WebGLVertexArrayObject)


-- | Null `WebGLBuffer`, used for unbinding
foreign import glNullBufferObject :: WebGLBuffer

-- | Null `WebGLVertexArrayObject`, used for unbinding
foreign import glNullVertexArrayObject :: WebGLVertexArrayObject

foreign import glNullFramebufferObject :: WebGLFramebuffer

-- | WebGLBuffer gl.createBuffer();
foreign import glCreateBuffer :: forall eff.  WebGLContext -> WebGLEff eff WebGLBuffer

-- | void gl.bindBuffer(target, buffer);
foreign import glBindBuffer :: forall eff. WebGLContext -> GLenum -> WebGLBuffer -> WebGLEff eff Unit

-- | void gl.deleteBuffer(buffer);
foreign import glDeleteBuffer :: forall eff. WebGLContext -> WebGLBuffer -> WebGLEff eff Unit

-- | void bufferData(enum target, Object data, enum usage)
foreign import glBufferData :: forall eff src.
                             BufferSource src =>
                             WebGLContext ->
                             GLenum ->
                             src ->
                             GLenum ->
                             WebGLEff eff Unit

-- | void bufferData(enum target, long size, enum usage)
foreign import glBufferData2 :: forall eff.
                              WebGLContext -> 
                              GLenum -> 
                              GLsizeiptr -> 
                              GLenum -> 
                              WebGLEff eff Unit

-- | void bufferData(enum target, ArrayBufferView srcData, enum usage, uint srcO set[, uint length=0]);
foreign import glBufferData3 :: forall eff src.
                               BufferSource src =>
                               WebGLContext ->
                               GLenum -> 
                               src -> 
                               GLenum ->
                               GLuint -> 
                               GLuint -> 
                               WebGLEff eff Unit

-- | void bufferData(enum target, Object data, enum usage)
foreign import glBufferSubData :: forall eff src.
                                BufferSource src =>
                                WebGLContext ->
                                GLenum ->
                                GLintptr ->
                                src ->
                                WebGLEff eff Unit

foreign import glCreateShader :: forall eff. WebGLContext -> GLenum -> WebGLEff eff WebGLShader
foreign import glCreateProgram :: forall eff. WebGLContext -> WebGLEff eff WebGLProgram
foreign import glShaderSource :: forall eff. WebGLContext -> WebGLShader -> String -> WebGLEff eff Unit
foreign import glCompileShader :: forall eff. WebGLContext -> WebGLShader -> WebGLEff eff Unit
foreign import glGetShaderInfoLog :: forall eff. WebGLContext -> WebGLShader -> WebGLEff eff String
foreign import glAttachShader :: forall eff. WebGLContext -> WebGLProgram -> WebGLShader -> WebGLEff eff Unit
foreign import glGetProgramInfoLog :: forall eff. WebGLContext -> WebGLProgram -> WebGLEff eff String
foreign import glDeleteProgram :: forall eff. WebGLContext -> WebGLProgram -> WebGLEff eff Unit
foreign import glDeleteShader :: forall eff. WebGLContext -> WebGLShader -> WebGLEff eff Unit
foreign import glLinkProgram :: forall eff. WebGLContext -> WebGLProgram -> WebGLEff eff Unit
foreign import glUseProgram :: forall eff. WebGLContext -> WebGLProgram -> WebGLEff eff Unit
foreign import glGetShaderParameter :: forall eff. WebGLContext -> WebGLShader -> GLenum -> WebGLEff eff Foreign
foreign import glGetProgramParameter :: forall eff. WebGLContext -> WebGLProgram -> GLenum -> WebGLEff eff Foreign

foreign import glCreateVertexArray :: forall eff. WebGLContext -> WebGLEff eff WebGLVertexArrayObject
foreign import glBindVertexArray :: forall eff. WebGLContext -> WebGLVertexArrayObject -> WebGLEff eff Unit
foreign import glDeleteVertexArray :: forall eff. WebGLContext -> WebGLVertexArrayObject -> WebGLEff eff Unit

foreign import glEnableVertexAttribArray :: forall eff. WebGLContext -> GLuint -> WebGLEff eff Unit
foreign import glDisableVertexAttribArray :: forall eff. WebGLContext -> GLuint -> WebGLEff eff Unit
foreign import glGetAttribLocation :: forall eff. WebGLContext -> WebGLProgram -> String -> WebGLEff eff GLint
foreign import glGetUniformLocation :: forall eff. WebGLContext -> WebGLProgram -> String -> WebGLEff eff (Nullable WebGLUniformLocation)
foreign import glVertexAttribPointer :: forall eff. WebGLContext -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> WebGLEff eff Unit
foreign import glVertexAttribIPointer :: forall eff. WebGLContext -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> WebGLEff eff Unit

foreign import glUniform1f :: forall eff. WebGLContext -> WebGLUniformLocation -> GLfloat -> WebGLEff eff Unit
foreign import glUniform1fv :: forall eff. WebGLContext -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import glUniform1i :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> WebGLEff eff Unit
foreign import glUniform1iv :: forall eff. WebGLContext -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import glUniform2f :: forall eff. WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> WebGLEff eff Unit
foreign import glUniform2fv :: forall eff. WebGLContext -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import glUniform2i :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> WebGLEff eff Unit
foreign import glUniform2iv :: forall eff. WebGLContext -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import glUniform3f :: forall eff. WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> WebGLEff eff Unit
foreign import glUniform3fv :: forall eff. WebGLContext -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import glUniform3i :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> Int -> WebGLEff eff Unit
foreign import glUniform3iv :: forall eff. WebGLContext -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import glUniform4f :: forall eff. WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> WebGLEff eff Unit
foreign import glUniform4fv :: forall eff. WebGLContext -> WebGLUniformLocation -> Float32Array -> WebGLEff eff Unit
foreign import glUniform4i :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> Int -> Int -> WebGLEff eff Unit
foreign import glUniform4iv :: forall eff. WebGLContext -> WebGLUniformLocation -> Int32Array -> WebGLEff eff Unit

foreign import glUniformMatrix2fv :: forall eff. WebGLContext -> WebGLUniformLocation -> GLboolean -> Float32Array -> WebGLEff eff Unit
foreign import glUniformMatrix3fv :: forall eff. WebGLContext -> WebGLUniformLocation -> GLboolean -> Float32Array -> WebGLEff eff Unit
foreign import glUniformMatrix4fv :: forall eff. WebGLContext -> WebGLUniformLocation -> GLboolean -> Float32Array -> WebGLEff eff Unit

foreign import glUniform1ui :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> WebGLEff eff Unit
foreign import glUniform2ui :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> WebGLEff eff Unit
foreign import glUniform3ui :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> Int -> WebGLEff eff Unit
foreign import glUniform4ui :: forall eff. WebGLContext -> WebGLUniformLocation -> Int -> Int -> Int -> Int -> WebGLEff eff Unit

foreign import glUniform1uiv :: forall eff. WebGLContext -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit
foreign import glUniform2uiv :: forall eff. WebGLContext -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit
foreign import glUniform3uiv :: forall eff. WebGLContext -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit
foreign import glUniform4uiv :: forall eff. WebGLContext -> WebGLUniformLocation -> Uint32Array -> WebGLEff eff Unit

foreign import glDrawArrays :: forall eff. WebGLContext -> GLenum -> GLint -> GLsizei -> WebGLEff eff Unit
foreign import glDrawElements :: forall eff. WebGLContext -> GLenum -> GLsizei -> GLenum -> GLintptr -> WebGLEff eff Unit

foreign import glClearColor :: forall eff. WebGLContext -> Number -> Number -> Number -> Number -> WebGLEff eff Unit
foreign import glClear :: forall eff. WebGLContext -> GLbitfield -> WebGLEff eff Unit

foreign import glGetExtension :: forall eff a. WebGLContext -> String -> WebGLEff eff a


foreign import glCreateTexture :: forall eff. WebGLContext -> WebGLEff eff WebGLTexture
foreign import glBindTexture :: forall eff. WebGLContext -> GLenum -> WebGLTexture -> WebGLEff eff Unit
foreign import glTexParameteri :: forall eff. WebGLContext -> GLenum -> GLenum -> Int -> WebGLEff eff Unit
foreign import glTexParameterf :: forall eff. WebGLContext -> GLenum -> GLenum -> Number -> WebGLEff eff Unit
foreign import glGenerateMipmap :: forall eff. WebGLContext -> GLenum -> WebGLEff eff Unit
foreign import glActiveTexture :: forall eff. WebGLContext -> GLenum -> WebGLEff eff Unit
foreign import glTexImage2D :: forall eff s. 
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
foreign import glTexImage2D2 :: forall eff. 
                              WebGLContext -> 
                              GLenum -> 
                              GLint -> 
                              GLenum ->
                              Int -> 
                              Int -> 
                              GLenum -> 
                              GLenum -> 
                              WebGLEff eff Unit


foreign import glCreateFramebuffer :: forall eff. WebGLContext -> WebGLEff eff WebGLFramebuffer
foreign import glDeleteFramebuffer :: forall eff. WebGLContext -> WebGLFramebuffer -> WebGLEff eff Unit
foreign import glBindFramebuffer :: forall eff. WebGLContext -> GLenum -> WebGLFramebuffer -> WebGLEff eff Unit
foreign import glFramebufferTexture2D :: forall eff. WebGLContext -> GLenum -> GLenum -> GLenum -> WebGLTexture -> GLint -> WebGLEff eff Unit
foreign import glFramebufferRenderbuffer :: forall eff. WebGLContext -> GLenum -> GLenum -> GLenum -> WebGLRenderbuffer -> WebGLEff eff Unit

foreign import glCreateRenderbuffer :: forall eff. WebGLContext -> WebGLEff eff WebGLRenderbuffer
foreign import glBindRenderbuffer :: forall eff. WebGLContext -> GLenum -> WebGLRenderbuffer -> WebGLEff eff Unit
foreign import glRenderbufferStorage :: forall eff. WebGLContext -> GLenum -> GLenum -> GLsizei -> GLsizei -> WebGLEff eff Unit
foreign import glDeleteRenderbuffer :: forall eff. WebGLContext -> WebGLRenderbuffer -> WebGLEff eff Unit
