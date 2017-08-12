module PureGL.WebGL where
  
import Prelude 
import PureGL.WebGL.Raw
import PureGL.WebGL.Types
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import PureGL.Context (Context(..))
import PureGL.Data.TypedArrays (class BufferSource, Float32Array, Int32Array)

createBuffer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => m WebGLBuffer
createBuffer = ask >>= \(Context ctx) -> liftEff $ glCreateBuffer ctx.glContext

deleteBuffer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLBuffer -> m Unit
deleteBuffer b = ask >>= \(Context ctx) -> liftEff $ glDeleteBuffer ctx.glContext b

bindBuffer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> WebGLBuffer -> m Unit
bindBuffer t b = ask >>= \(Context ctx) -> liftEff $ glBindBuffer ctx.glContext t b

bufferData :: forall eff m src. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => BufferSource src =>  GLenum -> src -> GLenum ->  m  Unit
bufferData target src usage = ask >>= \(Context ctx) -> liftEff $ glBufferData ctx.glContext target src usage

bufferSubData ::forall eff m src. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => BufferSource src =>  GLenum -> src -> m Unit
bufferSubData target src = ask >>= \(Context ctx) -> liftEff $ glBufferSubData ctx.glContext target 0 src

createVertexArray :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => m WebGLVertexArrayObject
createVertexArray = ask >>= \(Context ctx) -> liftEff $ glCreateVertexArray ctx.glContext

bindVertexArray :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLVertexArrayObject -> m Unit
bindVertexArray vao = ask >>= \(Context ctx) -> liftEff $ glBindVertexArray ctx.glContext vao

enableVertexAttribArray :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLuint -> m Unit
enableVertexAttribArray idx = ask >>= \(Context ctx) -> liftEff $ glEnableVertexAttribArray ctx.glContext idx

vertexAttribPointer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> m Unit
vertexAttribPointer idx size t norm stride offset = 
  ask >>= \(Context ctx) -> liftEff $ glVertexAttribPointer ctx.glContext idx size t norm stride offset

createShader :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> m WebGLShader
createShader t = ask >>= \(Context ctx) -> liftEff $ glCreateShader ctx.glContext t

shaderSource :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLShader -> String -> m Unit
shaderSource s src = ask >>= \(Context ctx) -> liftEff $ glShaderSource ctx.glContext s src

compileShader :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLShader -> m Unit
compileShader s = ask >>= \(Context ctx) -> liftEff $ glCompileShader ctx.glContext s

getShaderParameter :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLShader -> GLenum -> m Foreign
getShaderParameter s p = ask >>= \(Context ctx) -> liftEff $ glGetShaderParameter ctx.glContext s p 

getShaderInfoLog :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLShader -> m String 
getShaderInfoLog s = ask >>= \(Context ctx) -> liftEff $ glGetShaderInfoLog ctx.glContext s 

createProgram :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => m WebGLProgram
createProgram = ask >>= \(Context ctx) -> liftEff $ glCreateProgram ctx.glContext

attachShader :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLProgram -> WebGLShader -> m Unit
attachShader p s = ask >>= \(Context ctx) -> liftEff $ glAttachShader ctx.glContext p s

linkProgram :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLProgram -> m Unit
linkProgram p = ask >>= \(Context ctx) -> liftEff $ glLinkProgram ctx.glContext p

getProgramParameter :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLProgram -> GLenum -> m Foreign
getProgramParameter s p = ask >>= \(Context ctx) -> liftEff $ glGetProgramParameter ctx.glContext s p 

getProgramInfoLog :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLProgram -> m String 
getProgramInfoLog s = ask >>= \(Context ctx) -> liftEff $ glGetProgramInfoLog ctx.glContext s 

getUniformLocation :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLProgram -> String -> m (Maybe WebGLUniformLocation)
getUniformLocation p n = ask >>= \(Context ctx) -> liftEff $ toMaybe <$> glGetUniformLocation ctx.glContext p n

getAttribLocation :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLProgram -> String -> m GLint
getAttribLocation p n = ask >>= \(Context ctx) -> liftEff $ glGetAttribLocation ctx.glContext p n

useProgram :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLProgram -> m Unit
useProgram p = ask >>= \(Context ctx) -> liftEff $ glUseProgram ctx.glContext p

drawArrays :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> GLint -> GLsizei -> m Unit
drawArrays mode first count = ask >>= \(Context ctx) -> liftEff $ glDrawArrays ctx.glContext mode first count

clearColor :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => Number -> Number -> Number -> Number -> m Unit
clearColor r g b a = ask >>= \(Context ctx) -> liftEff $ glClearColor ctx.glContext r g b a

clear :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLbitfield -> m Unit
clear b = ask >>= \(Context ctx) -> liftEff $ glClear ctx.glContext b

getExtension :: forall eff m a. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => String -> m a
getExtension name = ask >>= \(Context ctx) -> liftEff $ glGetExtension ctx.glContext name

createTexture :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => m WebGLTexture
createTexture = ask >>= \(Context ctx) -> liftEff $ glCreateTexture ctx.glContext

bindTexture :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> WebGLTexture -> m Unit
bindTexture target texture = ask >>= \(Context ctx) -> liftEff $ glBindTexture ctx.glContext target texture

texParameteri :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> GLenum -> Int -> m Unit
texParameteri target pname value = ask >>= \(Context ctx) -> liftEff $ glTexParameteri ctx.glContext target pname value

texParameterf :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> GLenum -> Number -> m Unit
texParameterf target pname value = ask >>= \(Context ctx) -> liftEff $ glTexParameterf ctx.glContext target pname value

generateMipmap :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> m Unit
generateMipmap target = ask >>= \(Context ctx) -> liftEff $ glGenerateMipmap ctx.glContext target

activeTexture :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> m Unit
activeTexture target = ask >>= \(Context ctx) -> liftEff $ glActiveTexture ctx.glContext target

texImage2D :: forall eff m s. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => TextureSource s => 
                            GLenum -> 
                            GLint -> 
                            GLenum ->
                            GLenum -> 
                            GLenum -> 
                            s ->
                            m Unit
texImage2D tgt lvl intf f tp pxs = 
  ask >>= \(Context ctx) -> liftEff $ glTexImage2D ctx.glContext tgt lvl intf f tp pxs

texImage2D' :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m =>    GLenum -> 
                              GLint -> 
                              GLenum ->
                              Int -> 
                              Int -> 
                              GLenum -> 
                              GLenum -> 
                              m Unit
texImage2D' tgt lvl intf w h f tp = 
  ask >>= \(Context ctx) -> liftEff $ glTexImage2D2 ctx.glContext tgt lvl intf w h f tp 

createFramebuffer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => m WebGLFramebuffer
createFramebuffer = ask >>= \(Context ctx) -> liftEff $ glCreateFramebuffer ctx.glContext

bindFramebuffer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> WebGLFramebuffer -> m Unit
bindFramebuffer t f = ask >>= \(Context ctx) -> liftEff $ glBindFramebuffer ctx.glContext t f

deleteFramebuffer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLFramebuffer -> m Unit
deleteFramebuffer f = ask >>= \(Context ctx) -> liftEff $ glDeleteFramebuffer ctx.glContext f

framebufferTexture2D :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> GLenum -> GLenum -> WebGLTexture -> GLint -> m Unit
framebufferTexture2D target att textarget texture level = 
  ask >>= \(Context ctx) -> liftEff $ glFramebufferTexture2D ctx.glContext target att textarget texture level

framebufferRenderbuffer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> GLenum -> GLenum -> WebGLRenderbuffer -> m Unit
framebufferRenderbuffer target att rbtarget rb = 
  ask >>= \(Context ctx) -> liftEff $ glFramebufferRenderbuffer ctx.glContext target att rbtarget rb


createRenderbuffer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => m WebGLRenderbuffer
createRenderbuffer = ask >>= \(Context ctx) -> liftEff $ glCreateRenderbuffer ctx.glContext

bindRenderbuffer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> WebGLRenderbuffer -> m Unit
bindRenderbuffer t f = ask >>= \(Context ctx) -> liftEff $ glBindRenderbuffer ctx.glContext t f

deleteRenderbuffer :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLRenderbuffer -> m Unit
deleteRenderbuffer f = ask >>= \(Context ctx) -> liftEff $ glDeleteRenderbuffer ctx.glContext f

renderbufferStorage :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => GLenum -> GLenum -> GLsizei -> GLsizei -> m Unit
renderbufferStorage target format w h = 
  ask >>= \(Context ctx) -> liftEff $ glRenderbufferStorage ctx.glContext target format w h
  
uniform1f :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Number -> m Unit
uniform1f loc v = ask >>= \(Context ctx) -> liftEff $ glUniform1f ctx.glContext loc v

uniform2f :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Number -> Number -> m Unit
uniform2f loc v1 v2 = ask >>= \(Context ctx) -> liftEff $ glUniform2f ctx.glContext loc v1 v2

uniform3f :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Number -> Number -> Number -> m Unit
uniform3f loc v1 v2 v3 = ask >>= \(Context ctx) -> liftEff $ glUniform3f ctx.glContext loc v1 v2 v3

uniform4f :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Number -> Number -> Number -> Number -> m Unit
uniform4f loc v1 v2 v3 v4 = ask >>= \(Context ctx) -> liftEff $ glUniform4f ctx.glContext loc v1 v2 v3 v4

uniform1fv :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Float32Array -> m Unit
uniform1fv loc v = ask >>= \(Context ctx) -> liftEff $ glUniform1fv ctx.glContext loc v

uniform2fv :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Float32Array -> m Unit
uniform2fv loc v = ask >>= \(Context ctx) -> liftEff $ glUniform2fv ctx.glContext loc v

uniform3fv :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Float32Array -> m Unit
uniform3fv loc v = ask >>= \(Context ctx) -> liftEff $ glUniform3fv ctx.glContext loc v

uniform4fv :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Float32Array -> m Unit
uniform4fv loc v = ask >>= \(Context ctx) -> liftEff $ glUniform4fv ctx.glContext loc v

uniform1i :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Int -> m Unit
uniform1i loc v = ask >>= \(Context ctx) -> liftEff $ glUniform1i ctx.glContext loc v

uniform2i :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Int -> Int -> m Unit
uniform2i loc v1 v2 = ask >>= \(Context ctx) -> liftEff $ glUniform2i ctx.glContext loc v1 v2

uniform3i :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Int -> Int -> Int -> m Unit
uniform3i loc v1 v2 v3 = ask >>= \(Context ctx) -> liftEff $ glUniform3i ctx.glContext loc v1 v2 v3

uniform4i :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Int -> Int -> Int -> Int -> m Unit
uniform4i loc v1 v2 v3 v4 = ask >>= \(Context ctx) -> liftEff $ glUniform4i ctx.glContext loc v1 v2 v3 v4

uniform1iv :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Int32Array -> m Unit
uniform1iv loc v = ask >>= \(Context ctx) -> liftEff $ glUniform1iv ctx.glContext loc v

uniform2iv :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Int32Array -> m Unit
uniform2iv loc v = ask >>= \(Context ctx) -> liftEff $ glUniform2iv ctx.glContext loc v

uniform3iv :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Int32Array -> m Unit
uniform3iv loc v = ask >>= \(Context ctx) -> liftEff $ glUniform3iv ctx.glContext loc v

uniform4iv :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => MonadAsk Context m => WebGLUniformLocation -> Int32Array -> m Unit
uniform4iv loc v = ask >>= \(Context ctx) -> liftEff $ glUniform4iv ctx.glContext loc v
