module PureGL.Renderer.Internal.Program where

import Prelude
import PureGL.WebGL 
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (zip)
import Data.Dynamic (Dynamic, toDynamic)
import Data.Either (Either(..))
import Data.Foreign (readBoolean, readInt)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable, lookup)
import Data.Traversable (traverse)
import PureGL.Context (Context)
import PureGL.Data.TypedArrays (toTypedArray)
import PureGL.Renderer.GLConstant (getValue)
import PureGL.Renderer.Program (ProgramParamQuery(..), ProgramParamResponse(..), ShaderParamQuery(..), ShaderParamResponse(..), ShaderType(..), Uniform(..), uniformName)
import PureGL.Renderer.RenderState (RenderError(..))
import PureGL.WebGL.Types (WEBGL, WebGLProgram, WebGLShader, WebGLUniformLocation, GLint)

getShaderParameter' :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                                     MonadAsk Context m => 
                                     MonadThrow Dynamic  m =>
                                     WebGLShader -> 
                                     ShaderParamQuery -> 
                                     m ShaderParamResponse
getShaderParameter' s q = do
  case q of
    DeleteStatusQuery -> do
      res <- runExcept <<< readBoolean <$>  getShaderParameter s (getValue q)
      case res of
          Left _ -> throwError $ toDynamic GetShaderParameterError
          Right v -> pure $ DeleteStatus v
    CompileStatusQuery -> do
      res <- runExcept <<< readBoolean <$>  getShaderParameter s (getValue q)
      case res of
          Left _ -> throwError $ toDynamic GetShaderParameterError
          Right v -> pure $ CompileStatus v
    TypeOfShaderQuery -> do
      res <- runExcept <<< readInt <$>  getShaderParameter s (getValue q)
      case res of
          Left _ -> throwError $ toDynamic GetShaderParameterError
          Right v -> pure $ TypeOfShader v


getProgramParameter' :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                                      MonadAsk Context m => 
                                      MonadThrow Dynamic  m => 
                                      WebGLProgram -> 
                                      ProgramParamQuery -> 
                                      m ProgramParamResponse
getProgramParameter' p q = do
  res <- getProgramParameter p (getValue q)
  case q of 
    ProgramDeleteStatusQuery -> 
      case runExcept <<< readBoolean $ res of
        Left _ -> throwError $ toDynamic GetProgramParameterError
        Right v -> pure $ ProgramDeleteStatus v
    LinkStatusQuery -> 
      case runExcept <<< readBoolean $ res of
        Left _ -> throwError $ toDynamic GetProgramParameterError
        Right v -> pure $ LinkStatus v

buildShader :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                             MonadAsk Context m => 
                             MonadThrow Dynamic  m => 
                             String -> 
                             ShaderType -> 
                             m WebGLShader
buildShader src t = do
  shader <- createShader (getValue t)
  shaderSource shader src
  compileShader shader
  status <- getShaderParameter' shader CompileStatusQuery
  case status of
    (CompileStatus true) -> pure $ shader
    (CompileStatus false) -> do
      log <- getShaderInfoLog shader
      throwError $ toDynamic $ ShaderCompileError t log
    otherwise -> throwError $ toDynamic $ ShaderCompileError t "getShaderParameter error."

buildProgram :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                              MonadAsk Context m => 
                              MonadThrow Dynamic m => 
                              String -> 
                              String -> 
                              m WebGLProgram
buildProgram vs fs = do
  vertexShader <- buildShader vs VertexShader
  fragmentShader <- buildShader fs FragmentShader
  program <- createProgram
  attachShader program vertexShader
  attachShader program fragmentShader
  linkProgram program
  status <- getProgramParameter' program LinkStatusQuery
  case status of
    (LinkStatus true) -> pure $ program
    (LinkStatus false) -> do
      log <- getProgramInfoLog program
      throwError $ toDynamic $ ProgramLinkError log
    otherwise -> throwError $ toDynamic (ProgramLinkError "getProgramParameter error.")

getProgramUniformLocation :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                                           MonadAsk Context m => 
                                           MonadThrow Dynamic  m =>  
                                           WebGLProgram -> 
                                           Uniform -> 
                                           m WebGLUniformLocation
getProgramUniformLocation p u = do
  locationM <- getUniformLocation p (uniformName u)
  case locationM of
    Nothing -> throwError $ toDynamic $ UniformNotFound $ "Uniform '" <> (uniformName u) <> "not found."
    Just loc -> pure $ loc

getProgramUniformLocationsMap :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                                               MonadAsk Context m => 
                                               MonadThrow Dynamic  m => 
                                               WebGLProgram ->
                                               Array Uniform ->
                                               m (StrMap WebGLUniformLocation)
getProgramUniformLocationsMap p us = do
  locs <- getProgramUniformLocations p us
  pure $ fromFoldable $ zip (uniformName <$> us) locs

getProgramUniformLocations :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                                      MonadAsk Context m => 
                                      MonadThrow Dynamic  m =>   
                                      WebGLProgram -> 
                                      Array Uniform -> 
                                      m (Array WebGLUniformLocation)
getProgramUniformLocations p us = do
  traverse (getProgramUniformLocation p) us

getProgramAttribLocation :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                                      MonadAsk Context m => 
                                      MonadThrow Dynamic  m => 
                                      WebGLProgram -> 
                                      String ->
                                      m GLint
getProgramAttribLocation p a = do
  location <- getAttribLocation p a
  case location >= 0 of
    true -> pure $  location
    false -> throwError $ toDynamic $ AttributeNotFount $ "Counld not fint attribute '" <> a <> "' in the shader program."

getProgramAttribLocations :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                                      MonadAsk Context m => 
                                      MonadThrow Dynamic  m => 
                                      WebGLProgram -> 
                                      Array String -> 
                                      m (Array GLint)
getProgramAttribLocations p as = do
  traverse (getProgramAttribLocation p) as

getProgramAttribLocationsMap :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                                              MonadAsk Context m => 
                                              MonadThrow Dynamic  m =>  
                                              WebGLProgram -> 
                                              Array String -> 
                                              m (StrMap GLint)
getProgramAttribLocationsMap p as = do
  locs <- getProgramAttribLocations p as
  pure $ fromFoldable $ zip as locs

setUniform :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                            MonadAsk Context m => 
                            MonadThrow Dynamic  m =>  
                            WebGLUniformLocation -> 
                            Uniform -> 
                            m Unit
setUniform loc (UFloat _ v) = uniform1f loc v
setUniform loc (UVec2 _ v) = uniform2fv loc (toTypedArray v)
setUniform loc (UVec3 _ v) = uniform3fv loc (toTypedArray v)
setUniform loc (UVec4 _ v) = uniform4fv loc (toTypedArray v)
setUniform loc (UFVec2 _ v) = uniform2fv loc (toTypedArray v)
setUniform loc (UFVec3 _ v) = uniform3fv loc (toTypedArray v)
setUniform loc (UFVec4 _ v) = uniform4fv loc (toTypedArray v)
setUniform loc (UMat2 _ v) = uniform2fv loc (toTypedArray v)
setUniform loc (UMat3 _ v) = uniform3fv loc (toTypedArray v)
setUniform loc (UMat4 _ v) = uniform4fv loc (toTypedArray v)
setUniform loc (UFMat2 _ v) = uniform2fv loc (toTypedArray v)
setUniform loc (UFMat3 _ v) = uniform3fv loc (toTypedArray v)
setUniform loc (UFMat4 _ v) = uniform4fv loc (toTypedArray v)
setUniform loc (USampler2D _ v) = uniform1i loc v

setUniform' :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                             MonadAsk Context m => 
                             MonadThrow Dynamic  m =>  
                             StrMap WebGLUniformLocation -> 
                             Uniform -> 
                             m Unit
setUniform' locs u = 
  case lookup (uniformName u) locs of
    Nothing -> throwError $ toDynamic $ UniformNotFound (uniformName u)
    Just loc -> setUniform loc u

setUniformsMap :: forall eff m. MonadEff (webgl :: WEBGL | eff) m => 
                                      MonadAsk Context m => 
                                      MonadThrow Dynamic  m => 
                                      StrMap WebGLUniformLocation -> 
                                      Array Uniform -> 
                                      m Unit
setUniformsMap locs us = do
  _ <- traverse (setUniform' locs) us
  pure unit
