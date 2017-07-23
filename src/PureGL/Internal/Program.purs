module PureGL.Internal.Program where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Data.Array (zip)
import Data.Either (Either(..))
import Data.Foreign (readBoolean, readInt)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable, lookup)
import Data.Traversable (sequence, traverse)
import PureGL.Data.TypedArrays (toTypedArray)
import PureGL.GLConstant (getValue)
import PureGL.Math.Vector (toFloat32Array)
import PureGL.Program (ProgramParamQuery(..), ProgramParamResponse(..), ShaderParamQuery(..), ShaderParamResponse(..), ShaderType(..), Uniform(..), uniformName)
import PureGL.RenderState (RenderError(..), RenderT)
import PureGL.Utils.Misc (eitherFromMaybe)
import PureGL.WebGL (attachShader, compileShader, createProgram, createShader, getAttribLocation, getProgramInfoLog, getProgramParameter, getShaderInfoLog, getShaderParameter, getUniformLocation, linkProgram, shaderSource, uniform1f, uniform1i, uniform2fv, uniform3fv, uniform4fv)
import PureGL.WebGL.Types (WebGLProgram, WebGLShader, WebGLUniformLocation, GLint)

getShaderParameter' :: forall eff. WebGLShader -> ShaderParamQuery -> RenderT eff ShaderParamResponse
getShaderParameter' s q = do
  case q of
    DeleteStatusQuery -> do
      res <- runExcept <<< readBoolean <$>  getShaderParameter s (getValue q)
      case res of
          Left _ -> throwError GetShaderParameterError
          Right v -> pure $ DeleteStatus v
    CompileStatusQuery -> do
      res <- runExcept <<< readBoolean <$>  getShaderParameter s (getValue q)
      case res of
          Left _ -> throwError GetShaderParameterError
          Right v -> pure $ CompileStatus v
    TypeOfShaderQuery -> do
      res <- runExcept <<< readInt <$>  getShaderParameter s (getValue q)
      case res of
          Left _ -> throwError GetShaderParameterError
          Right v -> pure $ TypeOfShader v

getProgramParameter' :: forall eff. WebGLProgram -> ProgramParamQuery -> RenderT eff ProgramParamResponse
getProgramParameter' p q = do
  res <- getProgramParameter p (getValue q)
  case q of 
    ProgramDeleteStatusQuery -> 
      case runExcept <<< readBoolean $ res of
        Left _ -> throwError GetProgramParameterError
        Right v -> pure $ ProgramDeleteStatus v
    LinkStatusQuery -> 
      case runExcept <<< readBoolean $ res of
        Left _ -> throwError GetProgramParameterError
        Right v -> pure $ LinkStatus v

buildShader :: forall eff. String -> ShaderType -> RenderT eff WebGLShader
buildShader src t = do
  shader <- createShader (getValue t)
  shaderSource shader src
  compileShader shader
  status <- getShaderParameter' shader CompileStatusQuery
  case status of
    (CompileStatus true) -> pure $ shader
    (CompileStatus false) -> do
      log <- getShaderInfoLog shader
      throwError $ ShaderCompileError t log
    otherwise -> throwError $ ShaderCompileError t "getShaderParameter error."

buildProgram :: forall eff. String -> String -> RenderT eff WebGLProgram
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
      throwError $ ProgramLinkError log
    otherwise -> throwError $ ProgramLinkError "getProgramParameter error."

getProgramUniformLocation :: forall eff. WebGLProgram -> Uniform -> RenderT eff WebGLUniformLocation
getProgramUniformLocation p u = do
  locationM <- getUniformLocation p (uniformName u)
  case locationM of
    Nothing -> throwError $ UniformNotFound $ "Uniform '" <> (uniformName u) <> "not found."
    Just loc -> pure $ loc

getProgramUniformLocationsMap :: forall eff. WebGLProgram -> Array Uniform -> RenderT eff (StrMap WebGLUniformLocation)
getProgramUniformLocationsMap p us = do
  locs <- getProgramUniformLocations p us
  pure $ fromFoldable $ zip (uniformName <$> us) locs

getProgramUniformLocations :: forall eff.  WebGLProgram -> Array Uniform -> RenderT eff (Array WebGLUniformLocation)
getProgramUniformLocations p us = do
  traverse (getProgramUniformLocation p) us

getProgramAttribLocation :: forall eff. WebGLProgram -> String -> RenderT eff GLint
getProgramAttribLocation p a = do
  location <- getAttribLocation p a
  case location >= 0 of
    true -> pure $  location
    false -> throwError $ AttributeNotFount $ "Counld not fint attribute '" <> a <> "' in the shader program."

getProgramAttribLocations :: forall eff. WebGLProgram -> (Array String) -> RenderT eff (Array GLint)
getProgramAttribLocations p as = do
  traverse (getProgramAttribLocation p) as

getProgramAttribLocationsMap :: forall eff. WebGLProgram -> (Array String) -> RenderT eff (StrMap GLint)
getProgramAttribLocationsMap p as = do
  locs <- getProgramAttribLocations p as
  pure $ fromFoldable $ zip as locs

setUniform :: forall eff. WebGLUniformLocation -> Uniform -> RenderT eff Unit
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

setUniform' :: forall eff. StrMap WebGLUniformLocation -> Uniform -> RenderT eff Unit
setUniform' locs u = 
  case lookup (uniformName u) locs of
    Nothing -> throwError $ UniformNotFound (uniformName u)
    Just loc -> setUniform loc u

setUniformsMap :: forall eff. StrMap WebGLUniformLocation -> Array Uniform -> RenderT eff Unit
setUniformsMap locs us = do
  _ <- traverse (setUniform' locs) us
  pure unit