module PureGL.Program.Internal where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (zip)
import Data.Either (Either(..))
import Data.Foreign (readBoolean, readInt)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable)
import Data.Traversable (sequence, traverse)
import PureGL.GLConstant (getValue)
import PureGL.Program (ProgramParamQuery(..), ProgramParamResponse(..), ShaderParamQuery(..), ShaderParamResponse(..), ShaderType(..), Uniform, uniformName)
import PureGL.RenderState (RenderT)
import PureGL.Utils.Misc (eitherFromMaybe)
import PureGL.WebGL (attachShader, compileShader, createProgram, createShader, getAttribLocation, getProgramInfoLog, getProgramParameter, getShaderInfoLog, getShaderParameter, getUniformLocation, linkProgram, shaderSource)
import PureGL.WebGL.Types (WebGLProgram, WebGLShader, WebGLUniformLocation, GLint)

getShaderParameter' :: forall eff. WebGLShader -> ShaderParamQuery -> RenderT eff (Either String ShaderParamResponse)
getShaderParameter' s q = do
  case q of
    DeleteStatusQuery -> do
      res <- runExcept <<< readBoolean <$>  getShaderParameter s (getValue q)
      case res of
          Left _ -> pure $ Left "Error reading 'getShaderParameter' response"
          Right v -> pure $ Right $ DeleteStatus v
    CompileStatusQuery -> do
      res <- runExcept <<< readBoolean <$>  getShaderParameter s (getValue q)
      case res of
          Left _ -> pure $ Left "Error reading 'getShaderParameter' response"
          Right v -> pure $ Right $ CompileStatus v
    TypeOfShaderQuery -> do
      res <- runExcept <<< readInt <$>  getShaderParameter s (getValue q)
      case res of
          Left _ -> pure $ Left "Error reading 'getShaderParameter' response"
          Right v -> pure $ Right $ TypeOfShader v

getProgramParameter' :: forall eff. WebGLProgram -> ProgramParamQuery -> RenderT eff (Either String ProgramParamResponse)
getProgramParameter' p q = do
  res <- getProgramParameter p (getValue q)
  case q of 
    ProgramDeleteStatusQuery -> 
      case runExcept <<< readBoolean $ res of
        Left _ -> pure $ Left $ "Error reading 'getProgramParameter' response"
        Right v -> pure $ Right $ ProgramDeleteStatus v
    LinkStatusQuery -> 
      case runExcept <<< readBoolean $ res of
        Left _ -> pure $ Left $ "Error reading 'getProgramParameter' response"
        Right v -> pure $ Right $ LinkStatus v

buildShader :: forall eff. String -> ShaderType -> RenderT eff (Either String WebGLShader)
buildShader src t = do
  shader <- createShader (getValue t)
  shaderSource shader src
  compileShader shader
  p <- getShaderParameter' shader CompileStatusQuery
  case p of
    Right status -> 
      case status of 
        (CompileStatus true) -> pure $ Right shader
        (CompileStatus false) -> Left <$> getShaderInfoLog shader
        otherwise -> pure $ Left "Should not happen"
    Left e -> pure $ Left e

buildProgram :: forall eff. String -> String -> RenderT eff (Either String WebGLProgram)
buildProgram vs fs = do
  vertexShaderE <- buildShader vs VertexShader
  case vertexShaderE of
    Left e -> pure $ Left e
    Right vertexShader -> do
      fragmentShaderE <- buildShader fs FragmentShader
      case fragmentShaderE of
        Left e -> pure $ Left e
        Right fragmentShader -> do
          program <- createProgram
          attachShader program vertexShader
          attachShader program fragmentShader
          linkProgram program
          statusE <- getProgramParameter' program LinkStatusQuery
          case statusE of
            Right status -> 
              case status of
                (LinkStatus true) -> pure $ Right program
                (LinkStatus false) -> Left <$> getProgramInfoLog program
                otherwise -> pure $ Left "Should not happen"
            Left e -> pure $ Left e


getProgramUniformLocation :: forall eff. WebGLProgram -> Uniform -> RenderT eff (Either String WebGLUniformLocation)
getProgramUniformLocation p u = do
  locationM <- getUniformLocation p (uniformName u)
  case locationM of
    Nothing -> pure $ Left $ "Uniform '" <> (uniformName u) <> "not found."
    Just loc -> pure $ Right loc

getProgramUniformLocationsMap :: forall eff. WebGLProgram -> Array Uniform -> RenderT eff (Either String (StrMap WebGLUniformLocation))
getProgramUniformLocationsMap p us = do
  locsE <- getProgramUniformLocations p us
  case locsE of 
    Left e -> pure $ Left e
    Right locs -> pure $ Right $ fromFoldable $ zip (uniformName <$> us) locs

getProgramUniformLocations :: forall eff.  WebGLProgram -> Array Uniform -> RenderT eff (Either String  (Array WebGLUniformLocation))
getProgramUniformLocations p us = do
  sequence <$> traverse (\u -> (eitherFromMaybe $ "Could not find uniform '" <> (uniformName u) <> "' in the program.") <$> (getUniformLocation p (uniformName u))) us


getProgramAttribLocation :: forall eff. WebGLProgram -> String -> RenderT eff (Either String GLint)
getProgramAttribLocation p a = do
  location <- getAttribLocation p a
  case location >= 0 of
    true -> pure $ Right location
    false -> pure $ Left $ "Counld not fint attribute '" <> a <> "' in the shader program."

getProgramAttribLocations :: forall eff. WebGLProgram -> (Array String) -> RenderT eff (Either String (Array GLint))
getProgramAttribLocations p as = do
  sequence <$> traverse (\a -> getProgramAttribLocation p a) as

getProgramAttribLocationsMap :: forall eff. WebGLProgram -> (Array String) -> RenderT eff (Either String (StrMap GLint))
getProgramAttribLocationsMap p as = do
  locsE <- getProgramAttribLocations p as
  case locsE of
    Left e -> pure $ Left e
    Right locs -> pure $ Right $ fromFoldable $ zip as locs
