module PureGL.Program where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (zip)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Foreign (readBoolean, readInt)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, fromFoldable)
import Data.Traversable (scanl, sequence, traverse)
import PureGL.Context (ContextR)
import PureGL.Math.Matrix (Matrix2, Matrix3, Matrix4)
import PureGL.Math.Matrix.Fast (FMatrix2, FMatrix3, FMatrix4)
import PureGL.Math.Vector (Vector2(..), Vector3(..), Vector4(..))
import PureGL.Math.Vector.Fast (FVector2, FVector3, FVector4)
import PureGL.Utils.Misc (eitherFromMaybe)
import PureGL.WebGL (class GLConstant, attachShader, compileShader, createProgram, createShader, getAttribLocation, getProgramInfoLog, getProgramParameter, getShaderInfoLog, getShaderParameter, getUniformLocation, getValue, linkProgram, shaderSource)
import PureGL.WebGL.Constants (gl_COMPILE_STATUS, gl_DELETE_STATUS, gl_FRAGMENT_SHADER, gl_LINK_STATUS, gl_SHADER_TYPE, gl_VERTEX_SHADER)
import PureGL.WebGL.Types (GLenum, GLint, WebGLProgram, WebGLShader, WebGLUniformLocation, GLboolean)

newtype Program = Program { vertexShaderSource :: String
                          , fragmentShaderSource :: String
                          , uniforms :: Array Uniform
                          , attributes :: Array String
                          }

newtype LoadedProgram = LoadedProgram { program :: WebGLProgram
                                      , uniformLocations :: StrMap WebGLUniformLocation
                                      , attributeLocations :: StrMap GLint
                                      }

data Uniform = 
    UFloat String Number
  | UVec2 String Vector2
  | UVec3 String Vector3
  | UVec4 String Vector4
  | UFVec2 String FVector2
  | UFVec3 String FVector3
  | UFVec4 String FVector4
  | UMat2 String Matrix2
  | UMat3 String Matrix3
  | UMat4 String Matrix4
  | UFMat2 String FMatrix2
  | UFMat3 String FMatrix3
  | UFMat4 String FMatrix4

uniformName :: Uniform -> String
uniformName (UFloat n _) = n
uniformName (UVec2 n _)  = n
uniformName (UVec3 n _)  = n
uniformName (UVec4 n _)  = n
uniformName (UMat2 n _)  = n
uniformName (UMat3 n _)  = n
uniformName (UMat4 n _)  = n
uniformName (UFVec2 n _ ) = n
uniformName (UFVec3 n _ ) = n
uniformName (UFVec4 n _ ) = n
uniformName (UFMat2 n _)  = n
uniformName (UFMat3 n _)  = n
uniformName (UFMat4 n _)  = n

data ShaderType = VertexShader | FragmentShader

data ShaderParamQuery = DeleteStatusQuery | CompileStatusQuery | TypeOfShaderQuery

data ProgramParamQuery = 
    ProgramDeleteStatusQuery
  | LinkStatusQuery
  -- | ValidateStatusQuery
  -- | AttachedShadersQuery
  -- | ActiveAttributesQuery
  -- | ActiveUniformsQuery

data ShaderParamResponse = 
    DeleteStatus GLboolean
  | CompileStatus GLboolean
  | TypeOfShader GLenum

data ProgramParamResponse = 
    ProgramDeleteStatus GLboolean
  | LinkStatus GLboolean
  -- | ValidateStatus GLboolean
  -- | AttachedShaders GLint 
  -- | ActiveAttributes GLint
  -- | ActiveUniforms GLint

instance shaderTypeGLConstant :: GLConstant ShaderType Int where
  getValue VertexShader = gl_VERTEX_SHADER
  getValue FragmentShader = gl_FRAGMENT_SHADER

instance shaderParamQueryGLConstant :: GLConstant ShaderParamQuery Int where
  getValue DeleteStatusQuery = gl_DELETE_STATUS
  getValue CompileStatusQuery = gl_COMPILE_STATUS
  getValue TypeOfShaderQuery = gl_SHADER_TYPE

instance programParamQueryGLConstant :: GLConstant ProgramParamQuery Int where
  getValue ProgramDeleteStatusQuery = gl_DELETE_STATUS
  getValue LinkStatusQuery = gl_LINK_STATUS
  -- getValue ValidateStatusQuery = gl_VAL
  -- | AttachedShadersQuery
  -- | ActiveAttributesQuery
  -- | ActiveUniformsQuery

getShaderParameter' :: forall eff. WebGLShader -> ShaderParamQuery -> ContextR eff (Either String ShaderParamResponse)
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

getProgramParameter' :: forall eff. WebGLProgram -> ProgramParamQuery -> ContextR eff (Either String ProgramParamResponse)
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

buildShader :: forall eff. String -> ShaderType -> ContextR eff (Either String WebGLShader)
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

buildProgram :: forall eff. String -> String -> ContextR eff (Either String WebGLProgram)
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


getProgramUniformLocation :: forall eff. WebGLProgram -> Uniform -> ContextR eff (Either String WebGLUniformLocation)
getProgramUniformLocation p u = do
  locationM <- getUniformLocation p (uniformName u)
  case locationM of
    Nothing -> pure $ Left $ "Uniform '" <> (uniformName u) <> "not found."
    Just loc -> pure $ Right loc

getProgramUniformLocationsMap :: forall eff. WebGLProgram -> Array Uniform -> ContextR eff (Either String (StrMap WebGLUniformLocation))
getProgramUniformLocationsMap p us = do
  locsE <- getProgramUniformLocations p us
  case locsE of 
    Left e -> pure $ Left e
    Right locs -> pure $ Right $ fromFoldable $ zip (uniformName <$> us) locs

getProgramUniformLocations :: forall eff.  WebGLProgram -> Array Uniform -> ContextR eff (Either String  (Array WebGLUniformLocation))
getProgramUniformLocations p us = do
  sequence <$> traverse (\u -> (eitherFromMaybe $ "Could not find uniform '" <> (uniformName u) <> "' in the program.") <$> (getUniformLocation p (uniformName u))) us


getProgramAttribLocation :: forall eff. WebGLProgram -> String -> ContextR eff (Either String GLint)
getProgramAttribLocation p a = do
  location <- getAttribLocation p a
  case location >= 0 of
    true -> pure $ Right location
    false -> pure $ Left $ "Counld not fint attribute '" <> a <> "' in the shader program."

getProgramAttribLocations :: forall eff. WebGLProgram -> (Array String) -> ContextR eff (Either String (Array GLint))
getProgramAttribLocations p as = do
  sequence <$> traverse (\a -> getProgramAttribLocation p a) as

getProgramAttribLocationsMap :: forall eff. WebGLProgram -> (Array String) -> ContextR eff (Either String (StrMap GLint))
getProgramAttribLocationsMap p as = do
  locsE <- getProgramAttribLocations p as
  case locsE of
    Left e -> pure $ Left e
    Right locs -> pure $ Right $ fromFoldable $ zip as locs