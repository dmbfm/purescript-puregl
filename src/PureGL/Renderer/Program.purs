module PureGL.Renderer.Program where

import Prelude

import Data.Lens (Lens', lens, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong (class Strong)
import Data.StrMap (StrMap)
import Data.Symbol (SProxy(..))
import PureGL.Math.Matrix (Matrix2, Matrix3, Matrix4)
import PureGL.Math.Vector (Vector2, Vector3, Vector4)
import PureGL.WebGL.Types (GLenum, GLint, WebGLProgram, WebGLUniformLocation, GLboolean)

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
  | UMat2 String Matrix2
  | UMat3 String Matrix3
  | UMat4 String Matrix4
  | USampler2D String Int

uniformName :: Uniform -> String
uniformName (UFloat n _) = n
uniformName (UVec2 n _)  = n
uniformName (UVec3 n _)  = n
uniformName (UVec4 n _)  = n
uniformName (UMat2 n _)  = n
uniformName (UMat3 n _)  = n
uniformName (UMat4 n _)  = n
uniformName (USampler2D n _) = n

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


-- lenses
_Program :: Lens' Program _
_Program = lens (\(Program r) -> r) (\_ -> Program)

_uniforms :: Lens' Program (Array Uniform)
_uniforms = _Program <<< prop (SProxy :: SProxy "uniforms")

_LoadedProgram :: Lens' LoadedProgram _
_LoadedProgram = lens (\(LoadedProgram r) -> r) (\_ -> LoadedProgram)

_program :: forall r. Lens' LoadedProgram WebGLProgram
_program = _LoadedProgram <<< prop (SProxy :: SProxy "program")

_uniformLocations :: Lens' LoadedProgram (StrMap WebGLUniformLocation)
_uniformLocations = _LoadedProgram <<< prop (SProxy :: SProxy "uniformLocations")
