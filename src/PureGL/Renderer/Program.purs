module PureGL.Renderer.Program where

import Data.StrMap (StrMap)
import PureGL.Math.Matrix (Matrix2, Matrix3, Matrix4)
import PureGL.Math.Matrix.Fast (FMatrix2, FMatrix3, FMatrix4)
import PureGL.Math.Vector (Vector2, Vector3, Vector4)
import PureGL.Math.Vector.Fast (FVector2, FVector3, FVector4)
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
  | UFVec2 String FVector2
  | UFVec3 String FVector3
  | UFVec4 String FVector4
  | UMat2 String Matrix2
  | UMat3 String Matrix3
  | UMat4 String Matrix4
  | UFMat2 String FMatrix2
  | UFMat3 String FMatrix3
  | UFMat4 String FMatrix4
  | USampler2D String Int

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
