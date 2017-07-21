module PureGL.GLConstant where


import PureGL.WebGL.Constants
import PureGL.Framebuffer (RenderbufferFormat(..))
import PureGL.Program (ProgramParamQuery(..), ShaderParamQuery(..), ShaderType(..))
import PureGL.Texture (TexelDataType(..), TextureImageFormat(..), TextureInternalFormat(..), TextureMagFilter(..), TextureMinFilter(..), TextureTarget(..), TextureWrap(..))

-- | Typeclass used to extract WebGL constants wraped
-- | in type constructors.
class GLConstant a b | a -> b where
  getValue :: a -> b


instance renderbufferFormatConstant :: GLConstant RenderbufferFormat Int where
  getValue RenderbufferDepthComponent16 = gl_DEPTH_COMPONENT16
  getValue RenderbufferRGB565 = gl_RGB565
  getValue RenderbufferRGBA4 = gl_RGBA4
  getValue RenderbufferRGB5_A1 = gl_RGB5_A1
  getValue RenderbufferStencilIndex8 = gl_STENCIL_INDEX8

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

instance textureTargetGLConstant :: GLConstant TextureTarget Int where
  getValue TextureTarget2D = gl_TEXTURE_2D
  getValue TextureTargetCubeMap = gl_TEXTURE_CUBE_MAP

instance texelDataTypeGLConstant :: GLConstant TexelDataType Int where
  getValue TexelUnsignedByte = gl_UNSIGNED_BYTE
  getValue TexelUnsignedShort565 = gl_UNSIGNED_SHORT_5_6_5
  getValue TexelUnsignedShort4444 = gl_UNSIGNED_SHORT_4_4_4_4
  getValue TexelUnsignedShort5551 = gl_UNSIGNED_SHORT_5_5_5_1

instance textureMagFilterGLConstant :: GLConstant TextureMagFilter Int where
  getValue MagLinear = gl_LINEAR
  getValue MagNearest = gl_NEAREST

instance textureMinFilterGLConstant :: GLConstant TextureMinFilter Int where
  getValue MinLinear = gl_LINEAR
  getValue MinNearest = gl_NEAREST
  getValue MinNearestMipMapNearest = gl_NEAREST_MIPMAP_NEAREST
  getValue MinLinearMipMapNearest = gl_LINEAR_MIPMAP_NEAREST
  getValue MinNearestMipMapLinear = gl_NEAREST_MIPMAP_LINEAR
  getValue MinLinearMipMapLiner = gl_LINEAR_MIPMAP_LINEAR

instance texWrapGLconstant :: GLConstant TextureWrap Int where
  getValue WrapRepeat = gl_REPEAT
  getValue WrapClampToEdge = gl_CLAMP_TO_EDGE
  getValue WrapMirroredRepeat = gl_MIRRORED_REPEAT

instance textureFormatGLConstant :: GLConstant TextureImageFormat Int where
  getValue TF_RGB = gl_RGB
  getValue TF_RGBA = gl_RGBA

instance textureInternalFormatGLConstant :: GLConstant TextureInternalFormat Int where
  getValue TIF_RGB = gl_RGB
  getValue TIF_RGBA = gl_RGBA

