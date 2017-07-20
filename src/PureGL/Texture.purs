module PureGL.Texture where

import Prelude
import DOM.HTML.Types (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement)
import PureGL.Data.TypedArrays (Uint8Array)
import PureGL.WebGL (class GLConstant)
import PureGL.WebGL.Constants (gl_CLAMP_TO_EDGE, gl_LINEAR, gl_LINEAR_MIPMAP_LINEAR, gl_LINEAR_MIPMAP_NEAREST, gl_MIRRORED_REPEAT, gl_NEAREST, gl_NEAREST_MIPMAP_LINEAR, gl_NEAREST_MIPMAP_NEAREST, gl_REPEAT, gl_RGB, gl_RGBA, gl_TEXTURE_2D, gl_TEXTURE_CUBE_MAP, gl_UNSIGNED_BYTE, gl_UNSIGNED_SHORT_4_4_4_4, gl_UNSIGNED_SHORT_5_5_5_1, gl_UNSIGNED_SHORT_5_6_5)
import PureGL.WebGL.Types (ImageData, WebGLTexture)

newtype TextureSampler = TextureSampler { magFilter :: TextureMagFilter
                                        , minFilter :: TextureMinFilter
                                        , wrapS :: TextureWrap
                                        , wrapT :: TextureWrap
                                        , maxAnisotropy :: TextureMaxAnisotropy
                                        }

newtype TextureFormat = TextureFormat { format :: TextureImageFormat
                                      , internalFormat :: TextureInternalFormat
                                      , texelDataType :: TexelDataType 
                                      }

type TextureDimension = { width :: Int, height :: Int}

newtype Texture = Texture { pixels :: TexturePixels
                          , sampler :: TextureSampler 
                          , format :: TextureFormat
                          , textureTarget :: TextureTarget
                          }

newtype RenderTexture = RenderTexture { size :: TextureDimension 
                                      , sampler :: TextureSampler 
                                      , format :: TextureFormat
                                      , textureTarget :: TextureTarget
                                      } 
                
newtype LoadedTexture = LoadedTexture { texture :: WebGLTexture 
                                      , textureTarget :: TextureTarget
                                      }

mkTextureSampler :: TextureMagFilter -> 
                    TextureMinFilter ->
                    TextureWrap -> 
                    TextureWrap -> 
                    TextureMaxAnisotropy -> 
                    TextureSampler
mkTextureSampler mag min ws wt ma = TextureSampler { magFilter: mag 
                                                   , minFilter: min
                                                   , wrapS: ws
                                                   , wrapT: wt
                                                   , maxAnisotropy: ma
                                                   }

mkTextureSampler' :: TextureMaxAnisotropy -> TextureSampler
mkTextureSampler' a = mkTextureSampler MagLinear MinLinearMipMapLiner WrapRepeat WrapRepeat a

texSampler0 :: TextureSampler
texSampler0 = mkTextureSampler MagLinear MinLinearMipMapLiner WrapRepeat WrapRepeat 1.0

mkTextureFormat :: TextureImageFormat -> 
                   TextureInternalFormat -> 
                   TexelDataType ->
                   TextureFormat 
mkTextureFormat  f f' d = TextureFormat { format: f
                                        , internalFormat: f'
                                        , texelDataType: d
                                        }

texFormatRGBAU :: TextureFormat
texFormatRGBAU = mkTextureFormat TF_RGBA TIF_RGBA TexelUnsignedByte

mkTexture :: TexturePixels -> TextureSampler -> TextureFormat -> TextureTarget -> Texture
mkTexture p s f t = Texture { pixels: p 
                            , sampler: s
                            , format: f
                            , textureTarget: t
                            }

mkRenderTexture :: TextureDimension -> TextureSampler -> TextureFormat -> TextureTarget -> RenderTexture
mkRenderTexture d s f t = RenderTexture { size: d
                                        , sampler: s
                                        , format: f
                                        , textureTarget: t
                                        }

data TextureObjectType = TextureObjectTexture | TextureObjectRenderTexture

data TextureTarget = 
    TextureTarget2D
  | TextureTargetCubeMap

data TexturePixels = 
    HTMLImagePixels HTMLImageElement
  | HTMLVideoPixels HTMLVideoElement
  | HTMLCanvasPixels HTMLCanvasElement
  | ImageDataPixels ImageData
  | Uint8ArrayPixels Uint8Array

data TextureMagFilter = 
    MagLinear 
  | MagNearest

data TextureMinFilter = 
    MinLinear 
  | MinNearest 
  | MinNearestMipMapNearest
  | MinLinearMipMapNearest
  | MinNearestMipMapLinear
  | MinLinearMipMapLiner

data TextureWrap = 
    WrapRepeat
  | WrapClampToEdge
  | WrapMirroredRepeat

type TextureMaxAnisotropy =  Number

data TextureImageFormat = 
    TF_RGBA
  | TF_RGB

data TextureInternalFormat = 
    TIF_RGBA
  | TIF_RGB

data TexelDataType = 
    TexelUnsignedByte
  | TexelUnsignedShort565
  | TexelUnsignedShort4444
  | TexelUnsignedShort5551

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

