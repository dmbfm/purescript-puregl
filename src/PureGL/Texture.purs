module PureGL.Texture where

import DOM.HTML.Types (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement)
import PureGL.Data.TypedArrays (Uint8Array)
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

