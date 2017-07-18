module PureGL.Texture where

import Prelude

import DOM.HTML.Types (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement)
import PureGL.Data.TypedArrays (Uint8Array)
import PureGL.WebGL (class GLConstant)
import PureGL.WebGL.Constants (gl_CLAMP_TO_EDGE, gl_LINEAR, gl_LINEAR_MIPMAP_LINEAR, gl_LINEAR_MIPMAP_NEAREST, gl_MIRRORED_REPEAT, gl_NEAREST, gl_NEAREST_MIPMAP_LINEAR, gl_NEAREST_MIPMAP_NEAREST, gl_REPEAT, gl_RGB, gl_RGBA, gl_TEXTURE_2D, gl_TEXTURE_CUBE_MAP, gl_UNSIGNED_BYTE, gl_UNSIGNED_SHORT_4_4_4_4, gl_UNSIGNED_SHORT_5_5_5_1, gl_UNSIGNED_SHORT_5_6_5)
import PureGL.WebGL.Types (ImageData, WebGLTexture)

newtype TextureSampler = TextureSampler {  magFilter :: TextureMagFilter
                                        , minFilter :: TextureMinFilter
                                        , wrapS :: TextureWrap
                                        , wrapT :: TextureWrap
                                        , maxAnisotropy :: TextureMaxAnisotropy
                                        }

newtype Texture = Texture { pixels :: TexturePixels
                          , magFilter :: TextureMagFilter
                          , minFilter :: TextureMinFilter
                          , wrapS :: TextureWrap
                          , wrapT :: TextureWrap
                          , maxAnisotropy :: TextureMaxAnisotropy
                          , format :: TextureFormat
                          , internalFormat :: TextureInternalFormat
                          , texelDataType :: TexelDataType
                          , textureTarget :: TextureTarget
                          }


newtype LoadedTexture = LoadedTexture { texture :: WebGLTexture 
                                      , textureTarget :: TextureTarget
                                      }

setPixels ::  TexturePixels -> Texture -> Texture
setPixels p (Texture t) = Texture $ t { pixels = p }                          

setMaxAnisotropy :: TextureMaxAnisotropy -> Texture -> Texture
setMaxAnisotropy a (Texture t) = Texture $ t { maxAnisotropy = a }

mkTexture :: TexturePixels -> 
             TextureMagFilter -> 
             TextureMinFilter ->
             TextureWrap -> 
             TextureWrap -> 
             TextureMaxAnisotropy -> 
             TextureFormat -> 
             TextureInternalFormat -> 
             TexelDataType -> 
             TextureTarget -> 
             Texture 
mkTexture p mag min ws wt ma f f' t tg = 
  Texture { pixels: p
          , magFilter: mag
          , minFilter: min
          , wrapS: ws
          , wrapT: wt
          , maxAnisotropy: ma
          , format: f
          , internalFormat: f'
          , texelDataType: t
          , textureTarget: tg
          }

mkTexture' :: TexturePixels -> 
              TextureMagFilter -> 
              TextureMinFilter -> 
              TextureWrap -> 
              TextureWrap ->
              TextureMaxAnisotropy ->
              Texture
mkTexture' p mag min ws wt m = 
  mkTexture p mag min ws wt m TF_RGBA TIF_RGBA TexelUnsignedByte TextureTarget2D

mkTexture'':: TexturePixels ->
              TextureMagFilter -> 
              TextureMinFilter -> 
              TextureMaxAnisotropy -> 
              Texture 
mkTexture'' p mag min m = mkTexture' p mag min WrapRepeat WrapRepeat m

mkTexture''' ::  TexturePixels -> TextureMaxAnisotropy -> Texture
mkTexture''' p a = mkTexture'' p MagLinear MinLinear a

mkTexture'''' :: TexturePixels -> Texture
mkTexture'''' p = mkTexture''' p (TextureMaxAnisotropy 1.0)

data TextureTarget = 
    TextureTarget2D
  | TextureTargetCubeMap

data TexturePixels = 
    HTMLImagePixels HTMLImageElement
  | HTMLVideoPixels HTMLVideoElement
  | HTMLCanvasPixels HTMLCanvasElement
  | ImageDataPixels ImageData
  | Uint8ArrayPixels Uint8Array
  | NullPixels

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

data TextureMaxAnisotropy = TextureMaxAnisotropy Number

data TextureFormat = 
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

instance texAnisotropy :: GLConstant TextureMaxAnisotropy Number where
  getValue (TextureMaxAnisotropy v) = v

instance textureFormatGLConstant :: GLConstant TextureFormat Int where
  getValue TF_RGB = gl_RGB
  getValue TF_RGBA = gl_RGBA

instance textureInternalFormatGLConstant :: GLConstant TextureInternalFormat Int where
  getValue TIF_RGB = gl_RGB
  getValue TIF_RGBA = gl_RGBA

