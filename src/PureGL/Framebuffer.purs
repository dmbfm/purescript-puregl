module PureGL.Framebuffer where

import Prelude

import Data.Maybe (Maybe)
import PureGL.Context (ContextR)
import PureGL.Texture (LoadedTexture(..), RenderTexture(..))
import PureGL.Types (ResourceId)
import PureGL.WebGL (class GLConstant, framebufferRenderbuffer, framebufferTexture2D, getValue)
import PureGL.WebGL.Constants (gl_DEPTH_COMPONENT16, gl_FRAMEBUFFER, gl_RENDERBUFFER, gl_RGB565, gl_RGB5_A1, gl_RGBA4, gl_STENCIL_INDEX8)
import PureGL.WebGL.Types (WebGLFramebuffer, WebGLRenderbuffer, GLenum)

newtype Framebuffer = Framebuffer { color :: FBColorAttachment
                                  , depth :: Maybe ResourceId
                                  }

newtype LoadedFramebuffer = LoadedFramebuffer { fbo :: WebGLFramebuffer 
                                              , color :: FBColorAttachment 
                                              , depth :: Maybe ResourceId
                                              }

newtype Renderbuffer = Renderbuffer { width :: Int
                                    , height :: Int
                                    , format :: RenderbufferFormat
                                    }

newtype LoadedRenderbuffer = LoadedRenderbuffer { renderbuffer :: WebGLRenderbuffer }

data FBColorAttachment = 
    FBColorAttRenderTexture ResourceId
  | FBColorAttRebderBuffer  ResourceId

data RenderbufferFormat = 
    RenderbufferDepthComponent16
  | RenderbufferRGBA4
  | RenderbufferRGB5_A1
  | RenderbufferRGB565
  | RenderbufferStencilIndex8

instance renderbufferFormatConstant :: GLConstant RenderbufferFormat Int where
  getValue RenderbufferDepthComponent16 = gl_DEPTH_COMPONENT16
  getValue RenderbufferRGB565 = gl_RGB565
  getValue RenderbufferRGBA4 = gl_RGBA4
  getValue RenderbufferRGB5_A1 = gl_RGB5_A1
  getValue RenderbufferStencilIndex8 = gl_STENCIL_INDEX8
  
class FBAttachable a where 
  attachToFramebuffer :: forall eff. a -> GLenum -> ContextR eff Unit

instance loadedRenderbufferFBAttachable ::  FBAttachable LoadedRenderbuffer where
  attachToFramebuffer (LoadedRenderbuffer rb) target = 
    framebufferRenderbuffer gl_FRAMEBUFFER target gl_RENDERBUFFER rb.renderbuffer

instance loadedTextureFBAttachable :: FBAttachable LoadedTexture where
  attachToFramebuffer(LoadedTexture t) target = 
    framebufferTexture2D gl_FRAMEBUFFER target (getValue t.textureTarget) t.texture 0 
    