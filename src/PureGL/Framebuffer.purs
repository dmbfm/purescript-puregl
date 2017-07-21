module PureGL.Framebuffer where

import Data.Maybe (Maybe)
import PureGL.Types (ResourceId)
import PureGL.WebGL.Types (WebGLFramebuffer, WebGLRenderbuffer)

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
