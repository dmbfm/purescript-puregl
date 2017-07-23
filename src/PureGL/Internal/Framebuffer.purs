module PureGL.Internal.Framebuffer where

import Prelude

import PureGL.Framebuffer (LoadedRenderbuffer(..))
import PureGL.GLConstant (getValue)
import PureGL.RenderState (RenderT)
import PureGL.Texture (LoadedTexture(..))
import PureGL.WebGL (framebufferRenderbuffer, framebufferTexture2D)
import PureGL.WebGL.Constants (gl_FRAMEBUFFER, gl_RENDERBUFFER)
import PureGL.WebGL.Types (GLenum)

class FBAttachable a where 
  attachToFramebuffer :: forall eff. a -> GLenum -> RenderT eff Unit

instance loadedRenderbufferFBAttachable ::  FBAttachable LoadedRenderbuffer where
  attachToFramebuffer (LoadedRenderbuffer rb) target = 
    framebufferRenderbuffer gl_FRAMEBUFFER target gl_RENDERBUFFER rb.renderbuffer

instance loadedTextureFBAttachable :: FBAttachable LoadedTexture where
  attachToFramebuffer(LoadedTexture t) target = 
    framebufferTexture2D gl_FRAMEBUFFER target (getValue t.textureTarget) t.texture 0 
    
