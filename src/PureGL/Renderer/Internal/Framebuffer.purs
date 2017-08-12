module PureGL.Renderer.Internal.Framebuffer where

import Prelude

import PureGL.Renderer.Framebuffer (LoadedRenderbuffer(..))
import PureGL.Renderer.GLConstant (getValue)
import PureGL.Renderer.RenderState (RenderT)
import PureGL.Renderer.Texture (LoadedTexture(..))
import PureGL.WebGL (framebufferRenderbuffer, framebufferTexture2D)
import PureGL.WebGL.Constants (gl_FRAMEBUFFER, gl_RENDERBUFFER)
import PureGL.WebGL.Types (GLenum)

class FBAttachable a where 
  attachToFramebuffer :: forall e r. a -> GLenum -> RenderT r e Unit

instance loadedRenderbufferFBAttachable ::  FBAttachable LoadedRenderbuffer where
  attachToFramebuffer (LoadedRenderbuffer rb) target = 
    framebufferRenderbuffer gl_FRAMEBUFFER target gl_RENDERBUFFER rb.renderbuffer

instance loadedTextureFBAttachable :: FBAttachable LoadedTexture where
  attachToFramebuffer(LoadedTexture t) target = 
    framebufferTexture2D gl_FRAMEBUFFER target (getValue t.textureTarget) t.texture 0 
    
