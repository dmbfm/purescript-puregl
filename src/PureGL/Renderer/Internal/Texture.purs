module PureGL.Renderer.Internal.Texture where

import Prelude

import Control.Monad.Reader (ask)
import PureGL.Context (Context(..))
import PureGL.Renderer.GLConstant (getValue)
import PureGL.Renderer.RenderState (RenderT)
import PureGL.Renderer.Texture (TextureSampler(..))
import PureGL.WebGL (texParameterf, texParameteri)
import PureGL.WebGL.Constants (gl_TEXTURE_MAG_FILTER, gl_TEXTURE_MAX_ANISOTROPY_EXT, gl_TEXTURE_MIN_FILTER, gl_TEXTURE_WRAP_S, gl_TEXTURE_WRAP_T)
import PureGL.WebGL.Types (WebGLTexture, GLenum)

setSamplerState :: forall e r. WebGLTexture -> GLenum -> TextureSampler -> RenderT e r Unit
setSamplerState tex target (TextureSampler s) = do
  (Context context) <- ask
  texParameteri target gl_TEXTURE_MAG_FILTER (getValue s.magFilter)
  texParameteri target gl_TEXTURE_MIN_FILTER (getValue s.minFilter)
  texParameteri target gl_TEXTURE_WRAP_S (getValue s.wrapS)
  texParameteri target gl_TEXTURE_WRAP_T (getValue s.wrapT)
  case context.enabledExtensions.ext_texture_filter_anisotropic of
    true -> texParameterf target gl_TEXTURE_MAX_ANISOTROPY_EXT  s.maxAnisotropy
    false -> pure unit
