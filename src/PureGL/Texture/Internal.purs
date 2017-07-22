module PureGL.Texture.Internal where

import Prelude

import Control.Monad.Reader (ask)
import PureGL.Context (Context(..))
import PureGL.GLConstant (getValue)
import PureGL.RenderState (RenderT)
import PureGL.Texture (TextureSampler(..))
import PureGL.WebGL (texParameterf, texParameteri)
import PureGL.WebGL.Constants (gl_TEXTURE_MAG_FILTER, gl_TEXTURE_MAX_ANISOTROPY_EXT, gl_TEXTURE_MIN_FILTER, gl_TEXTURE_WRAP_S, gl_TEXTURE_WRAP_T)
import PureGL.WebGL.Types (WebGLTexture, GLenum)

setSamplerState :: forall eff. WebGLTexture -> GLenum -> TextureSampler -> RenderT eff Unit
setSamplerState tex target (TextureSampler s) = do
  (Context context) <- ask
  texParameteri target gl_TEXTURE_MAG_FILTER (getValue s.magFilter)
  texParameteri target gl_TEXTURE_MIN_FILTER (getValue s.minFilter)
  texParameteri target gl_TEXTURE_WRAP_S (getValue s.wrapS)
  texParameteri target gl_TEXTURE_WRAP_T (getValue s.wrapT)
  case context.enabledExtensions.ext_texture_filter_anisotropic of
    true -> texParameterf target gl_TEXTURE_MAX_ANISOTROPY_EXT  s.maxAnisotropy
    false -> pure unit
