module PureGL.Extensions where

import PureGL.WebGL.Types (WebGLContext, WebGLEff)

type EnabledExtensions = { ext_texture_filter_anisotropic :: Boolean 
                         , webgl_debug_renderer_info :: Boolean 
                         }

foreign import initExtensions :: forall eff.  WebGLContext -> WebGLEff eff EnabledExtensions
