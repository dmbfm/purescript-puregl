module PureGL.Context where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Reader.Trans (ReaderT)
import DOM (DOM)
import DOM.HTML.Types (HTMLCanvasElement)
import Data.Maybe (Maybe(..))
import PureGL.Extensions (EnabledExtensions, initExtensions)
import PureGL.Utils.DOM (getCanvasElement, getWebGL1Context, getWebGL2Context)
import PureGL.WebGL.Types (WEBGL, WebGLContext, WebGLEff)

-- | This type describes the possible WebGL Versions
data GLVersion = WebGL1 | WebGL2

-- | The `Context` type stores information about the WebGL context
-- | being used, like the `WebGLContext` object, the context's webgl version,
-- | and the canvas element it is attached to.
newtype Context = Context { glContext :: WebGLContext
                          , glVersion :: GLVersion
                          , enabledExtensions :: EnabledExtensions
                          , canvas :: HTMLCanvasElement
                          }

-- | A Reader Transformer for `Context` with WebGL Effects
type ContextR eff a = ReaderT Context (WebGLEff eff) a

-- | A Reader Transofmer to be used with `GLContext`s
type WebGLContextR eff a = ReaderT WebGLContext (WebGLEff eff) a

-- | Creates a `Context` from a HTMLCanvasElement id. If possible, it creates
-- | a WebGL2 context; if this fails, it creates a WebGL1 context.
fromCanvasId :: forall eff. String -> Eff (webgl :: WEBGL, dom :: DOM | eff) (Maybe Context)
fromCanvasId id = do
  canvas <- getCanvasElement id
  case canvas of    
    Just canvas' -> fromCanvasElement canvas'
    Nothing -> pure $ Nothing

-- | Creates a `Context` from a HTMLCanvasElement. If possible, it creates
-- | a WebGL2 context; if this fails, it creates a WebGL1 context.
fromCanvasElement:: forall eff. HTMLCanvasElement -> Eff (webgl :: WEBGL | eff) (Maybe Context)
fromCanvasElement canvas = do
  ctx2 <- getWebGL2Context canvas
  case ctx2 of 
    Just ctx2' -> do
      enabledExtensions <- initExtensions ctx2' 
      pure $ Just $ Context { glContext: ctx2'
                            , glVersion: WebGL2
                            , enabledExtensions: enabledExtensions
                            , canvas: canvas
                            }
    Nothing -> do
      ctx <- getWebGL1Context canvas
      case ctx of
        Just ctx' -> do
          enabledExtensions <- initExtensions ctx' 
          pure $ Just $ Context { glContext: ctx'
                                , glVersion: WebGL1
                                , enabledExtensions: enabledExtensions
                                , canvas: canvas
                                }
        Nothing -> pure $ Nothing
