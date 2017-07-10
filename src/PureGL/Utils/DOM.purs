module PureGL.Utils.DOM 
  ( getCanvasElement
  , getWebGL1Context
  , getWebGL2Context 
  ) 
  where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.Types (HTMLCanvasElement)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import PureGL.WebGL.Types (WEBGL, WebGLContext)

getCanvasElement :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe HTMLCanvasElement)
getCanvasElement id = toMaybe <$> _getCanvasElement id

getWebGL1Context :: forall eff. HTMLCanvasElement -> Eff (webgl :: WEBGL | eff) (Maybe WebGLContext)
getWebGL1Context canvas = toMaybe <$> _getWebGL1Context canvas

getWebGL2Context :: forall eff. HTMLCanvasElement -> Eff (webgl :: WEBGL | eff) (Maybe WebGLContext)
getWebGL2Context canvas = toMaybe <$> _getWebGL2Context canvas

foreign import _getCanvasElement :: forall eff. String -> Eff (dom :: DOM | eff) (Nullable HTMLCanvasElement)
foreign import _getWebGL1Context :: forall eff. HTMLCanvasElement -> Eff (webgl :: WEBGL | eff) (Nullable WebGLContext)
foreign import _getWebGL2Context :: forall eff. HTMLCanvasElement -> Eff (webgl :: WEBGL | eff) (Nullable WebGLContext)

