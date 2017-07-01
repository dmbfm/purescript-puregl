module PureGL.WebGL where
  
import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either(..), either)
import PureGL.Context (Context(..), ContextR, GLContextR)
import PureGL.WebGL.Raw as RAW
import PureGL.WebGL.Types (class GLContext, WebGLBuffer, WebGLEff)

runGLContext :: forall ctx eff a. GLContext ctx => ctx -> GLContextR ctx eff a ->  WebGLEff eff a
runGLContext ctx reader = runReaderT reader ctx


createBuffer :: forall eff ctx. GLContext ctx => GLContextR ctx eff WebGLBuffer
createBuffer = ask >>= \ctx -> liftEff $ RAW.createBuffer ctx

createBuffer' :: forall eff. ContextR eff WebGLBuffer
createBuffer' = ask >>= \(Context ctx) -> 
  case ctx.glContext of 
    Left ctx1 -> liftEff $ RAW.createBuffer ctx1
    Right ctx2 -> liftEff $ RAW.createBuffer ctx2

