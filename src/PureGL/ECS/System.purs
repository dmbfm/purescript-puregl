module PureGL.ECS.System where

import Prelude

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (class MonadState)
import Data.Dynamic (Dynamic(..))
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..))
import PureGL.Context (Context(..))
import PureGL.ECS (ECSManager(..), SystemWithComponents, Entity)
import PureGL.WebGL.Types (WebGLEffRows)

updateSystem :: forall c m t r1 s r2 e . MonadState (ECSManager { | r1 }) m ⇒
                                         MonadEff (WebGLEffRows e) m =>
                                         MonadReader Context m =>
                                         MonadThrow Dynamic m =>
                                         IsSymbol s ⇒                                  
                                         RowCons s (SystemWithComponents c r2) t r1 ⇒ 
                                         SProxy s →
                                         (Entity -> c -> m c) →
                                         m Unit

updateSystem sp f = pure unit
