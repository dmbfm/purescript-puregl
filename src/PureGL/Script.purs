module PureGL.Script where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.State (get)
import Data.Foldable (traverse_)
import Data.Lens (Lens', view)
import Data.Lens.Record (prop)
import Data.Map (empty, toUnfoldable)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import PureGL.ECS (ECSManager(..), Entity, SystemWithComponents, _components, _ecsManager, _systemStates)
import PureGL.Renderer.RenderState (RenderT)
import PureGL.Scene (_sceneState)

newtype ScriptComponent = ScriptComponent { update :: forall e r . Entity -> RenderT r e Unit  }

x :: ScriptComponent
x = ScriptComponent { update: (\_ -> pure unit) }

type ScriptState = SystemWithComponents ScriptComponent ()


emptyScriptState :: ScriptState
emptyScriptState = { components: empty }

updateScriptSystem :: forall r e. RenderT (script :: ScriptState | r) e Unit
updateScriptSystem = do
  state <- get
  let scriptComponents = view (_ecsManager <<< _systemStates <<< _scriptState <<< _components) state
  let cs :: Array (Tuple Entity ScriptComponent)
      cs = toUnfoldable scriptComponents
  traverse_ (\(Tuple e (ScriptComponent c) ) -> c.update e) cs


-- lenses
_scriptState :: forall r. Lens' { script :: ScriptState | r}  ScriptState
_scriptState = prop (SProxy :: SProxy "script")                  