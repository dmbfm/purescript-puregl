module PureGL.ECS.Component where

import Prelude

import Control.Monad.State (class MonadState, get, modify)
import Data.Lens (over, (^.))
import Data.Lens.Record (prop)
import Data.Map (insert, member)
import Data.Symbol (class IsSymbol, SProxy)
import PureGL.ECS (ECSManager, Entity, SystemWithComponents, _components, _ecsManager, _systemStates)

-- | Checks if a system named `s`, which is of type `∀ r2. SystemWithComponents
-- | c r2`, has a component for a given `Entity`
hasComponent :: ∀ c m t r1 s r2. Bind m ⇒ 
                                 MonadState (ECSManager { | r1 }) m ⇒ 
                                 IsSymbol s ⇒                                  
                                 RowCons s (SystemWithComponents c r2) t r1 ⇒ 
                                 SProxy s →
                                 Entity →                                   
                                 m Boolean

hasComponent sp e = do
  state <- get
  pure $ member e (state ^. _ecsManager <<< _systemStates <<< prop sp <<< _components)

-- | Insert/replace a component of type `c` in a system named `s`, of type 
-- | `∀ r2. SystemWithComponents c r2` for a given `Entity`
insertComponent :: ∀ t r1 s r2 c m. MonadState (ECSManager { | r1 }) m ⇒ 
                                    IsSymbol s ⇒ 
                                    RowCons s (SystemWithComponents c r2) t r1 ⇒ 
                                    SProxy s → 
                                    Entity → 
                                    c → 
                                    m Unit
insertComponent sp e c =
  modify $ over (_ecsManager <<< _systemStates <<< prop sp <<< _components) (insert e c)