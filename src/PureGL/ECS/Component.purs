module PureGL.ECS.Component where

import Prelude

import Control.Monad.State (class MonadState, get, modify)
import Data.Lens (_Just, over, (^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map, insert, member)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy)
import PureGL.ECS (ECSManager, Entity, SystemWithComponents, _components, _ecsManager, _systemStates)

-- | Checks if a system named `s`, which is of type `∀ r2. SystemWithComponents
-- | c r2`, has a component for a given `Entity`.
-- | 
-- | ```purescript
-- | hasComponent SProxy :: SProxy "mesh" entity
-- | ```
-- | 
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

-- | Tries to get a component of type `c` for a system named `s`, with state of 
-- | type `∀ r2. SystemWithComponents c r2` in a `ECSManager` state monad.
-- | 
-- | ```purescript
-- | maybeComponent <- getComponent (SProxy :: SProxy "mesh") entity
-- | ```
-- |  
getComponent :: ∀ c m t r1 s r2. Bind m ⇒ 
                                 MonadState (ECSManager { | r1 }) m ⇒ 
                                 IsSymbol s ⇒                                  
                                 RowCons s (SystemWithComponents c r2) t r1 ⇒ 
                                 SProxy s →
                                 Entity →                                   
                                 m (Maybe c)

getComponent sp e = do
  state <- get
  pure $ state ^. _ecsManager <<< _systemStates <<< prop sp <<< _components <<< at e

modifyComponent :: ∀ c m t r1 s r2. Bind m ⇒ 
                                    MonadState (ECSManager { | r1 }) m ⇒ 
                                    IsSymbol s ⇒                                  
                                    RowCons s (SystemWithComponents c r2) t r1 ⇒ 
                                    SProxy s →
                                    Entity →
                                    (c -> c) ->            
                                    m Unit

modifyComponent sp e f = 
  modify $ over (_ecsManager <<< _systemStates <<< prop sp <<< _components <<< at e <<< _Just) f

-- | Insert/replace a component of type `c` in a system named `s`, of type 
-- | `∀ r2. SystemWithComponents c r2` for a given `Entity`.
-- | 
-- | ```purescript 
-- | insertComponent (SProxy :: SProxy "mesh") entity meshComponet
-- | ```
insertComponent :: ∀ t r1 s r2 c m. MonadState (ECSManager { | r1 }) m ⇒ 
                                    IsSymbol s ⇒ 
                                    RowCons s (SystemWithComponents c r2) t r1 ⇒ 
                                    SProxy s → 
                                    Entity → 
                                    c → 
                                    m Unit
insertComponent sp e c =
  modify $ over (_ecsManager <<< _systemStates <<< prop sp <<< _components) (insert e c)

-- | Get the entity-component `Map` for a given system `s`.
getComponents :: ∀ c m t r1 s r2. Bind m ⇒ 
                                 MonadState (ECSManager { | r1 }) m ⇒ 
                                 IsSymbol s ⇒                                  
                                 RowCons s (SystemWithComponents c r2) t r1 ⇒ 
                                 SProxy s →
                                 m (Map Entity c)
getComponents sp = do
  state <- get
  pure $ state ^. _ecsManager <<< _systemStates <<< prop sp <<< _components  

