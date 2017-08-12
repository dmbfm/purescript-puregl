module PureGL.ECS.Entity where

import Prelude

import Control.Monad.State (get, modify, put)
import Data.Array (elem, filter, insert)
import Data.Lens (over, view)
import Data.Tuple (Tuple(..))
import PureGL.ECS (ECSManager, ECSManagerT, Entity, _ecsManager, _entities)
import PureGL.Utils.HasID (requestId, returnId)

-- | Creates a new `Entity` in a `ECSManagerT e c r` monad.
newEntity :: forall e c r. ECSManagerT e c r Entity
newEntity = do
  state <- get
  case requestId state of
    Tuple id state' -> do
      put $ over (_ecsManager <<< _entities) (insert id) state'
      pure id

-- | Checks if an `Entity` is presend in the `ECSManager r` `entities` array.
hasEntity :: forall r. Entity -> ECSManager r -> Boolean
hasEntity e = (elem e) <<< view (_ecsManager <<< _entities)

-- | Remove an `Entity` from the `entities` array and returns it's ID to the
-- | `idPool`.
removeEntity :: forall e c r. Entity -> ECSManagerT e c r Unit
removeEntity e = do
  state <- get
  case hasEntity e state of
    false -> pure unit
    true -> do
      modify $ (returnId e)  <<< over (_ecsManager <<< _entities) (filter $ notEq e)
