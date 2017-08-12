module PureGL.ECS.Types where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Dynamic (Dynamic)
import Data.Lens (Lens', lens, over, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map, empty)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import PureGL.Utils.HasID (class HasID)


-- | An `Entity` is identified by its integer ID
type Entity = Int

-- | Each system is identitfied by its guid `String`
type SystemID = String

-- | The `Record` type for the `ECSManager`. This holds | the state for the ECS
-- | system. 
-- | 
-- | The parameter `r` is an arbitrary data type that should | represent the
-- | inner state of all the systems, including | their components. 
-- | 
-- | The idea is that, in practice, the parameter type `r` | should not be
-- | completly arbitrary, but should follow | some convention. This convention
-- | will be somewhat inforced | by provided auxiliary types and functions.
-- | 
-- | In this sense, the `ECSManager` type is an auxiliary | data type in
-- | constructiong the state for the ECS system, | not a ready to use type.
type ECSManagerRec r = {
                       -- | An array with the entities currently active in the system 
                         entities :: Array Entity 

                       -- | An array with the IDs of the avaliable systems
                       , systemIDs :: Array SystemID 

                       -- | The states and components of the available systems
                       , systemStates :: r

                       -- | A counter for the `Entity`s IDs
                       , idCounter :: Int

                       -- | A pool for free `Entity` IDs
                       , idPool :: Array Int

                       }


-- | The `ECSManager` data type wraps the `ECSManagerRec` `Record` described above
newtype ECSManager r = ECSManager (ECSManagerRec r)

-- | This is an auxiliary type alias for the data of a system with | an inner
-- | state and entity components. 
-- | 
-- | ```purescript
-- | type MyECSManager = ECSManager { systemA :: SystemWithComponents Int ( state :: Int )
-- |                                , systemB :: SystemWithComponents Number () 
-- |                                }
-- | 
-- | ecs2 :: MyECSManager
-- | ecs2 = ECSManager { entities: []
-- |                   , systemIDs: []
-- |                   , systemStates: { systemA: { components: empty, state: 0 }
-- |                                   , systemB: { components: empty }
-- |                                   }
-- |                   , idCounter: 0
-- |                   , idPool: []
-- |                   }
-- | ```
type SystemWithComponents c r = { components :: Map Entity c | r }

-- | The transformer stack for the `ECSManager` data type.
-- | 
-- | `e` -> The effects row
-- | `c` -> The context type
-- | `r` -> The systems type
-- | `a` -> The result type
type ECSManagerT e c r a = ExceptT Dynamic (ReaderT c (StateT (ECSManager r) (Eff e))) a

-- Some Instances
instance hasIDECSManager :: HasID (ECSManager r) where
  getIdCounter = view $ _ecsManager <<< _idCounter
  getIdPool = view $ _ecsManager <<< _idPool
  modifyIdCounter = over  _ecsManager <<< _idCounter
  modifyIdPool = over _ecsManager <<< _idPool

instance showECSManager :: Show (ECSManager r) where
  show (ECSManager r) = "(ECSManager { entities: " <> (show r.entities) <>
                       "\n            , systemIDs: " <> (show r.systemIDs) <>
                       "\n            , systemStates: (systemStates) " <>
                       "\n            , idCounter: " <> (show r.idCounter) <>
                       "\n            , idPool: " <> (show r.idPool) <>
                       "\n            })"

-- sample state
ecs :: ECSManager {}
ecs = ECSManager { entities: []
                 , systemIDs: []
                 , systemStates: {}
                 , idCounter: 0
                 , idPool: []
                 }

type MyECSManagerR = { systemA :: SystemWithComponents Int ( state :: Int )
                     , systemB :: SystemWithComponents Number () 
                     }

type MyECSManager = ECSManager MyECSManagerR

ecs2 :: MyECSManager
ecs2 = ECSManager { entities: []
                  , systemIDs: []
                  , systemStates: { systemA: { components: empty, state: 0 }
                                  , systemB: { components: empty }
                                  }
                  , idCounter: 0
                  , idPool: []
                  }
-- LENSES
_ecsManager :: forall r. Lens' (ECSManager r) (ECSManagerRec r)
_ecsManager = lens (\(ECSManager r) -> r) (\_ -> ECSManager)

_entities :: forall r. Lens' (ECSManagerRec r) (Array Entity)
_entities = prop (SProxy :: SProxy "entities")

_systemIDs :: forall r. Lens' (ECSManagerRec r) (Array SystemID)
_systemIDs = prop (SProxy :: SProxy "systemIDs")

_systemStates :: forall r. Lens' (ECSManagerRec r) r
_systemStates = prop (SProxy :: SProxy "systemStates")

_idCounter :: forall r. Lens' (ECSManagerRec r) Int
_idCounter = prop (SProxy :: SProxy "idCounter")

_idPool :: forall r. Lens' (ECSManagerRec r) (Array Int)
_idPool = prop (SProxy :: SProxy "idPool")

_components :: forall c r. Lens' (SystemWithComponents c r) (Map Entity c)
_components = prop (SProxy :: SProxy "components")

-- Some Tests
