module PureGL  where

import Prelude

import PureGL.Context (Context)
import PureGL.ECS (ECSManager, ECSManagerT, execECSManagerT, fromSystemStates)
import PureGL.Mesh (MeshSystemState, meshEmptyState)
import PureGL.Renderer.RenderState (RenderState, emptyRenderState)
import PureGL.Utils.Misc (merge)
import PureGL.WebGL.Types (WEBGL, WebGLEff)

-- | Extendable record type alias for the base PureGL ECS system.
type PureGLRec r = { renderer :: RenderState, mesh :: MeshSystemState | r}

-- | The `ECSManager` type for the PureGL system. It can be extend via the `r`
-- | parameter to include user-defined systems.
type PureGL r = ECSManager (PureGLRec r)

-- | The `ECSManagerT` transformer stack for the PureGL system. It includes
-- | `WEBGL` effects and `Context` as the ECS context. Here: 
-- | 
-- | `r` -> User-defined type row to extend the ECS systems
-- | `e` -> Additional effects
-- | `a` -> Return value
-- | 
-- | ```purescript
-- | type MyPureGLT e a = PureGLT ( mySystem :: MySystem ) e a
-- | ```
type PureGLT r e a = ECSManagerT (webgl :: WEBGL | e) Context (PureGLRec r) a

-- | Create an initial, 'empty', `PureGL r` state from an initial value for the
-- | user-defined extension `r`.
pureGL :: forall r. { | r } -> PureGL r
pureGL r = fromSystemStates $ merge { renderer: emptyRenderState
                                    , mesh: meshEmptyState 
                                    } r

-- | Runs an initialization action for the PureGL system. Given an initial value
-- | for the user-defined extension `r`, a `Context` and an initialization action
-- | `PureGLT r e Unit`, it returns the initialized state `PudeGL r` wrapped
-- | inside an `Eff`.
init :: forall r e. { | r } -> Context -> PureGLT r e Unit -> WebGLEff e (PureGL r)
init r ctx run = execECSManagerT ctx (pureGL r) run
