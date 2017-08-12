module PureGL.ECS.ECSManager where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State (execStateT, get, modify, put)
import DOM (DOM)
import Data.Array (insert)
import Data.Lens (over, (^.))
import Data.Tuple (Tuple(..))
import PureGL.ECS.Types (ECSManager(..), ECSManagerT, Entity, MyECSManagerR, _ecsManager, _entities, _idCounter)
import PureGL.Utils.HasID (incrementIdCounter, requestId)
import Signal (Signal, foldpE, runSignal, sampleOn, (~>))
import Signal.DOM (animationFrame)

-- | Create an initial state for a `ECSManager r` from a `systemStates` object
-- | of type `r`
fromSystemStates :: forall r. r -> ECSManager r
fromSystemStates r = ECSManager { entities: []
                                , systemIDs: []
                                , systemStates: r
                                , idCounter: 0
                                , idPool: []
                                }

-- | Given a context of type `c`, an initial state of type `ECSManager r`, and a
-- | `ECSManagerT e c r a` action, this function evaluates and returns the final
-- | state wrapped in the `Eff` monad.
execECSManagerT :: forall e c r a. c -> 
                                   ECSManager r -> 
                                   ECSManagerT e c r a -> 
                                   Eff e (ECSManager r)

execECSManagerT c s =  ((flip execStateT) s) <<< ((flip runReaderT ) c) <<< runExceptT 

-- | Create a signal for `ECSManagerT e c r Unit` from a run function `(i ->
-- | ECSManagerT e c r Unit)`, a context `c`, an initial state `ECSManager r` and
-- | an input signal `Signal i`.
ecsSignal :: forall i e c r. c -> 
                             ECSManager r -> 
                             Signal i -> 
                             (i -> ECSManagerT e c r Unit) -> 
                             Signal (ECSManager r)

ecsSignal ctx state input run = 
  foldpE (\i s -> execECSManagerT ctx s (run i)) state input

-- | Runs the ECS system using the `animationFrame` signal. It takes a run
-- | function `(i -> ECSManagerT e c r Unit)`,  a context `c`, an initial state
-- | `ECSManager r`, an input signal `Signal i` which will be sampled on each frame, 
-- | and starts a loop via the `animationFrame` (i.e., `requestAnimationFrame`).
ecsRun :: forall i e c r. c -> 
                          (ECSManager r) -> 
                          Signal i -> 
                          (i -> ECSManagerT e c r Unit) -> 
                          Eff (dom :: DOM, timer :: TIMER | e) Unit

ecsRun ctx state input run = do
  frame <- animationFrame
  let inputSignal = sampleOn frame input
  runSignal $ (ecsSignal ctx state inputSignal run) ~> (const $ pure unit)

-- | Sample run function
sampleECSRun :: forall e. Unit -> ECSManagerT (console :: CONSOLE | e) String MyECSManagerR Unit
sampleECSRun _ = do  
  ctx <- ask
  s <- get
  liftEff $ log $ "(" <> ctx <> ", " <> (show $ s ^. _ecsManager <<<  _idCounter)  <> ")"
  modify incrementIdCounter
