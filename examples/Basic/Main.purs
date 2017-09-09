module Example.PureGL.Basic.Main where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Tree (showTree)
import PureGL (init)
import PureGL.Context (Context(..), fromCanvasId)
import PureGL.ECS (_ecsManager, _systemStates)
import PureGL.ECS.Entity (newEntity)
import PureGL.Math.Matrix as M
import PureGL.Math.Vector as V
import PureGL.Scene (SceneComponent, _root, _sceneState, addSceneComponent, sceneRoot)
import PureGL.Utils.Log (logObject)
import PureGL.WebGL.Types (WebGLEff)

sceneComponent :: SceneComponent
sceneComponent = { position: V.mkVector3 0.0 0.0 0.0
                 , orientation: one
                 , scale: V.mkVector3 1.0 1.0 1.0
                 , transform: M.identity
                 }

main :: forall e. WebGLEff (console :: CONSOLE | e) Unit
main = do
  log "Basic example"
  contextM <- fromCanvasId "canvas"
  case contextM of
    Nothing -> log "Failed to create context."
    Just context -> main_ context

  where

    main_ :: Context -> WebGLEff (console :: CONSOLE | e) Unit
    main_ context = do
      
      -- Initialize the ECS system, starting from a "blank" state
      initialState <- init {} context $ do
        e <- newEntity
        e' <- newEntity
        e'' <- newEntity

        addSceneComponent e sceneRoot sceneComponent
        addSceneComponent e' e sceneComponent
        addSceneComponent e'' sceneRoot sceneComponent
        pure unit

      logObject "initialState" initialState
      log $ showTree $ view (_ecsManager <<< _systemStates <<< _sceneState <<< _root) initialState