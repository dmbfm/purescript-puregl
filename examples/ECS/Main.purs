module PureGL.Examples.ECS.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.State (get, modify)
import DOM (DOM)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import PureGL (PureGL, PureGLT, init, pureGL)
import PureGL.Context (Context(..), fromCanvasId)
import PureGL.ECS.ECSManager (ecsRun, execECSManagerT, sampleECSRun)
import PureGL.ECS.Entity (newEntity, removeEntity)
import PureGL.ECS.Types (ECSManagerT, ecs2)
import PureGL.Renderer.Program (Program(..), Uniform(..))
import PureGL.Renderer.RenderResource (loadProgram)
import PureGL.Utils.HasID (incrementIdCounter)
import PureGL.Utils.Log (logObject)
import PureGL.Utils.Misc ((>-))
import PureGL.WebGL.Types (WEBGL, WebGLEff)
import Signal (constant)

unlines :: Array String -> String
unlines lines = foldl (\acc s -> acc <> s <> "\n") "" lines

simpleProgram :: Program
simpleProgram =
  Program { vertexShaderSource: unlines   [ ""
                                          , "attribute vec3 position;"
                                          , "attribute vec2 uv;"
                                          , "varying vec2 fs_uv;"
                                          , "void main() {"
                                          , "fs_uv = uv;"
                                          , "gl_Position = vec4(position, 1.0);"
                                          , "}"
                                          ]
          , fragmentShaderSource: unlines [ ""
                                          , "precision mediump float;"
                                          , "uniform float myUniform;"
                                          , "uniform sampler2D uTex;"
                                          , "varying vec2 fs_uv;"
                                          , "void main() {"
                                          --, "gl_FragColor = vec4(1.0, myUniform, 0.0, 1.0);"
                                          , "float d = myUniform;"
                                          , "gl_FragColor = texture2D(uTex, fs_uv);"
                                          , "}"
                                          ]
          , uniforms: [UFloat "myUniform" 0.5, USampler2D "uTex" 0]
          , attributes: []
          }

main :: Eff (console :: CONSOLE, dom :: DOM, timer :: TIMER, webgl :: WEBGL) Unit
main = do
  ctxM <- fromCanvasId "canvas"
  case ctxM of
    Nothing -> log "Error!"
    Just ctx -> mainWithContext ctx

  where
    mainWithContext ctx = do

      initialState <- init {} ctx $ do
        e1 <- newEntity
        removeEntity e1
        e2 <- newEntity
        e3 <- newEntity
        id <- loadProgram simpleProgram
        pure unit
      
      ecsRun ctx initialState (constant unit) (run >- run)

    run _ = do 
      state <- get
      --e <- newEntity
      --modify incrementIdCounter
      liftEff $ logObject "STATE: " state

-- main = ecsRun (sampleECSRun >-
--                sampleECSRun >- 
--                sampleECSRun >- 
--                sampleECSRun >- 
--                sampleECSRun >- 
--                sampleECSRun) "Context" ecs2 (constant unit)
--log "Other example!"

