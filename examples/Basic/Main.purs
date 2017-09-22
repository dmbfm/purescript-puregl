module Example.PureGL.Basic.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.State (get)
import DOM.HTML.HTMLProgressElement (position)
import Data.Dynamic (toDynamic)
import Data.Foldable (foldl, traverse_)
import Data.Lens (view, (.~), (^.))
import Data.Lens.At (at)
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Data.Tree (showTree)
import Math (cos, sin)
import Partial.Unsafe (unsafePartial)
import PureGL (init, run)
import PureGL.Camera (CameraComponent(..), _fov, _projectionMatrix, _viewMatrix, mkCamera, updateCameraSystem)
import PureGL.Camera.LookAt (lookAt)
import PureGL.Context (Context(..), fromCanvasId)
import PureGL.Data.TypedArrays (Float32Array, fromArray)
import PureGL.ECS (_ecsManager, _systemStates)
import PureGL.ECS.Component (getComponent, insertComponent, modifyComponent)
import PureGL.ECS.Entity (newEntity)
import PureGL.Material (MaterialComponent(..), _program, _programId)
import PureGL.Math.Matrix as M
import PureGL.Math.Vector as V
import PureGL.Mesh (addMesh, loadAllMeshGeometries, mkMesh)
import PureGL.Renderer.Geometry (Geometry(..), VertexAttribute(..), mkGeometry)
import PureGL.Renderer.Internal.Program (setUniform)
import PureGL.Renderer.Program (LoadedProgram(..), Program(..), Uniform(..), _uniformLocations, _uniforms, uniformName)
import PureGL.Renderer.RenderResource (loadProgram)
import PureGL.Renderer.RenderState (getLoadedProgram, getRenderer)
import PureGL.Scene (SceneComponent, _position, _root, _sceneState, addSceneComponent, sceneRoot, updateSceneSystem)
import PureGL.Utils.Log (logObject)
import PureGL.Utils.Misc ((>-))
import PureGL.WebGL (clear, clearColor, drawArrays, getUniformLocation)
import PureGL.WebGL.Constants (gl_COLOR_BUFFER_BIT, gl_TRIANGLES)
import PureGL.WebGL.Types (WebGLEff)
import Signal (constant)

unlines :: Array String -> String
unlines lines = foldl (\acc s -> acc <> s <> "\n") "" lines

vShaderSource :: String
vShaderSource = 
  """
    attribute vec3 position;
    uniform mat4 viewMatrix;
    uniform mat4 projectionMatrix;

    void main() {
      gl_Position = projectionMatrix * viewMatrix * vec4(position, 1.0);
    }
  """

fShaderSource :: String
fShaderSource = 
  """
    precision mediump float;

    void main() {
      gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
    }
  """

simpleProgram :: Program
simpleProgram =
  Program { vertexShaderSource: vShaderSource
          , fragmentShaderSource: fShaderSource
          , uniforms: [ UMat4 "viewMatrix" M.identity
                      , UMat4 "projectionMatrix" M.identity
                      ]
          , attributes: []
          }

sceneComponent :: SceneComponent
sceneComponent = { position: V.mkVector3 0.0 0.0 0.0
                 , orientation: one
                 , scale: V.mkVector3 1.0 1.0 1.0
                 , transform: M.identity
                 }

sampleLookAtCamera :: CameraComponent
sampleLookAtCamera = mkCamera 0.8 60.0

triangle :: forall e. WebGLEff e Float32Array
triangle = fromArray [ 0.0, 0.0, 0.0
                     , 0.0, 0.5, 0.0
                     , 0.5, 0.0, 0.0
                     ]

triangleGeo :: forall e. WebGLEff e Geometry
triangleGeo = do
  t <- triangle
  pure $ mkGeometry t [ FloatAttribute 3 ]

main :: forall e. WebGLEff (console :: CONSOLE, ref :: REF | e) Unit
main = do
  log "Basic example"
  contextM <- fromCanvasId "canvas"
  case contextM of
    Nothing -> log "Failed to create context."
    Just context -> main_ context

  where

    main_ :: Context -> WebGLEff (console :: CONSOLE, ref :: REF | e) Unit
    main_ context = do
      
      tri <- newRef 0
      cam <- newRef 1

      -- Initialize the ECS system, starting from a "blank" state
      initialState <- init {  } context $ do
        e <- newEntity
        e' <- newEntity        

        addSceneComponent e  sceneRoot ({ position: V.mkVector3 0.0 0.0 0.0
                                        , orientation: one 
                                        , scale: V.mkVector3 1.0 1.0 1.0
                                        , transform: M.identity
                                        }) 

        addSceneComponent e' sceneRoot ({ position: V.mkVector3 0.0 0.0 2.0
                                        , orientation: one 
                                        , scale: V.mkVector3 1.0 1.0 1.0
                                        , transform: M.identity
                                        })  

        insertComponent (SProxy :: SProxy "camera") e' sampleLookAtCamera

        g <- liftEff $  triangleGeo

        let mesh = mkMesh g

        addMesh e mesh

        loadAllMeshGeometries

        p <- loadProgram simpleProgram
        liftEff $ logObject "p" p

        insertComponent (SProxy :: SProxy "material") e (MaterialComponent { program: simpleProgram
                                                                           , programId: p
                                                                           , loaded: true
                                                                           })

        liftEff $ writeRef tri e
        liftEff $ writeRef cam e'

        pure unit

      logObject "initialState" initialState
      log $ showTree $ view (_ecsManager <<< _systemStates <<< _sceneState <<< _root) initialState

      t <- newRef 0.0

      run context initialState (constant 10) $ (\_ -> updateSceneSystem) >- (\_ -> updateCameraSystem) >- 
        (\_ -> do
          
          

          e <- liftEff $ readRef tri
          e' <- liftEff $ readRef cam

          now <- liftEff $ readRef t

          let x = 0.2 * (sin now)
              z = 0.2 * (cos now)
              y = 4.2 * (cos $ 0.2 * now)
              fov = 70.0 + 10.0 * (sin now)

          modifyComponent (SProxy :: SProxy "camera") e' (_fov .~ fov)

          modifyComponent (SProxy :: SProxy "scene") e' 
            ( _position .~ (V.mkVector3 x y 2.0))

          lookAt e' (V.mkVector3 0.0 0.0 0.0) (V.mkVector3 x y z)

          liftEff $ writeRef t (now + 0.04)


          material <- unsafePartial $ fromJust <$> getComponent (SProxy :: SProxy "material") e
          rs <- getRenderer          
          let loadedProgram = unsafePartial $ fromJust $  getLoadedProgram (material ^. _programId) rs
          
          camera <- unsafePartial $ fromJust <$> getComponent (SProxy :: SProxy "camera") e'
          (flip traverse_) (material ^. _program <<< _uniforms) 
            (\u -> do
              let loc = unsafePartial $ fromJust $ loadedProgram ^. _uniformLocations <<< at (uniformName u)
              --setUniform loc (UMat4 (uniformName u) )
              case (uniformName u) of
                "viewMatrix" -> setUniform loc $ UMat4 "viewMatrix" (camera ^. _viewMatrix)
                --"projectionMatrix" -> setUniform loc $ UMat4 "projectionMatrix" M.identity
                "projectionMatrix" -> setUniform loc $ UMat4 "projectionMatrix" (camera ^. _projectionMatrix)
                otherwise -> pure unit

              pure unit    
            )
          
          clearColor 0.0 0.0 0.0 1.0
          clear gl_COLOR_BUFFER_BIT
          drawArrays gl_TRIANGLES 0 3
          --liftEff $ logObject "state" state
          pure unit
        )
        

      -- run context initialState (constant 10) 
      --   $ (\_ -> liftEff $ log " ok" ) >- (\_ -> liftEff $ log " notok")