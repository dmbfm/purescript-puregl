module PureGL.Examples.Basic.Main where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT, get, runStateT)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import PureGL.Context (fromCanvasId)
import PureGL.Data.TypedArrays (fromArray)
import PureGL.Geometry (Geometry(..), VertexAttribute(..), mkGeometry)
import PureGL.Program (Program(..), Uniform(..))
import PureGL.RenderResource (loadGeometry, loadProgram)
import PureGL.RenderState (RenderT, fromContext)
import PureGL.Renderer (renderGeometry)
import PureGL.Utils.Log (logObject)
import PureGL.WebGL.Types (WebGLEff)

unlines :: Array String -> String
unlines lines = foldl (\acc s -> acc <> s <> "\n") "" lines

triangleGeometry :: Geometry
triangleGeometry = mkGeometry (fromArray triangleArray) [ FloatAttribute 3 ]
  where triangleArray = [ 0.0, 0.0, 0.0 
                        , 0.0, 0.5, 0.0
                        , 0.5, 0.0, 0.0
                        ]

simpleProgram :: Program
simpleProgram = 
  Program { vertexShaderSource: unlines   [ ""
                                          , "attribute vec3 position;"
                                          , "void main() {"
                                          , "gl_Position = vec4(position, 1.0);"
                                          , "}"
                                          ]
          , fragmentShaderSource: unlines [ ""
                                          , "precision mediump float;"                                          
                                          , "void main() {"
                                          , "gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);"
                                          , "}"
                                          ]
          , uniforms: []
          , attributes: []
          }

main :: forall eff. WebGLEff (console :: CONSOLE, dom :: DOM | eff) Unit
main = do
  contextM <- fromCanvasId "canvas"
  case contextM of
    Nothing -> log "Error creating WebGL context."
    Just context -> do
      let initialState = fromContext context      
      r <-  evalStateT (runReaderT (runExceptT mainP) context) initialState
      case r of
        Left e -> logObject "RenderError" e
        Right _ -> log "No RenderError!"

  where

    mainP :: RenderT (console :: CONSOLE, dom :: DOM | eff) Unit
    mainP = do
      geoId <- loadGeometry triangleGeometry
      progId <- loadProgram simpleProgram
      state <- get
      liftEff $ logObject "State0" state
      renderGeometry geoId progId

-- main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, dom :: DOM, exception :: EXCEPTION, webgl :: WEBGL | e) Unit
-- main = void $ launchAff do
--   image <- loadImage' "./image.jpg"
--   let texture = mkTexture'''' (HTMLImagePixels image)
--   liftEff $ logObject "IMAGE" image
--   liftEff $ logObject "TEXTURE" texture
--   liftEff $ mainP texture
--   pure unit

--   where 
--   mainP :: Texture -> Eff (console :: CONSOLE, webgl :: WEBGL, dom :: DOM, exception :: EXCEPTION | e) Unit
--   mainP tex =  do
--     contextM <- fromCanvasId "canvas"
--     case contextM of
--       Nothing -> log "Error creating WebGL context"
--       Just context -> do
--         let initialState = fromContext context
--         evalStateT run initialState
    
--     where
--       run = do
--         geoId <- loadGeometry triangleGeometry
--         progIdE <- loadProgram simpleProgram
--         texId <- loadTexture tex
--         s <- get
--         liftEff $ logObject "STATE0" s
--         case progIdE of 
--           Left e -> liftEff $ log e
--           Right progId -> do
--             state <- get
--             liftEff $ logObject "STATE" state
--             renderGeometry geoId progId
          
