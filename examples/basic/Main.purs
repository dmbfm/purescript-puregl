module PureGL.Examples.Basic.Main where

import Prelude
import PureGL.WebGL.Raw

import Control.Monad.Aff (launchAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.State.Class (get)
import Control.Monad.State.Trans (evalStateT, runStateT)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Functor (void)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import PureGL.Context (fromCanvasId)
import PureGL.Data.TypedArrays (fromArray)
import PureGL.Geometry (Geometry(..), VertexAttribute(..), mkGeometry)
import PureGL.Math.Vector (mkVector2)
import PureGL.Program (Program(..))
import PureGL.Renderer (fromContext, loadGeometry, loadProgram, loadTexture, renderGeometry)
import PureGL.Texture (Texture(..), TexturePixels(..), mkTexture'''')
import PureGL.Utils.DOM (loadImage')
import PureGL.Utils.Log (logObject)
import PureGL.WebGL.Types (WEBGL)

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

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, dom :: DOM, exception :: EXCEPTION, webgl :: WEBGL | e) Unit
main = void $ launchAff do
  image <- loadImage' "./image.jpg"
  let texture = mkTexture'''' (HTMLImagePixels image)
  liftEff $ logObject "IMAGE" image
  liftEff $ logObject "TEXTURE" texture
  liftEff $ mainP texture
  pure unit

  where 
  mainP :: Texture -> Eff (console :: CONSOLE, webgl :: WEBGL, dom :: DOM, exception :: EXCEPTION | e) Unit
  mainP tex =  do
    contextM <- fromCanvasId "canvas"
    case contextM of
      Nothing -> log "Error creating WebGL context"
      Just context -> do
        let initialState = fromContext context
        evalStateT run initialState
    
    where
      run = do
        geoId <- loadGeometry triangleGeometry
        progIdE <- loadProgram simpleProgram
        texId <- loadTexture tex
        s <- get
        liftEff $ logObject "STATE0" s
        case progIdE of 
          Left e -> liftEff $ log e
          Right progId -> do
            state <- get
            liftEff $ logObject "STATE" state
            renderGeometry geoId progId
          