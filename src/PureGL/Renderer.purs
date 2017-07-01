module PureGL.Renderer where

import Data.Map (Map)
import PureGL.Buffer (BufferResource)
import PureGL.Context (Context)

type RenderState = { context :: Context
                   , loadedBuffers :: Map Int BufferResource
                   , idCounter :: Int
                   , idPool :: Array Int
                   }


