module PureGL.Types where

type ResourceId = Int 

-- | Possible Renderer error types
data RenderError = 
    DefaultError 
  | LookupResourceError ResourceId
  | OtherErrors

