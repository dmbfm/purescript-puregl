module PureGL.Framebuffer where

newtype Framebuffer = Framebuffer { depthBuffer :: Boolean
                                  , stencilBuffer :: Boolean
                                  }