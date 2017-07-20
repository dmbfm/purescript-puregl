module PureGL.WebGL.Constants where

-- Constants
gl_BYTE                           = 0x1400 :: Int
gl_UNSIGNED_BYTE                  = 0x1401 :: Int
gl_SHORT                          = 0x1402 :: Int
gl_UNSIGNED_SHORT                 = 0x1403 :: Int
gl_INT                            = 0x1404 :: Int
gl_UNSIGNED_INT                   = 0x1405 :: Int
gl_FLOAT                          = 0x1406 :: Int
gl_SHADER_TYPE                    = 0x8B4F :: Int
gl_DELETE_STATUS                  = 0x8B80 :: Int
gl_LINK_STATUS                    = 0x8B82 :: Int
gl_COMPILE_STATUS                 = 0x8B81 :: Int
gl_FRAGMENT_SHADER                = 0x8B30 :: Int
gl_VERTEX_SHADER                  = 0x8B31 :: Int
gl_ARRAY_BUFFER                   = 0x8892 :: Int
gl_ELEMENT_ARRAY_BUFFER           = 0x8893 :: Int
gl_ARRAY_BUFFER_BINDING           = 0x8894 :: Int
gl_ELEMENT_ARRAY_BUFFER_BINDING   = 0x8895 :: Int
gl_STREAM_DRAW                    = 0x88E0 :: Int
gl_STATIC_DRAW                    = 0x88E4 :: Int
gl_DYNAMIC_DRAW                   = 0x88E8 :: Int
gl_BUFFER_SIZE                    = 0x8764 :: Int
gl_BUFFER_USAGE                   = 0x8765 :: Int
gl_CURRENT_VERTEX_ATTRIB          = 0x8626 :: Int
gl_DEPTH_BUFFER_BIT               = 0x00000100 :: Int
gl_STENCIL_BUFFER_BIT             = 0x00000400 :: Int
gl_COLOR_BUFFER_BIT               = 0x00004000 :: Int
gl_POINTS                         = 0x0000 :: Int
gl_LINES                          = 0x0001 :: Int
gl_LINE_LOOP                      = 0x0002 :: Int
gl_LINE_STRIP                     = 0x0003 :: Int
gl_TRIANGLES                      = 0x0004 :: Int
gl_TRIANGLE_STRIP                 = 0x0005 :: Int
gl_TRIANGLE_FAN                   = 0x0006 :: Int

gl_TEXTURE_MAX_ANISOTROPY_EXT       = 0x84FE :: Int
gl_MAX_TEXTURE_MAX_ANISOTROPY_EXT   = 0x84FF :: Int

 
gl_NEAREST                        = 0x2600 :: Int
gl_LINEAR                         = 0x2601 :: Int

 
gl_NEAREST_MIPMAP_NEAREST         = 0x2700 :: Int
gl_LINEAR_MIPMAP_NEAREST          = 0x2701 :: Int
gl_NEAREST_MIPMAP_LINEAR          = 0x2702 :: Int
gl_LINEAR_MIPMAP_LINEAR           = 0x2703 :: Int

 
gl_TEXTURE_MAG_FILTER             = 0x2800 :: Int
gl_TEXTURE_MIN_FILTER             = 0x2801 :: Int
gl_TEXTURE_WRAP_S                 = 0x2802 :: Int
gl_TEXTURE_WRAP_T                 = 0x2803 :: Int

    
gl_TEXTURE_2D                     = 0x0DE1 :: Int
gl_TEXTURE                        = 0x1702 :: Int

gl_TEXTURE_CUBE_MAP               = 0x8513 :: Int
gl_TEXTURE_BINDING_CUBE_MAP       = 0x8514 :: Int
gl_TEXTURE_CUBE_MAP_POSITIVE_X    = 0x8515 :: Int
gl_TEXTURE_CUBE_MAP_NEGATIVE_X    = 0x8516 :: Int
gl_TEXTURE_CUBE_MAP_POSITIVE_Y    = 0x8517 :: Int
gl_TEXTURE_CUBE_MAP_NEGATIVE_Y    = 0x8518 :: Int
gl_TEXTURE_CUBE_MAP_POSITIVE_Z    = 0x8519 :: Int
gl_TEXTURE_CUBE_MAP_NEGATIVE_Z    = 0x851A :: Int
gl_MAX_CUBE_MAP_TEXTURE_SIZE      = 0x851C :: Int

    
gl_TEXTURE0                       = 0x84C0 :: Int
gl_TEXTURE1                       = 0x84C1 :: Int
gl_TEXTURE2                       = 0x84C2 :: Int
gl_TEXTURE3                       = 0x84C3 :: Int
gl_TEXTURE4                       = 0x84C4 :: Int
gl_TEXTURE5                       = 0x84C5 :: Int
gl_TEXTURE6                       = 0x84C6 :: Int
gl_TEXTURE7                       = 0x84C7 :: Int
gl_TEXTURE8                       = 0x84C8 :: Int
gl_TEXTURE9                       = 0x84C9 :: Int
gl_TEXTURE10                      = 0x84CA :: Int
gl_TEXTURE11                      = 0x84CB :: Int
gl_TEXTURE12                      = 0x84CC :: Int
gl_TEXTURE13                      = 0x84CD :: Int
gl_TEXTURE14                      = 0x84CE :: Int
gl_TEXTURE15                      = 0x84CF :: Int
gl_TEXTURE16                      = 0x84D0 :: Int
gl_TEXTURE17                      = 0x84D1 :: Int
gl_TEXTURE18                      = 0x84D2 :: Int
gl_TEXTURE19                      = 0x84D3 :: Int
gl_TEXTURE20                      = 0x84D4 :: Int
gl_TEXTURE21                      = 0x84D5 :: Int
gl_TEXTURE22                      = 0x84D6 :: Int
gl_TEXTURE23                      = 0x84D7 :: Int
gl_TEXTURE24                      = 0x84D8 :: Int
gl_TEXTURE25                      = 0x84D9 :: Int
gl_TEXTURE26                      = 0x84DA :: Int
gl_TEXTURE27                      = 0x84DB :: Int
gl_TEXTURE28                      = 0x84DC :: Int
gl_TEXTURE29                      = 0x84DD :: Int
gl_TEXTURE30                      = 0x84DE :: Int
gl_TEXTURE31                      = 0x84DF :: Int
gl_ACTIVE_TEXTURE                 = 0x84E0 :: Int

    
gl_REPEAT                         = 0x2901 :: Int
gl_CLAMP_TO_EDGE                  = 0x812F :: Int
gl_MIRRORED_REPEAT                = 0x8370 :: Int

gl_DEPTH_COMPONENT                = 0x1902 :: Int
gl_ALPHA                          = 0x1906 :: Int
gl_RGB                            = 0x1907 :: Int
gl_RGBA                           = 0x1908 :: Int
gl_LUMINANCE                      = 0x1909 :: Int
gl_LUMINANCE_ALPHA                = 0x190A :: Int

gl_UNSIGNED_SHORT_4_4_4_4         = 0x8033 :: Int
gl_UNSIGNED_SHORT_5_5_5_1         = 0x8034 :: Int
gl_UNSIGNED_SHORT_5_6_5           = 0x8363 :: Int

gl_FRAMEBUFFER                    = 0x8D40 :: Int
gl_RENDERBUFFER                   = 0x8D41 :: Int

gl_RGBA4                          = 0x8056 :: Int
gl_RGB5_A1                        = 0x8057 :: Int
gl_RGB565                         = 0x8D62 :: Int
gl_DEPTH_COMPONENT16              = 0x81A5 :: Int
gl_STENCIL_INDEX8                 = 0x8D48 :: Int
gl_DEPTH_STENCIL                  = 0x84F9 :: Int

gl_RENDERBUFFER_WIDTH             = 0x8D42 :: Int
gl_RENDERBUFFER_HEIGHT            = 0x8D43 :: Int
gl_RENDERBUFFER_INTERNAL_FORMAT   = 0x8D44 :: Int
gl_RENDERBUFFER_RED_SIZE          = 0x8D50 :: Int
gl_RENDERBUFFER_GREEN_SIZE        = 0x8D51 :: Int
gl_RENDERBUFFER_BLUE_SIZE         = 0x8D52 :: Int
gl_RENDERBUFFER_ALPHA_SIZE        = 0x8D53 :: Int
gl_RENDERBUFFER_DEPTH_SIZE        = 0x8D54 :: Int
gl_RENDERBUFFER_STENCIL_SIZE      = 0x8D55 :: Int

gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE           = 0x8CD0 :: Int
gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME           = 0x8CD1 :: Int
gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL         = 0x8CD2 :: Int
gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = 0x8CD3 :: Int

gl_COLOR_ATTACHMENT0              = 0x8CE0 :: Int
gl_DEPTH_ATTACHMENT               = 0x8D00 :: Int
gl_STENCIL_ATTACHMENT             = 0x8D20 :: Int
gl_DEPTH_STENCIL_ATTACHMENT       = 0x821A :: Int

gl_NONE                           = 0 :: Int

gl_FRAMEBUFFER_COMPLETE                      = 0x8CD5 :: Int
gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT         = 0x8CD6 :: Int
gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 0x8CD7 :: Int
gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS         = 0x8CD9 :: Int
gl_FRAMEBUFFER_UNSUPPORTED                   = 0x8CDD :: Int

gl_FRAMEBUFFER_BINDING            = 0x8CA6 :: Int
gl_RENDERBUFFER_BINDING           = 0x8CA7 :: Int
gl_MAX_RENDERBUFFER_SIZE          = 0x84E8 :: Int

gl_INVALID_FRAMEBUFFER_OPERATION  = 0x0506 :: Int