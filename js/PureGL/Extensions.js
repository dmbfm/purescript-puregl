exports.initExtensions = ctx => () => { 

  let result = {
    ext_texture_filter_anisotropic: false,
    webgl_debug_renderer_info: false
  };

  if (!ctx) {
    return result;
  }

  if (ctx.getExtension('EXT_texture_filter_anisotropic')) {
    result['ext_texture_filter_anisotropic'] = true;
  }

  if (ctx.getExtension('WEBGL_debug_renderer_info')) {
    result['webgl_debug_renderer_info'] = true;
  }

  return result;

}