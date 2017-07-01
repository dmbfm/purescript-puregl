exports._getCanvasElement = id => () => document.getElementbyId(id);
exports._getWebGL1Context = canvas => () => canvas.getContext('webgl');
exports._getWebGL2Context = canvas => () => canvas.getContext('webgl2');