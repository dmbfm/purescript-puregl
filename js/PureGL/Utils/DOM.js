exports._getCanvasElement = id => () => document.getElementById(id);
exports._getWebGL1Context = canvas => () => canvas.getContext('webgl');
exports._getWebGL2Context = canvas => () => canvas.getContext('webgl2');

exports.loadImage = url => cb => () => {

  let image = new Image();

  image.onload = () => { cb(image)(); };
  image.src = url;

};