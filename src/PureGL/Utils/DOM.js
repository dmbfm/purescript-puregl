'use strict';

exports._getCanvasElement = function (id) {
  return function () {
    return document.getElementbyId(id);
  };
};
exports._getWebGL1Context = function (canvas) {
  return function () {
    return canvas.getContext('webgl');
  };
};
exports._getWebGL2Context = function (canvas) {
  return function () {
    return canvas.getContext('webgl2');
  };
};