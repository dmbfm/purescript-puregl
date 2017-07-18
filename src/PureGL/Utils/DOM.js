'use strict';

exports._getCanvasElement = function (id) {
  return function () {
    return document.getElementById(id);
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

exports.loadImage = function (url) {
  return function (cb) {
    return function () {

      var image = new Image();

      image.onload = function () {
        cb(image)();
      };
      image.src = url;
    };
  };
};