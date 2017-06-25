"use strict";

exports.mkVector2 = function (a) {
  return function (b) {
    return new Float32Array([a, b]);
  };
};

exports.addVector2 = function (v1) {
  return function (v2) {
    return function (ref) {
      return function () {
        ref.value[0] = v1[0] + v2[0];
        ref.value[1] = v1[1] + v2[1];
      };
    };
  };
};