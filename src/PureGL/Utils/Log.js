"use strict";

exports.logObject = function (str) {
  return function (o) {
    return function () {
      console.log(str, o);
    };
  };
};
exports.secretLog = function (str) {
  return function (o) {
    return function () {
      console.log(str, o);
    };
  };
};