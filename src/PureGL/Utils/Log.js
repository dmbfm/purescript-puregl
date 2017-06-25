"use strict";

exports.logObject = function (str) {
  return function (o) {
    return function () {
      console.log(str, o);
    };
  };
};