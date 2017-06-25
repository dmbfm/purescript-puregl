"use strict";

exports.mkDataView = function (b) {
  return new DataView(b);
};

exports._mkDataView2 = function (b) {
  return function (o) {
    return function (l) {
      return new DataView(b, o, l);
    };
  };
};
exports._setter = function (name) {
  return function (end) {
    return function (view) {
      return function (offset) {
        return function (val) {
          return function () {
            view[name](offset, val, end);
          };
        };
      };
    };
  };
};
exports._getter = function (name) {
  return function (len) {
    return function (end) {
      return function (view) {
        return function (offset) {
          return function () {
            return offset + len <= view.byteLength ? view[name](offset, end) : null;
          };
        };
      };
    };
  };
};