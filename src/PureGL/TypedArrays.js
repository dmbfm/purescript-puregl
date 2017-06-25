"use strict";

exports.mkArrayBuffer = function (len) {
  return new ArrayBuffer(len);
};

exports.fromArrayFloat32Array = function (arr) {
  return new Float32Array(arr);
};
exports.toArrayFloat32Array = function (f) {
  return Array.from(f);
};
exports.lengthFloat32Array = function (f) {
  return f.length;
};
exports.getBufferFloat32Array = function (f) {
  return f.buffer;
};
exports.byteLengthFloat32Array = function (f) {
  return f.byteLength;
};
exports.fillFloat32Array = function (f) {
  return function (v) {
    return function () {
      return f.fill(v);
    };
  };
};
exports.toStringFloat32Array = function (f) {
  return f.toString();
};
exports.getAtFloat32Array = function (f) {
  return function (i) {
    return typeof f[i] === "undefined" ? null : f[i];
  };
};
exports.setAtFloat32Array = function (f) {
  return function (i) {
    return function (a) {
      return function () {
        f[i] = a;
      };
    };
  };
};
exports.fromBufferFloat32Array = function (b) {
  return new Float32Array(b);
};
exports.fromBufferFloat32Array2 = function (b) {
  return function (o) {
    return function (l) {
      return new Float32Array(b, o, l);
    };
  };
};

exports.fromArrayInt8Array = function (arr) {
  return new Int8Array(arr);
};
exports.toArrayInt8Array = function (f) {
  return Array.from(f);
};
exports.lengthInt8Array = function (f) {
  return f.length;
};
exports.getBufferInt8Array = function (f) {
  return f.buffer;
};
exports.byteLengthInt8Array = function (f) {
  return f.byteLength;
};
exports.fillInt8Array = function (f) {
  return function (v) {
    return function () {
      return f.fill(v);
    };
  };
};
exports.toStringInt8Array = function (f) {
  return f.toString();
};
exports.getAtInt8Array = function (f) {
  return function (i) {
    return typeof f[i] === "undefined" ? null : f[i];
  };
};
exports.setAtInt8Array = function (f) {
  return function (i) {
    return function (a) {
      return function () {
        f[i] = a;
      };
    };
  };
};
exports.fromBufferInt8Array = function (b) {
  return new Int8Array(b);
};
exports.fromBufferInt8Array2 = function (b) {
  return function (o) {
    return function (l) {
      return new Int8Array(b, o, l);
    };
  };
};

exports.fromArrayInt16Array = function (arr) {
  return new Int16Array(arr);
};
exports.toArrayInt16Array = function (f) {
  return Array.from(f);
};
exports.lengthInt16Array = function (f) {
  return f.length;
};
exports.getBufferInt16Array = function (f) {
  return f.buffer;
};
exports.byteLengthInt16Array = function (f) {
  return f.byteLength;
};
exports.fillInt16Array = function (f) {
  return function (v) {
    return function () {
      return f.fill(v);
    };
  };
};
exports.toStringInt16Array = function (f) {
  return f.toString();
};
exports.getAtInt16Array = function (f) {
  return function (i) {
    return typeof f[i] === "undefined" ? null : f[i];
  };
};
exports.setAtInt16Array = function (f) {
  return function (i) {
    return function (a) {
      return function () {
        f[i] = a;
      };
    };
  };
};
exports.fromBufferInt16Array = function (b) {
  return new Int16Array(b);
};
exports.fromBufferInt16Array2 = function (b) {
  return function (o) {
    return function (l) {
      return new Int16Array(b, o, l);
    };
  };
};