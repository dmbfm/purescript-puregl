"use strict";

//----
var fromArray = function fromArray(n) {
  return function (arr) {
    var out = new Array(n);
    if (arr.length < n) {
      for (var i = 0; i < arr.length; i++) {
        out[i] = arr[i];
      }

      for (var _i = arr.length; _i < n; _i++) {
        out[_i] = 0;
      }
    } else {
      for (var _i2 = 0; _i2 < n; _i2++) {
        out[_i2] = arr[_i2];
      }
    }
    return out;
  };
};

exports._toStringMatrix = function (m) {
  return m.toString();
};
//----
exports.mkMatrix2 = function (a) {
  return function (b) {
    return function (c) {
      return function (d) {
        return [a, b, c, d];
      };
    };
  };
};
exports.eqMatrix2 = function (m1) {
  return function (m2) {
    return m1[0] == m2[0] && m1[1] == m2[1] && m1[2] == m2[2] && m1[3] == m2[3];
  };
};
exports.addMatrix2 = function (m1) {
  return function (m2) {
    return [m1[0] + m2[0], m1[1] + m2[1], m1[2] + m2[2], m1[3] + m2[3]];
  };
};
exports.subMatrix2 = function (m1) {
  return function (m2) {
    return [m1[0] - m2[0], m1[1] - m2[1], m1[2] - m2[2], m1[3] - m2[3]];
  };
};
exports.transposeMatrix2 = function (m) {
  return [m[0], m[2], m[1], m[3]];
};
exports.getArrayMatrix2 = function (m) {
  return m;
};
exports.scalarMulMatrix2 = function (a) {
  return function (m) {
    return [a * m[0], a * m[1], a * m[2], a * m[3]];
  };
};
exports.mulMatrix2 = function (m1) {
  return function (m2) {
    return [m1[0] * m2[0] + m1[1] * m2[2], m1[0] * m2[1] + m1[1] * m2[3], m1[2] * m2[0] + m1[3] * m2[2], m1[2] * m2[1] + m1[3] * m2[3]];
  };
};
exports.determinantMatrix2 = function (m) {
  return m[0] * m[3] - m[1] * m[2];
};
exports.invertMatrix2 = function (m) {

  var det = exports.determinantMatrix2(m);

  if (det == 0) {
    return null;
  }

  var a = 1 / det;

  return [a * m[3], -a * m[1], -a * m[2], a * m[0]];
};
exports.fromArrayMatrix2 = fromArray(4);
exports._toFloat32Array = function (m) {
  return new toFloat32Array(m);
};

exports.mkMatrix3 = function (a) {
  return function (b) {
    return function (c) {
      return function (d) {
        return function (e) {
          return function (f) {
            return function (g) {
              return function (h) {
                return function (i) {
                  return [a, b, c, d, e, f, g, h, i];
                };
              };
            };
          };
        };
      };
    };
  };
};
exports.eqMatrix3 = function (m1) {
  return function (m2) {
    return m1[0] == m2[0] && m1[1] == m2[1] && m1[2] == m2[2] && m1[3] == m2[3] && m1[4] == m2[4] && m1[5] == m2[5] && m1[6] == m2[6] && m1[7] == m2[7] && m1[8] == m2[8];
  };
};
exports.addMatrix3 = function (m1) {
  return function (m2) {
    return [m1[0] + m2[0], m1[1] + m2[1], m1[2] + m2[2], m1[3] + m2[3], m1[4] + m2[4], m1[5] + m2[5], m1[6] + m2[6], m1[7] + m2[7], m1[8] + m2[8]];
  };
};
exports.subMatrix3 = function (m1) {
  return function (m2) {
    return [m1[0] - m2[0], m1[1] - m2[1], m1[2] - m2[2], m1[3] - m2[3], m1[4] - m2[4], m1[5] - m2[5], m1[6] - m2[6], m1[7] - m2[7], m1[8] - m2[8]];
  };
};
exports.transposeMatrix3 = function (m) {
  return [m[0], m[3], m[6], m[1], m[4], m[7], m[2], m[5], m[8]];
};
exports.getArrayMatrix3 = function (m) {
  return m;
};
exports.scalarMulMatrix3 = function (a) {
  return function (m) {
    return [a * m[0], a * m[1], a * m[2], a * m[3], a * m[4], a * m[5], a * m[6], a * m[7], a * m[8]];
  };
};
exports.mulMatrix3 = function (m1) {
  return function (m2) {
    return [m1[0] * m2[0] + m1[1] * m2[3] + m1[2] * m2[6], m1[0] * m2[1] + m1[1] * m2[4] + m1[2] * m2[7], m1[0] * m2[2] + m1[1] * m2[5] + m1[2] * m2[8], m1[3] * m2[0] + m1[4] * m2[3] + m1[5] * m2[6], m1[3] * m2[1] + m1[4] * m2[4] + m1[5] * m2[7], m1[3] * m2[2] + m1[4] * m2[5] + m1[5] * m2[8], m1[6] * m2[0] + m1[7] * m2[3] + m1[8] * m2[6], m1[6] * m2[1] + m1[7] * m2[4] + m1[8] * m2[7], m1[6] * m2[2] + m1[7] * m2[5] + m1[8] * m2[8]];
  };
};
exports.determinantMatrix3 = function (m) {
  return m[0] * (m[4] * m[8] - m[5] * m[7]) - m[1] * (m[3] * m[8] - m[5] * m[6]) + m[2] * (m[3] * m[7] - m[4] * m[6]);
};
exports.invertMatrix3 = function (m) {

  var a00 = m[0],
      a01 = m[1],
      a02 = m[2],
      a10 = m[3],
      a11 = m[4],
      a12 = m[5],
      a20 = m[6],
      a21 = m[7],
      a22 = m[7];

  var det = exports.determinantMatrix3(m);

  if (det == 0) {
    return null;
  }

  var a = 1.0 / det;

  return [(a11 * a22 - a12 * a21) * a, (a02 * a21 - a01 * a22) * a, (a01 * a12 - a02 * a11) * a, (a12 * a20 - a10 * a22) * a, (a00 * a22 - a02 * a20) * a, (a02 * a10 - a00 * a12) * a, (a10 * a21 - a11 * a20) * a, (a01 * a20 - a00 * a21) * a, (a00 * a11 - a01 * a10) * a];
};
exports.fromArrayMatrix3 = fromArray(9);

exports.mkMatrix4 = function (a00) {
  return function (a01) {
    return function (a02) {
      return function (a03) {
        return function (a10) {
          return function (a11) {
            return function (a12) {
              return function (a13) {
                return function (a20) {
                  return function (a21) {
                    return function (a22) {
                      return function (a23) {
                        return function (a30) {
                          return function (a31) {
                            return function (a32) {
                              return function (a33) {
                                return [a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, a22, a23, a30, a31, a32, a33];
                              };
                            };
                          };
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.eqMatrix4 = function (m1) {
  return function (m2) {
    return m1[0] == m2[0] && m1[1] == m2[1] && m1[2] == m2[2] && m1[3] == m2[3] && m1[4] == m2[4] && m1[5] == m2[5] && m1[6] == m2[6] && m1[7] == m2[7] && m1[8] == m2[8] && m1[9] == m2[9] && m1[10] == m2[10] && m1[11] == m2[11] && m1[12] == m2[12] && m1[13] == m2[13] && m1[14] == m2[14] && m1[15] == m2[15];
  };
};

exports.addMatrix4 = function (m1) {
  return function (m2) {
    return [m1[0] + m2[0], m1[1] + m2[1], m1[2] + m2[2], m1[3] + m2[3], m1[4] + m2[4], m1[5] + m2[5], m1[6] + m2[6], m1[7] + m2[7], m1[8] + m2[8], m1[9] + m2[9], m1[10] + m2[10], m1[11] + m2[11], m1[12] + m2[12], m1[13] + m2[13], m1[14] + m2[14], m1[15] + m2[15]];
  };
};

exports.subMatrix4 = function (m1) {
  return function (m2) {
    return [m1[0] - m2[0], m1[1] - m2[1], m1[2] - m2[2], m1[3] - m2[3], m1[4] - m2[4], m1[5] - m2[5], m1[6] - m2[6], m1[7] - m2[7], m1[8] - m2[8], m1[9] - m2[9], m1[10] - m2[10], m1[11] - m2[11], m1[12] - m2[12], m1[13] - m2[13], m1[14] - m2[14], m1[15] - m2[15]];
  };
};

exports.transposeMatrix4 = function (m) {
  return [m[0], m[4], m[8], m[12], m[1], m[5], m[9], m[13], m[2], m[6], m[10], m[14], m[3], m[7], m[11], m[15]];
};
exports.getArrayMatrix4 = function (m) {
  return m;
};
exports.scalarMulMatrix4 = function (a) {
  return function (m) {
    return [a * m[0], a * m[1], a * m[2], a * m[3], a * m[4], a * m[5], a * m[6], a * m[7], a * m[8], a * m[9], a * m[10], a * m[11], a * m[12], a * m[13], a * m[14], a * m[15]];
  };
};

exports.mulMatrix4 = function (m1) {
  return function (m2) {

    var a00 = m1[0],
        a01 = m1[1],
        a02 = m1[2],
        a03 = m1[3],
        a10 = m1[4],
        a11 = m1[5],
        a12 = m1[6],
        a13 = m1[7],
        a20 = m1[8],
        a21 = m1[9],
        a22 = m1[10],
        a23 = m1[11],
        a30 = m1[12],
        a31 = m1[13],
        a32 = m1[14],
        a33 = m1[15];

    var b00 = m2[0],
        b01 = m2[1],
        b02 = m2[2],
        b03 = m2[3],
        b10 = m2[4],
        b11 = m2[5],
        b12 = m2[6],
        b13 = m2[7],
        b20 = m2[8],
        b21 = m2[9],
        b22 = m2[10],
        b23 = m2[11],
        b30 = m2[12],
        b31 = m2[13],
        b32 = m2[14],
        b33 = m2[15];

    return [a00 * b00 + a01 * b10 + a02 * b20 + a03 * b30, a00 * b01 + a01 * b11 + a02 * b21 + a03 * b31, a00 * b02 + a01 * b12 + a02 * b22 + a03 * b32, a00 * b03 + a01 * b13 + a02 * b23 + a03 * b33, a10 * b00 + a11 * b10 + a12 * b20 + a13 * b30, a10 * b01 + a11 * b11 + a12 * b21 + a13 * b31, a10 * b02 + a11 * b12 + a12 * b22 + a13 * b32, a10 * b03 + a11 * b13 + a12 * b23 + a13 * b33, a20 * b00 + a21 * b10 + a22 * b20 + a23 * b30, a20 * b01 + a21 * b11 + a22 * b21 + a23 * b31, a20 * b02 + a21 * b12 + a22 * b22 + a23 * b32, a20 * b03 + a21 * b13 + a22 * b23 + a23 * b33, a30 * b00 + a31 * b10 + a32 * b20 + a33 * b30, a30 * b01 + a31 * b11 + a32 * b21 + a33 * b31, a30 * b02 + a31 * b12 + a32 * b22 + a33 * b32, a30 * b03 + a31 * b13 + a32 * b23 + a33 * b33];
  };
};

exports.determinantMatrix4 = function (a) {

  var a00 = a[0],
      a01 = a[1],
      a02 = a[2],
      a03 = a[3],
      a10 = a[4],
      a11 = a[5],
      a12 = a[6],
      a13 = a[7],
      a20 = a[8],
      a21 = a[9],
      a22 = a[10],
      a23 = a[11],
      a30 = a[12],
      a31 = a[13],
      a32 = a[14],
      a33 = a[15],
      b00 = a00 * a11 - a01 * a10,
      b01 = a00 * a12 - a02 * a10,
      b02 = a00 * a13 - a03 * a10,
      b03 = a01 * a12 - a02 * a11,
      b04 = a01 * a13 - a03 * a11,
      b05 = a02 * a13 - a03 * a12,
      b06 = a20 * a31 - a21 * a30,
      b07 = a20 * a32 - a22 * a30,
      b08 = a20 * a33 - a23 * a30,
      b09 = a21 * a32 - a22 * a31,
      b10 = a21 * a33 - a23 * a31,
      b11 = a22 * a33 - a23 * a32;

  return b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06;
};

exports.invertMatrix4 = function (a) {
  var a00 = a[0],
      a01 = a[1],
      a02 = a[2],
      a03 = a[3],
      a10 = a[4],
      a11 = a[5],
      a12 = a[6],
      a13 = a[7],
      a20 = a[8],
      a21 = a[9],
      a22 = a[10],
      a23 = a[11],
      a30 = a[12],
      a31 = a[13],
      a32 = a[14],
      a33 = a[15],
      b00 = a00 * a11 - a01 * a10,
      b01 = a00 * a12 - a02 * a10,
      b02 = a00 * a13 - a03 * a10,
      b03 = a01 * a12 - a02 * a11,
      b04 = a01 * a13 - a03 * a11,
      b05 = a02 * a13 - a03 * a12,
      b06 = a20 * a31 - a21 * a30,
      b07 = a20 * a32 - a22 * a30,
      b08 = a20 * a33 - a23 * a30,
      b09 = a21 * a32 - a22 * a31,
      b10 = a21 * a33 - a23 * a31,
      b11 = a22 * a33 - a23 * a32,
      det = b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06;

  if (!det) {
    return null;
  }
  det = 1.0 / det;

  return [(a11 * b11 - a12 * b10 + a13 * b09) * det, (a02 * b10 - a01 * b11 - a03 * b09) * det, (a31 * b05 - a32 * b04 + a33 * b03) * det, (a22 * b04 - a21 * b05 - a23 * b03) * det, (a12 * b08 - a10 * b11 - a13 * b07) * det, (a00 * b11 - a02 * b08 + a03 * b07) * det, (a32 * b02 - a30 * b05 - a33 * b01) * det, (a20 * b05 - a22 * b02 + a23 * b01) * det, (a10 * b10 - a11 * b08 + a13 * b06) * det, (a01 * b08 - a00 * b10 - a03 * b06) * det, (a30 * b04 - a31 * b02 + a33 * b00) * det, (a21 * b02 - a20 * b04 - a23 * b00) * det, (a11 * b07 - a10 * b09 - a12 * b06) * det, (a00 * b09 - a01 * b07 + a02 * b06) * det, (a31 * b01 - a30 * b03 - a32 * b00) * det, (a20 * b03 - a21 * b01 + a22 * b00) * det];
};
exports.fromArrayMatrix4 = fromArray(16);