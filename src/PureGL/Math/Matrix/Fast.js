"use strict";

exports.mkFMatrix2 = function (a) {
  return function (b) {
    return function (c) {
      return function (d) {
        return function () {
          return new Float32Array([a, b, c, d]);
        };
      };
    };
  };
};
exports.mkFMatrix3 = function (a) {
  return function (b) {
    return function (c) {
      return function (d) {
        return function (e) {
          return function (f) {
            return function (g) {
              return function (h) {
                return function (i) {
                  return function () {
                    return new Float32Array([a, b, c, d, e, f, g, h, i]);
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
exports.mkFMatrix4 = function (a00) {
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
                                return function () {
                                  return new Float32Array([a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, a22, a23, a30, a31, a32, a33]);
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
};
exports._toStringFMatrix = function (m) {
  return m.toString();
};
exports._toFloat32ArrayFMatrix = function (a) {
  return function () {
    return new Float32Array(a);
  };
};
exports._toFloat32ArrayFMatrixUnsafe = function (a) {
  return function () {
    return a;
  };
};
exports._fromArrayFMatrix = function (arr) {
  return function () {
    return new Float32Array(arr);
  };
};

// FMatrix2: 
exports.addFMatrix2 = function (a) {
  return function (b) {
    return function (out) {
      return function () {
        var m = out.value;

        m[0] = a[0] + b[0];
        m[1] = a[1] + b[1];
        m[2] = a[2] + b[2];
        m[3] = a[3] + b[3];
      };
    };
  };
};
exports.subFMatrix2 = function (a) {
  return function (b) {
    return function (out) {
      return function () {
        var m = out.value;

        m[0] = a[0] - b[0];
        m[1] = a[1] - b[1];
        m[2] = a[2] - b[2];
        m[3] = a[3] - b[3];
      };
    };
  };
};

exports.invFMatrix2 = function (a) {
  return function (out) {
    return function () {
      out[0] = -a[0];
      out[1] = -a[1];
      out[2] = -a[2];
      out[3] = -a[3];
    };
  };
};
exports.mulFMatrix2 = function (x) {
  return function (a) {
    return function (out) {
      return function () {
        out[0] = x * a[0];
        out[1] = x * a[1];
        out[2] = x * a[2];
        out[3] = x * a[3];
      };
    };
  };
};
exports.identityFMatrix2 = function (out) {
  return function () {
    out[0] = 1.0;out[1] = 0.0;
    out[2] = 0.0;out[3] = 1.0;
  };
};

exports.transposeFMatrix2 = function (a) {
  return function (out) {
    return function () {

      var a00 = a[0],
          a01 = a[1],
          a10 = a[2],
          a11 = a[3];

      out[0] = a00;
      out[1] = a10;
      out[2] = a01;
      out[3] = a11;
    };
  };
};
exports.mulMatrixFMatrix2 = function (a) {
  return function (b) {
    return function (out) {
      return function () {

        var a00 = a[0],
            a01 = a[1],
            a10 = a[2],
            a11 = a[3];

        var b00 = b[0],
            b01 = b[1],
            b10 = b[2],
            b11 = b[3];

        out[0] = a00 * b00 + a01 * b10; // m00
        out[1] = a00 * b01 + a01 * b11; // m01
        out[2] = a10 * b00 + a11 * b10; // m10
        out[3] = a10 * b01 + a11 * b11; // m11
      };
    };
  };
};
exports.determinantFMatrix2 = function (m) {
  return function () {
    return m[0] * m[3] - m[1] * m[2];
  };
};
exports.invertFMatrix2 = function (a) {
  return function (out) {
    return function () {

      var det = exports.determinantFMatrix2(a)();

      if (det == 0) {
        return false;
      }

      var x = 1 / det;

      out[0] = x * a[3];
      out[1] = -x * a[1];
      out[2] = -x * a[2];
      out[3] = x * a[0];

      return true;
    };
  };
};

// FMatrix3: 
exports.addFMatrix3 = function (a) {
  return function (b) {
    return function (out) {
      return function () {
        out[0] = a[0] + b[0];
        out[1] = a[1] + b[1];
        out[2] = a[2] + b[2];
        out[3] = a[3] + b[3];
        out[4] = a[4] + b[4];
        out[5] = a[5] + b[5];
        out[6] = a[6] + b[6];
        out[7] = a[7] + b[7];
        out[8] = a[8] + b[8];
      };
    };
  };
};
exports.subFMatrix3 = function (a) {
  return function (b) {
    return function (out) {
      return function () {
        out[0] = a[0] - b[0];
        out[1] = a[1] - b[1];
        out[2] = a[2] - b[2];
        out[3] = a[3] - b[3];
        out[4] = a[4] - b[4];
        out[5] = a[5] - b[5];
        out[6] = a[6] - b[6];
        out[7] = a[7] - b[7];
        out[8] = a[8] - b[8];
      };
    };
  };
};

exports.invFMatrix3 = function (a) {
  return function (out) {
    return function () {
      out[0] = -a[0];
      out[1] = -a[1];
      out[2] = -a[2];
      out[3] = -a[3];
      out[4] = -a[4];
      out[5] = -a[5];
      out[6] = -a[6];
      out[7] = -a[7];
      out[8] = -a[8];
    };
  };
};
exports.mulFMatrix3 = function (x) {
  return function (a) {
    return function (out) {
      return function () {
        out[0] = x * a[0];
        out[1] = x * a[1];
        out[2] = x * a[2];
        out[3] = x * a[3];
        out[4] = x * a[4];
        out[5] = x * a[5];
        out[6] = x * a[6];
        out[7] = x * a[7];
        out[8] = x * a[8];
      };
    };
  };
};
exports.identityFMatrix3 = function (out) {
  return function () {
    out[0] = 1.0;out[1] = 0.0;out[2] = 0.0;
    out[3] = 0.0;out[4] = 1.0;out[5] = 0.0;
    out[6] = 0.0;out[7] = 0.0;out[8] = 1.0;
  };
};

exports.transposeFMatrix3 = function (a) {
  return function (out) {
    return function () {
      var a00 = a[0],
          a01 = a[1],
          a02 = a[2],
          a10 = a[3],
          a11 = a[4],
          a12 = a[5],
          a20 = a[6],
          a21 = a[7],
          a22 = a[8];

      out[0] = a00;
      out[1] = a10;
      out[2] = a20;
      out[3] = a01;
      out[4] = a11;
      out[5] = a21;
      out[6] = a02;
      out[7] = a12;
      out[8] = a22;
    };
  };
};
exports.mulMatrixFMatrix3 = function (a) {
  return function (b) {
    return function (out) {
      return function () {
        var a00 = a[0],
            a01 = a[1],
            a02 = a[2],
            a10 = a[3],
            a11 = a[4],
            a12 = a[5],
            a20 = a[6],
            a21 = a[7],
            a22 = a[8];

        var b00 = b[0],
            b01 = b[1],
            b02 = b[2],
            b10 = b[3],
            b11 = b[4],
            b12 = b[5],
            b20 = b[6],
            b21 = b[7],
            b22 = b[8];

        out[0] = a00 * b00 + a01 * b10 + a02 * b20; // m00
        out[1] = a00 * b01 + a01 * b11 + a02 * b21; // m01
        out[2] = a00 * b02 + a01 * b12 + a02 * b22; // m02

        out[3] = a10 * b00 + a11 * b10 + a12 * b20; // m10
        out[4] = a10 * b01 + a11 * b11 + a12 * b21; // m11
        out[5] = a10 * b02 + a11 * b12 + a12 * b22; // m12

        out[6] = a20 * b00 + a21 * b10 + a22 * b20; // m20
        out[7] = a20 * b01 + a21 * b11 + a22 * b21; // m21
        out[8] = a20 * b02 + a21 * b12 + a22 * b22; // m22
      };
    };
  };
};
exports.determinantFMatrix3 = function (m) {
  return function () {
    return m[0] * (m[4] * m[8] - m[5] * m[7]) - m[1] * (m[3] * m[8] - m[5] * m[6]) + m[2] * (m[3] * m[7] - m[4] * m[6]);
  };
};
exports.invertFMatrix3 = function (m) {
  return function (out) {
    return function () {
      var a00 = m[0],
          a01 = m[1],
          a02 = m[2],
          a10 = m[3],
          a11 = m[4],
          a12 = m[5],
          a20 = m[6],
          a21 = m[7],
          a22 = m[7];

      var det = exports.determinantFMatrix3(m)();

      if (det == 0) {
        return false;
      }

      var a = 1.0 / det;

      out[0] = (a11 * a22 - a12 * a21) * a;
      out[1] = (a02 * a21 - a01 * a22) * a;
      out[2] = (a01 * a12 - a02 * a11) * a;
      out[3] = (a12 * a20 - a10 * a22) * a;
      out[4] = (a00 * a22 - a02 * a20) * a;
      out[5] = (a02 * a10 - a00 * a12) * a;
      out[6] = (a10 * a21 - a11 * a20) * a;
      out[7] = (a01 * a20 - a00 * a21) * a;
      out[8] = (a00 * a11 - a01 * a10) * a;

      return true;
    };
  };
};

// FMatrix4: 
exports.addFMatrix4 = function (a) {
  return function (b) {
    return function (out) {
      return function () {
        out[0] = a[0] + b[0];
        out[1] = a[1] + b[1];
        out[2] = a[2] + b[2];
        out[3] = a[3] + b[3];
        out[4] = a[4] + b[4];
        out[5] = a[5] + b[5];
        out[6] = a[6] + b[6];
        out[7] = a[7] + b[7];
        out[8] = a[8] + b[8];
        out[9] = a[9] + b[0];
        out[10] = a[10] + b[10];
        out[11] = a[11] + b[11];
        out[12] = a[12] + b[12];
        out[13] = a[13] + b[13];
        out[14] = a[14] + b[14];
        out[15] = a[15] + b[15];
      };
    };
  };
};
exports.subFMatrix4 = function (a) {
  return function (b) {
    return function (out) {
      return function () {
        out[0] = a[0] - b[0];
        out[1] = a[1] - b[1];
        out[2] = a[2] - b[2];
        out[3] = a[3] - b[3];
        out[4] = a[4] - b[4];
        out[5] = a[5] - b[5];
        out[6] = a[6] - b[6];
        out[7] = a[7] - b[7];
        out[8] = a[8] - b[8];
        out[9] = a[9] - b[0];
        out[10] = a[10] - b[10];
        out[11] = a[11] - b[11];
        out[12] = a[12] - b[12];
        out[13] = a[13] - b[13];
        out[14] = a[14] - b[14];
        out[15] = a[15] - b[15];
      };
    };
  };
};

exports.invFMatrix4 = function (a) {
  return function (out) {
    return function () {
      out[0] = -a[0];
      out[1] = -a[1];
      out[2] = -a[2];
      out[3] = -a[3];
      out[4] = -a[4];
      out[5] = -a[5];
      out[6] = -a[6];
      out[7] = -a[7];
      out[8] = -a[8];
      out[9] = -a[9];
      out[10] = -a[10];
      out[11] = -a[11];
      out[12] = -a[12];
      out[13] = -a[13];
      out[14] = -a[14];
      out[15] = -a[15];
    };
  };
};
exports.mulFMatrix4 = function (x) {
  return function (a) {
    return function (out) {
      return function () {
        out[0] = x * a[0];
        out[1] = x * a[1];
        out[2] = x * a[2];
        out[3] = x * a[3];
        out[4] = x * a[4];
        out[5] = x * a[5];
        out[6] = x * a[6];
        out[7] = x * a[7];
        out[8] = x * a[8];
        out[9] = x * a[9];
        out[10] = x * a[10];
        out[11] = x * a[11];
        out[12] = x * a[12];
        out[13] = x * a[13];
        out[14] = x * a[14];
        out[15] = x * a[15];
      };
    };
  };
};
exports.identityFMatrix4 = function (out) {
  return function () {
    out[0] = 1.0;out[1] = 0.0;out[2] = 0.0;out[3] = 0.0;
    out[4] = 0.0;out[5] = 1.0;out[6] = 0.0;out[7] = 0.0;
    out[8] = 0.0;out[9] = 0.0;out[10] = 1.0;out[11] = 0.0;
    out[12] = 0.0;out[13] = 0.0;out[14] = 0.0;out[15] = 1.0;
  };
};

exports.transposeFMatrix4 = function (a) {
  return function (out) {
    return function () {
      var a00 = a[0],
          a01 = a[1],
          a02 = a[2],
          a03 = a[3];
      a10 = a[4], a11 = a[5], a12 = a[6], a13 = a[7];
      a20 = a[8], a21 = a[9], a22 = a[10], a23 = a[11];
      a30 = a[12], a31 = a[13], a32 = a[14], a33 = a[15];

      out[0] = a00;
      out[1] = a10;
      out[2] = a20;
      out[3] = a30;
      out[4] = a01;
      out[5] = a11;
      out[6] = a21;
      out[7] = a31;
      out[8] = a02;
      out[9] = a12;
      out[10] = a22;
      out[11] = a32;
      out[12] = a03;
      out[13] = a13;
      out[14] = a23;
      out[15] = a33;
    };
  };
};
exports.mulMatrixFMatrix4 = function (a) {
  return function (b) {
    return function (out) {
      return function () {
        var a00 = a[0],
            a01 = a[1],
            a02 = a[2],
            a03 = a[3];
        a10 = a[4], a11 = a[5], a12 = a[6], a13 = a[7];
        a20 = a[8], a21 = a[9], a22 = a[10], a23 = a[11];
        a30 = a[12], a31 = a[13], a32 = a[14], a33 = a[15];

        var b00 = b[0],
            b01 = b[1],
            b02 = b[2],
            b03 = b[3];
        b10 = b[4], b11 = b[5], b12 = b[6], b13 = b[7];
        b20 = b[8], b21 = b[9], b22 = b[10], b23 = b[11];
        b30 = b[12], b31 = b[13], b32 = b[14], b33 = b[15];

        out[0] = a00 * b00 + a01 * b10 + a02 * b20 + a03 * b30; // m00
        out[1] = a00 * b01 + a01 * b11 + a02 * b21 + a03 * b31; // m01
        out[2] = a00 * b02 + a01 * b12 + a02 * b22 + a03 * b32; // m02
        out[3] = a00 * b03 + a01 * b13 + a02 * b23 + a03 * b33; // m03

        out[4] = a10 * b00 + a11 * b10 + a12 * b20 + a13 * b30; // m10
        out[5] = a10 * b01 + a11 * b11 + a12 * b21 + a13 * b31; // m11
        out[6] = a10 * b02 + a11 * b12 + a12 * b22 + a13 * b32; // m12
        out[7] = a10 * b03 + a11 * b13 + a12 * b23 + a13 * b33; // m13

        out[8] = a20 * b00 + a21 * b10 + a22 * b20 + a23 * b30; // m20
        out[9] = a20 * b01 + a21 * b11 + a22 * b21 + a23 * b31; // m21
        out[10] = a20 * b02 + a21 * b12 + a22 * b22 + a23 * b32; // m22
        out[11] = a20 * b03 + a21 * b13 + a22 * b23 + a23 * b33; // m23

        out[12] = a30 * b00 + a31 * b10 + a32 * b20 + a33 * b30; // m30
        out[13] = a30 * b01 + a31 * b11 + a32 * b21 + a33 * b31; // m31
        out[14] = a30 * b02 + a31 * b12 + a32 * b22 + a33 * b32; // m32
        out[15] = a30 * b03 + a31 * b13 + a32 * b23 + a33 * b33; // m33
      };
    };
  };
};
exports.determinantFMatrix4 = function (a) {
  return function () {
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
};
exports.invertFMatrix4 = function (a) {
  return function (out) {
    return function () {
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
        return false;
      }
      det = 1.0 / det;

      out[0] = (a11 * b11 - a12 * b10 + a13 * b09) * det;
      out[1] = (a02 * b10 - a01 * b11 - a03 * b09) * det;
      out[2] = (a31 * b05 - a32 * b04 + a33 * b03) * det;
      out[3] = (a22 * b04 - a21 * b05 - a23 * b03) * det;
      out[4] = (a12 * b08 - a10 * b11 - a13 * b07) * det;
      out[5] = (a00 * b11 - a02 * b08 + a03 * b07) * det;
      out[6] = (a32 * b02 - a30 * b05 - a33 * b01) * det;
      out[7] = (a20 * b05 - a22 * b02 + a23 * b01) * det;
      out[8] = (a10 * b10 - a11 * b08 + a13 * b06) * det;
      out[9] = (a01 * b08 - a00 * b10 - a03 * b06) * det;
      out[10] = (a30 * b04 - a31 * b02 + a33 * b00) * det;
      out[11] = (a21 * b02 - a20 * b04 - a23 * b00) * det;
      out[12] = (a11 * b07 - a10 * b09 - a12 * b06) * det;
      out[13] = (a00 * b09 - a01 * b07 + a02 * b06) * det;
      out[14] = (a31 * b01 - a30 * b03 - a32 * b00) * det;
      out[16] = (a20 * b03 - a21 * b01 + a22 * b00) * det;

      return true;
    };
  };
};

exports.orthographic = function (r) {
  return function (l) {
    return function (t) {
      return function (b) {
        return function (n) {
          return function (f) {
            return new Float32Array([2 / (r - l), 0, 0, -(r + l) / (r - l), 0, 2 / (t - b), 0, -(t + b) / (t - b), 0, 0, -2 / (f - n), -(f + n) / (f - n), 0, 0, 0, 1]);
          };
        };
      };
    };
  };
};

exports.orthographic2 = function (w) {
  return function (h) {
    return function (n) {
      return function (f) {
        return new Float32Array([2 / w, 0, 0, 0, 0, 2 / h, 0, 0, 0, 0, -2 * (f - n), -(f + n) / (f - n), 0, 0, 0, 1]);
      };
    };
  };
};

exports.perspective = function (r) {
  return function (l) {
    return function (t) {
      return function (b) {
        return function (n) {
          return function (f) {
            return new Float32Array([2 * n / (r - l), 0, (r + l) / (r - l), 0, 0, 2 * n / (t - b), (t + b) / (t - b), 0, 0, 0, -(f + n) / (f - n), -2 * f * n / (f - n), 0, 0, -1, 0]);
          };
        };
      };
    };
  };
};

exports.perspective2 = function (w) {
  return function (h) {
    return function (n) {
      return function (f) {
        return new Float32Array([2 * n / w, 0, 0, 0, 0, 2 * n / h, 0, 0, 0, 0, -(f + n) / (f - n), -2 * f * n / (f - n), 0, 0, -1, 0]);
      };
    };
  };
};

exports.perspective3 = function (ratio) {
  return function (fov) {
    return function (n) {
      return function (f) {
        var S = n / f * (1 / Math.tan(fov / 2));
        return new Float32Array([S, 0, 0, 0, 0, ratio * S, 0, 0, 0, 0, -(f + n) / (f - n), -2 * f * n / (f - n), 0, 0, -1, 0]);
      };
    };
  };
};

exports.translation = function (x) {
  return function (y) {
    return function (z) {
      return function (out) {
        return function () {
          out[0] = 1.0;out[1] = 0.0;out[2] = 0.0;out[3] = x;
          out[4] = 0.0;out[5] = 1.0;out[6] = 0.0;out[7] = y;
          out[8] = 0.0;out[9] = 0.0;out[10] = 1.0;out[11] = z;
          out[12] = 0.0;out[13] = 0.0;out[14] = 0.0;out[15] = 1.0;
        };
      };
    };
  };
};

exports.translation2 = function (v) {
  return function (out) {
    return function () {
      exports.translation(v[0])(v[1])(v[2])(out)();
    };
  };
};

exports.mkTranslation = function (x) {
  return function (y) {
    return function (z) {
      return function () {
        new Float32Array([1.0, 0.0, 0.0, x, 0.0, 1, 0, 0.0, y, 0.0, 0.0, 1.9, z, 0.0, 0.0, 0.0, 1.0]);
      };
    };
  };
};

exports.mkScale = function (x) {
  return function (y) {
    return function (z) {
      return new Float32Array([x, 0.0, 0.0, 0.0, 0.0, y, 0.0, 0.0, 0.0, 0.0, z, 0.0, 0.0, 0.0, 0.0, 1.0]);
    };
  };
};

exports._mkScale2 = function (v) {
  return exports.mkScale(v[0])(v[1])(v[2]);
};

exports.mkRotateZ = function (a) {

  var x = a * (Math.PI / 180);
  var s = Math.sin(x);
  var c = Math.cos(x);

  return new Float32Array([c, -s, 0.0, 0.0, s, c, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0]);
};

exports.mkRotateY = function (a) {

  var x = a * (Math.PI / 180);
  var s = Math.sin(x);
  var c = Math.cos(x);

  return new Float32Array([c, 0.0, s, 0.0, 0.0, 1.0, 0.0, 0.0, -s, 0.0, c, 0.0, 0.0, 0.0, 0.0, 1.0]);
};

exports.mkRotateX = function (a) {

  var x = a * (Math.PI / 180);
  var s = Math.sin(x);
  var c = Math.cos(x);

  return new Float32Array([1.0, 0.0, 0.0, 0.0, 0.0, c, -s, 0.0, 0.0, s, c, 0.0, 0.0, 0.0, 0.0, 1.0]);
};

exports.mkRotation = function (v) {
  return function (a) {

    var norm = Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
    var ux = v[0] / norm;
    var uy = v[1] / norm;
    var uz = v[2] / norm;
    var x = a * (Math.PI / 180);
    var s = Math.sin(x);
    var c = Math.cos(x);

    return new Float32Array([c + (1 - c) * ux * ux, (1 - c) * ux * uy - s * uz, (1 - c) * ux * uz + s * uy, 0.0, (1 - c) * ux * uy + s * uz, c + (1 - c) * uy * uy, (1 - c) * uy * uz - s * ux, 0.0, (1 - c) * ux * uz - s * uy, (1 - c) * uy * uz + s * ux, c + (1 - c) * uz * uz, 0.0, 0.0, 0.0, 0.0, 1.0]);
  };
};