//----
const fromArray = n => arr => {
  let out = new Array(n);
  
  for (let i = 0; i < n; i++) {
    out[i] = arr[i] || 0;
  }
  return out;
}

exports._toStringMatrix = m => m.toString();

exports.mkMatrix2 = a00 => a01 => a10 => a11 => [a00, a10, a01, a11];

exports.eqMatrix2 = m1 => m2 => 
  (m1[0] == m2[0]) &&
  (m1[1] == m2[1]) &&
  (m1[2] == m2[2]) &&
  (m1[3] == m2[3]);

exports.addMatrix2 = m1 => m2 => 
  [ m1[0] + m2[0]
  , m1[1] + m2[1]
  , m1[2] + m2[2]
  , m1[3] + m2[3]];
  
exports.subMatrix2 = m1 => m2 => 
  [ m1[0] - m2[0]
  , m1[1] - m2[1]
  , m1[2] - m2[2]
  , m1[3] - m2[3]];
  
exports.transposeMatrix2 = m => [m[0], m[2], m[1], m[3]];

exports.getArrayMatrix2 = m => m;

exports.scalarMulMatrix2 = a => m => [a * m[0], a * m[1], a * m[2], a * m[3]];

exports.mulMatrix2 = m1 => m2 => {
  let
    a00 = m1[0], a10 = m1[1], a01 = m1[2], a11 = m1[3],
    b00 = m2[0], b10 = m2[1], b01 = m2[2], b11 = m2[3];
  
  return (
    [
      a00 * b00 + a01 * b10, // c00
      a10 * b00 + a11 * b10, // c10
      a00 * b01 + a01 * b11, // c01
      a10 * b01 + a11 * b11  // c11
    ]
  );
}
exports.determinantMatrix2 = m => m[0] * m[3] - m[2] * m[1];

exports.invertMatrix2 = m => {

  let det = exports.determinantMatrix2(m);

  if (det == 0) {
    return null;
  }

  let a = 1 / det;

  return [a * m[3], -a * m[1], -a * m[2], a * m[0]];
}

exports.fromArrayMatrix2 = fromArray(4)

exports._toFloat32Array = m => () => new Float32Array(m);

exports.mkMatrix3 = a00 => a01 => a02 => a10 => a11 => a12 => a20 => a21 => a22 =>
  [a00, a10, a20, a01, a11, a21, a02, a12, a22];  

exports.eqMatrix3 = m1 => m2 => 
  (m1[0] == m2[0]) &&
  (m1[1] == m2[1]) &&
  (m1[2] == m2[2]) &&
  (m1[3] == m2[3]) &&
  (m1[4] == m2[4]) &&
  (m1[5] == m2[5]) &&
  (m1[6] == m2[6]) &&
  (m1[7] == m2[7]) &&
  (m1[8] == m2[8]);
exports.addMatrix3 = m1 => m2 => 
  [ m1[0] + m2[0]
  , m1[1] + m2[1]
  , m1[2] + m2[2]
  , m1[3] + m2[3] 
  , m1[4] + m2[4]
  , m1[5] + m2[5]
  , m1[6] + m2[6]
  , m1[7] + m2[7] 
  , m1[8] + m2[8] 
  ];
exports.subMatrix3 = m1 => m2 => 
  [ m1[0] - m2[0]
  , m1[1] - m2[1]
  , m1[2] - m2[2]
  , m1[3] - m2[3] 
  , m1[4] - m2[4]
  , m1[5] - m2[5]
  , m1[6] - m2[6]
  , m1[7] - m2[7] 
  , m1[8] - m2[8] 
  ];
exports.transposeMatrix3 = m => 
  [m[0], m[3], m[6], m[1], m[4], m[7], m[2], m[5], m[8]];
  
exports.getArrayMatrix3 = m => m;

exports.scalarMulMatrix3 = a => m => 
  [ a * m[0], a * m[1], a * m[2]
  , a * m[3], a * m[4], a * m[5]
  , a * m[6], a * m[7], a * m[8]
  ];

exports.mulMatrix3 = m1 => m2 => {
  let
    a00 = m1[0], a10 = m1[1], a20 = m1[2],
    a01 = m1[3], a11 = m1[4], a21 = m1[5],
    a02 = m1[6], a12 = m1[7], a22 = m1[8],

    b00 = m2[0], b10 = m2[1], b20 = m2[2],
    b01 = m2[3], b11 = m2[4], b21 = m2[5],
    b02 = m2[6], b12 = m2[7], b22 = m2[8];
  
  return (
    [
      a00 * b00 + a01 * b10 + a02 * b20, // c00 
      a10 * b00 + a11 * b10 + a12 * b20, // c10 
      a20 * b00 + a21 * b10 + a22 * b20, // c20 

      a00 * b01 + a01 * b11 + a02 * b21, // c01 
      a10 * b01 + a11 * b11 + a12 * b21, // c11 
      a20 * b01 + a21 * b11 + a22 * b21, // c21 

      a00 * b02 + a01 * b12 + a02 * b22, // c02 
      a10 * b02 + a11 * b12 + a12 * b22, // c12 
      a20 * b02 + a21 * b12 + a22 * b22, // c22 
    ]
  );
}

exports.determinantMatrix3 = m =>   
  m[0] * (m[4] * m[8] - m[5] * m[7]) -
  m[1] * (m[3] * m[8] - m[5] * m[6]) +
  m[2] * (m[3] * m[7] - m[4] * m[6]);

exports.invertMatrix3 = m => {

  let a00 = m[0], a01 = m[3], a02 = m[6],
      a10 = m[1], a11 = m[4], a12 = m[7],
      a20 = m[2], a21 = m[5], a22 = m[8];

  let det = exports.determinantMatrix3(m);

  if (det == 0) {
    return null;
  }

  let a = 1.0 / det;

  return (
    [
        (a11 * a22 - a12 * a21) * a
      , (a12 * a20 - a10 * a22) * a
      , (a10 * a21 - a11 * a20) * a

      , (a02 * a21 - a01 * a22) * a
      , (a00 * a22 - a02 * a20) * a
      , (a01 * a20 - a00 * a21) * a

      , (a01 * a12 - a02 * a11) * a
      , (a02 * a10 - a00 * a12) * a
      , (a00 * a11 - a01 * a10) * a
    ]
  );
}
exports.fromArrayMatrix3 = fromArray(9)

exports.mkMatrix4 =
  a00 => a01 => a02 => a03 =>
    a10 => a11 => a12 => a13 =>
      a20 => a21 => a22 => a23 =>
        a30 => a31 => a32 => a33 =>
          [
            a00, a10, a20, a30,
            a01, a11, a21, a31,
            a02, a12, a22, a32,
            a03, a13, a23, a33
          ];     

exports.eqMatrix4 = m1 => m2 => 
  (m1[0] == m2[0]) &&
  (m1[1] == m2[1]) &&
  (m1[2] == m2[2]) &&
  (m1[3] == m2[3]) &&
  (m1[4] == m2[4]) &&
  (m1[5] == m2[5]) &&
  (m1[6] == m2[6]) &&
  (m1[7] == m2[7]) &&
  (m1[8] == m2[8]) &&
  (m1[9] == m2[9]) &&
  (m1[10] == m2[10]) &&
  (m1[11] == m2[11]) &&
  (m1[12] == m2[12]) &&
  (m1[13] == m2[13]) &&
  (m1[14] == m2[14]) &&
  (m1[15] == m2[15]);

exports.addMatrix4 = m1 => m2 => 
  [ m1[0]  + m2[0], m1[1]   + m2[1], m1[2]   + m2[2], m1[3]   + m2[3]
  , m1[4]  + m2[4], m1[5]   + m2[5], m1[6]   + m2[6], m1[7]   + m2[7]
  , m1[8]  + m2[8], m1[9]   + m2[9], m1[10]  + m2[10], m1[11] + m2[11]
  , m1[12] + m2[12], m1[13] + m2[13], m1[14] + m2[14], m1[15] + m2[15]
  ];

exports.subMatrix4 = m1 => m2 => 
  [ m1[0]  - m2[0], m1[1]   - m2[1], m1[2]   - m2[2], m1[3]   - m2[3]
  , m1[4]  - m2[4], m1[5]   - m2[5], m1[6]   - m2[6], m1[7]   - m2[7]
  , m1[8]  - m2[8], m1[9]   - m2[9], m1[10]  - m2[10], m1[11] - m2[11]
  , m1[12] - m2[12], m1[13] - m2[13], m1[14] - m2[14], m1[15] - m2[15]
  ];

exports.transposeMatrix4 = m => 
  [ m[0], m[4], m[8], m[12]
  , m[1], m[5], m[9], m[13]
  , m[2], m[6], m[10], m[14]
  , m[3], m[7], m[11], m[15]
  ];

exports.getArrayMatrix4 = m => m;

exports.scalarMulMatrix4 = a => m => 
  [ a * m[0], a * m[1], a * m[2], a * m[3]
  , a * m[4], a * m[5], a * m[6], a * m[7]
  , a * m[8], a * m[9], a * m[10], a * m[11]
  , a * m[12], a * m[13], a * m[14], a * m[15]
  ];

exports.mulMatrix4 = m1 => m2 => {

  let a00 = m1[0], a01 = m1[4], a02 = m1[8], a03 = m1[12],
      a10 = m1[1], a11 = m1[5], a12 = m1[9], a13 = m1[13],
      a20 = m1[2], a21 = m1[6], a22 = m1[10], a23 = m1[14],
      a30 = m1[3], a31 = m1[7], a32 = m1[11], a33 = m1[15];
  
  let b00 = m2[0], b01 = m2[4], b02 = m2[8],  b03 = m2[12],
      b10 = m2[1], b11 = m2[5], b12 = m2[9],  b13 = m2[13],
      b20 = m2[2], b21 = m2[6], b22 = m2[10], b23 = m2[14],
      b30 = m2[3], b31 = m2[7], b32 = m2[11], b33 = m2[15];

  return (
    [
      a00 * b00 + a01 * b10 + a02 * b20 + a03 * b30,
      a10 * b00 + a11 * b10 + a12 * b20 + a13 * b30,
      a20 * b00 + a21 * b10 + a22 * b20 + a23 * b30,
      a30 * b00 + a31 * b10 + a32 * b20 + a33 * b30,

      a00 * b01 + a01 * b11 + a02 * b21 + a03 * b31,
      a10 * b01 + a11 * b11 + a12 * b21 + a13 * b31,
      a20 * b01 + a21 * b11 + a22 * b21 + a23 * b31,
      a30 * b01 + a31 * b11 + a32 * b21 + a33 * b31,

      a00 * b02 + a01 * b12 + a02 * b22 + a03 * b32,
      a10 * b02 + a11 * b12 + a12 * b22 + a13 * b32,
      a20 * b02 + a21 * b12 + a22 * b22 + a23 * b32,
      a30 * b02 + a31 * b12 + a32 * b22 + a33 * b32,

      a00 * b03 + a01 * b13 + a02 * b23 + a03 * b33,
      a10 * b03 + a11 * b13 + a12 * b23 + a13 * b33,
      a20 * b03 + a21 * b13 + a22 * b23 + a23 * b33,
      a30 * b03 + a31 * b13 + a32 * b23 + a33 * b33
    ]
  );

}

exports.determinantMatrix4 = a => {

    var a00 = a[0], a01 = a[1], a02 = a[2], a03 = a[3],
        a10 = a[4], a11 = a[5], a12 = a[6], a13 = a[7],
        a20 = a[8], a21 = a[9], a22 = a[10], a23 = a[11],
        a30 = a[12], a31 = a[13], a32 = a[14], a33 = a[15],

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
}

exports.invertMatrix4 = a => {
  let
    a00 = a[0], a01 = a[4], a02 = a[8], a03 = a[12],
    a10 = a[1], a11 = a[5], a12 = a[9], a13 = a[13],
    a20 = a[2], a21 = a[6], a22 = a[10], a23 = a[14],
    a30 = a[3], a31 = a[7], a32 = a[11], a33 = a[15];

  let
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

    return (
      [
        (a11 * b11 - a12 * b10 + a13 * b09) * det
        , (a12 * b08 - a10 * b11 - a13 * b07) * det
        , (a10 * b10 - a11 * b08 + a13 * b06) * det
        , (a11 * b07 - a10 * b09 - a12 * b06) * det

        , (a02 * b10 - a01 * b11 - a03 * b09) * det
        , (a00 * b11 - a02 * b08 + a03 * b07) * det
        , (a01 * b08 - a00 * b10 - a03 * b06) * det
        , (a00 * b09 - a01 * b07 + a02 * b06) * det

        , (a31 * b05 - a32 * b04 + a33 * b03) * det
        , (a32 * b02 - a30 * b05 - a33 * b01) * det
        , (a30 * b04 - a31 * b02 + a33 * b00) * det
        , (a31 * b01 - a30 * b03 - a32 * b00) * det

        , (a22 * b04 - a21 * b05 - a23 * b03) * det
        , (a20 * b05 - a22 * b02 + a23 * b01) * det
        , (a21 * b02 - a20 * b04 - a23 * b00) * det
        , (a20 * b03 - a21 * b01 + a22 * b00) * det


      ]
    );
}
exports.fromArrayMatrix4 = fromArray(16);

exports.mkOrtho = r => l => t => b => n => f =>
  [
    2 / (r - l), 0, 0, 0,
    0, 2 / (t - b), 0, 0,
    0, 0, -2 / (f - n), 0,
    - (r + l) / (r - l), - (t + b) / (t - b), -(f + n) / (f - n), 1
    
  ];

exports.mkOrtho2 = w => h => n => f =>
  [
    2 / w, 0, 0, 0,
    0, 2 / h, 0, 0,
    0, 0, -2 * (f - n), 0,
    0, 0, -(f + n) / (f - n), 1
  ];

exports.mkPerspective = r => l => t => b => n => f =>
  [
    2 * n / (r - l), 0, 0, 0,
    0, 2 * n / (t - b), 0, 0,
    (r + l) / (r - l), (t + b) / (t - b), - (f + n) / (f - n), -1, 
    0, 0, -2 * f * n / (f - n), 0
  ];

exports.mkPerspective2 = w => h => n => f =>
  [
    2 * n / w, 0, 0, 0,
    0, 2 * n / h, 0, 0,
    0, 0, - (f + n) / (f - n), -1,
    0, 0, -2 * f * n / (f - n), 0
  ];

exports.mkPerspective3 = ratio => fov => n => f => {

  let S = 1 / Math.tan((fov / 2) * (Math.PI / 180));

  return (
    [
      S, 0, 0, 0,
      0, ratio * S, 0, 0,
      0, 0, - (f + n) / (f - n), -1,
      0, 0, -2 * f * n / (f - n), 0
    ]
  );

  // let S = (n / f) * (1 / Math.tan(fov / 2));
  
  // return (
  //   [
  //     S, 0, 0, 0,
  //     0, ratio * S, 0, 0,
  //     0, 0, - (f + n) / (f - n), -1,
  //     0, 0, -2 * f * n / (f - n), 0
  //   ]
  // );
}

exports.applyTransform = m => v => {  
  return {
    x: m[0] * v.x + m[4] * v.y + m[8] * v.z + m[12] * v.w,
    y: m[1] * v.x + m[5] * v.y + m[9] * v.z + m[13] * v.w,
    z: m[2] * v.x + m[6] * v.y + m[10] * v.z + m[14] * v.w,
    w: m[3] * v.x + m[7] * v.y + m[11] * v.z + m[15] * v.w,
  }
}



////  

exports.translate = v => m => {
  let b30 = m[3], b31 = m[7], b32 = m[11], b33 = m[15];
  
  return (
    [
      m[0] + v.x * b30,
      m[1] + v.y * b30,
      m[2] + v.z * b30,
      b30,

      m[4] + v.x * b31,
      m[5] + v.y * b31,
      m[6] + v.z * b31,
      b31,

      m[8] + v.x * b32,
      m[9] + v.y * b32,
      m[10] + v.z * b32,
      b32,

      m[12] + v.x * b33,
      m[13] + v.y * b33,
      m[14] + v.z * b33,
      b33
    ]
  );
}

exports.rotate = v => a => m => {

  let c = Math.cos(a * (Math.PI / 180.0));
  let s = Math.sin(a * (Math.PI / 180.0));
  let n = Math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
  let u = [v.x / n, v.y / n, v.z / n];

  let
    a00 = c + (1 - c) * u[0] * u[0],
    a01 = (1 - c) * u[0] * u[1] - s * u[2],
    a02 = (1 - c) * u[0] * u[2] + s * u[1],

    a10 = (1 - c) * u[0] * u[1] + s * u[2],
    a11 = c + (1 - c) * u[1] * u[1],
    a12 = (1 - c) * u[1] * u[2] - s * u[0],

    a20 = (1 - c) * u[0] * u[2] - s * u[1],
    a21 = (1 - c) * u[1] * u[2] + s * u[0],
    a22 = c + (1 - c) * u[2] * u[2];

  let
    b00 = m[0], b01 = m[4], b02 = m[8], b03 = m[12],
    b10 = m[1], b11 = m[5], b12 = m[9], b13 = m[13],
    b20 = m[2], b21 = m[6], b22 = m[10], b23 = m[14];
  
  return (
    [
      a00 * b00 + a01 * b10 + a02 * b20,
      a10 * b00 + a11 * b10 + a12 * b20,
      a20 * b00 + a21 * b10 + a22 * b20,
      m[3],

      a00 * b01 + a01 * b11 + a02 * b21,
      a10 * b01 + a11 * b11 + a12 * b21,
      a20 * b01 + a21 * b11 + a22 * b21,
      m[7],

      a00 * b02 + a01 * b12 + a02 * b22,
      a10 * b02 + a11 * b12 + a12 * b22,
      a20 * b02 + a21 * b12 + a22 * b22,
      m[11],

      a00 * b03 + a01 * b13 + a02 * b23,
      a10 * b03 + a11 * b13 + a12 * b23,
      a20 * b03 + a21 * b13 + a22 * b23,
      m[15]
    ]
  );
}


exports.rotate2 = q => m => {
  
  let n = Math.sqrt(q.a * q.a + q.b * q.b + q.c * q.c + q.d * q.d);
  let x = q.b / n;
  let y = q.c / n;
  let z = q.d / n;
  let w = q.a / n;

  let xx2 = 2 * x * x;
  let yy2 = 2 * y * y;
  let zz2 = 2 * z * z;
  //let u = [q.a / n, q.b / n, q.c / n, q.d / n];

  let
    a00 = 1 - yy2 - zz2,
    a01 = 2 * x * y - 2 * w * z,
    a02 = 2 * x * z + 2 * w * y,

    a10 = 2 * x * y + 2 * w * z,
    a11 = 1 - xx2 - zz2,
    a12 = 2 * y * z - 2 * w * x,

    a20 = 2 * x * z - 2 * w * y,
    a21 = 2 * y * z + 2 * w * x,
    a22 = 1 - xx2 - yy2;

  let
    b00 = m[0], b01 = m[4], b02 = m[8], b03 = m[12],
    b10 = m[1], b11 = m[5], b12 = m[9], b13 = m[13],
    b20 = m[2], b21 = m[6], b22 = m[10], b23 = m[14];

  return (
    [
      a00 * b00 + a01 * b10 + a02 * b20,
      a10 * b00 + a11 * b10 + a12 * b20,
      a20 * b00 + a21 * b10 + a22 * b20,
      m[3],

      a00 * b01 + a01 * b11 + a02 * b21,
      a10 * b01 + a11 * b11 + a12 * b21,
      a20 * b01 + a21 * b11 + a22 * b21,
      m[7],

      a00 * b02 + a01 * b12 + a02 * b22,
      a10 * b02 + a11 * b12 + a12 * b22,
      a20 * b02 + a21 * b12 + a22 * b22,
      m[11],

      a00 * b03 + a01 * b13 + a02 * b23,
      a10 * b03 + a11 * b13 + a12 * b23,
      a20 * b03 + a21 * b13 + a22 * b23,
      m[15]
    ]
  );
}

exports.projectOrtho = r => l => t => b => n => f => m => {

  let
    a00 = 2 / (r - l),
    a03 = - (r + l) / (r - l),
    a11 = 2 / (t - b),
    a13 = - (t + b) / (t - b),
    a22 = -2 / (f - n),
    a23 = -(f + n) / (f - n),
    a33 = 1;

  let b30 = m[3], b31 = m[7], b32 = m[11], b33 = m[15];

  return (
    [
      a00 * m[0] + a03 * b30,
      a11 * m[1] + a13 * b30,
      a22 * m[2] + a23 * b30,
      a33 * b30,

      a00 * m[4] + a03 * b31,
      a11 * m[5] + a13 * b31,
      a22 * m[6] + a23 * b31,
      a33 * b31,

      a00 * m[8] + a03 * b32,
      a11 * m[9] + a13 * b32,
      a22 * m[10] + a23 * b32,
      a33 * b32,

      a00 * m[12] + a03 * b33,
      a11 * m[13] + a13 * b33,
      a22 * m[14] + a23 * b33,
      a33 * b33
    ]
  );
}


exports.projectOrtho2 = w => h => n => f => m => {

  let
    a00 = 2 / w,
    a11 = 2 / h,
    a22 = -2 / (f - n),
    a23 = -(f + n) / (f - n),
    a33 = 1;

  let b30 = m[3], b31 = m[7], b32 = m[11], b33 = m[15];

  return (
    [
      a00 * m[0],
      a11 * m[1],
      a22 * m[2] + a23 * b30,
      a33 * b30,

      a00 * m[4],
      a11 * m[5],
      a22 * m[6] + a23 * b31,
      a33 * b31,

      a00 * m[8],
      a11 * m[9],
      a22 * m[10] + a23 * b32,
      a33 * b32,

      a00 * m[12],
      a11 * m[13],
      a22 * m[14] + a23 * b33,
      a33 * b33
    ]
  );
}


exports.projectPerspective = r => l => t => b => n => f => m => {

  let
    a00 = 2 * n / (r - l),      //m1[0],
    a02 = (r + l) / (r - l),    //m1[8],
    a11 = 2 * n / (t - b),      //m1[5],
    a12 = (t + b) / (t - b),    //m1[9],
    a22 = - (f + n) / (f - n),  //m1[10],
    a23 = -2 * f * n / (f - n), //m1[14],
    a32 = -1;                   //m1[11];

  let b20 = [2]; b21 = m[6], b22 = m[10], b23 = m[14];

  return (
    [
      a00 * m[0] + a02 * b20,
      a11 * m[1] + a12 * b20,
      a22 * b20 + a23 * m[3],
      a32 * b20,

      a00 * m[4] + a02 * b21,
      a11 * m[5] + a12 * b21,
      a22 * b21 + a23 * m[7],
      a32 * b21,

      a00 * m[8] + a02 * b22,
      a11 * m[9] + a12 * b22,
      a22 * b22 + a23 * m[11],
      a32 * b22,

      a00 * m[12] + a02 * b23,
      a11 * m[13] + a12 * b23,
      a22 * b23 + a23 * m[15],
      a32 * b23
    ]
  );

}

exports.projectPerspective2 = w => h => n => f => m => {
  let
    a00 = 2 * n / w,      //m1[0],
    a11 = 2 * n / h,      //m1[5],
    a22 = - (f + n) / (f - n),  //m1[10],
    a23 = -2 * f * n / (f - n), //m1[14],
    a32 = -1;                   //m1[11];

  let b20 = [2]; b21 = m[6], b22 = m[10], b23 = m[14];

  return (
    [
      a00 * m[0],
      a11 * m[1],
      a22 * b20 + a23 * m[3],
      a32 * b20,

      a00 * m[4],
      a11 * m[5],
      a22 * b21 + a23 * m[7],
      a32 * b21,

      a00 * m[8],
      a11 * m[9],
      a22 * b22 + a23 * m[11],
      a32 * b22,

      a00 * m[12],
      a11 * m[13],
      a22 * b23 + a23 * m[15],
      a32 * b23
    ]
  );
}


exports.projectPerspective3 = ratio => fov => n => f => m => {
  let S = (n / f) * (1 / Math.tan(fov / 2));

  let
    a00 = S,      //m1[0],
    a11 = ratio * S,      //m1[5],
    a22 = - (f + n) / (f - n),  //m1[10],
    a23 = -2 * f * n / (f - n), //m1[14],
    a32 = -1;                   //m1[11];

  let b20 = [2]; b21 = m[6], b22 = m[10], b23 = m[14];

  return (
    [
      a00 * m[0],
      a11 * m[1],
      a22 * b20 + a23 * m[3],
      a32 * b20,

      a00 * m[4],
      a11 * m[5],
      a22 * b21 + a23 * m[7],
      a32 * b21,

      a00 * m[8],
      a11 * m[9],
      a22 * b22 + a23 * m[11],
      a32 * b22,

      a00 * m[12],
      a11 * m[13],
      a22 * b23 + a23 * m[15],
      a32 * b23
    ]
  );
}

exports.scale = s => m => {

  let
    a00 = s.x * s.x,
    a11 = s.y * s.y,
    a22 = s.z * s.z,
    a33 = 1.0;
  
  return (
    [
      a00 * m[0],
      a11 * m[1],
      a22 * m[2],
      a33 * m[3],
      a00 * m[4],
      a11 * m[5],
      a22 * m[6],
      a33 * m[7],
      a00 * m[8],
      a11 * m[9],
      a22 * m[10],
      a33 * m[11],
      a00 * m[12],
      a11 * m[13],
      a22 * m[14],
      a33 * m[15]
    ]
  );

}
