exports.mkFMatrix2 = a => b => c => d => new Float32Array([a, b, c, d]);
exports.mkFMatrix3 = a => b => c => d => e => f => g => h => i =>  
  new Float32Array([a, b, c, d, e, f, g, h, i]);
exports.mkFMatrix4 = a00 => a01 => a02 => a03 =>
                    a10 => a11 => a12 => a13 =>
                    a20 => a21 => a22 => a23 =>
                    a30 => a31 => a32 => a33 => 
                    new Float32Array([ a00, a01, a02, a03
                    , a10, a11, a12, a13
                    , a20, a21, a22, a23
                    , a30, a31, a32, a33 ]);
exports._toStringFMatrix = m => m.toString();
exports._toFloat32ArrayFMatrix = a => a;
exports._fromArrayFMatrix = arr => new Float32Array(arr); 
exports.eqFMatrix2 = m1 => m2 => 
  (m1[0] == m2[0]) &&
  (m1[1] == m2[1]) &&
  (m1[2] == m2[2]) &&
  (m1[3] == m2[3]);
  exports.eqFMatrix3 = m1 => m2 => 
  (m1[0] == m2[0]) &&
  (m1[1] == m2[1]) &&
  (m1[2] == m2[2]) &&
  (m1[3] == m2[3]) &&
  (m1[4] == m2[4]) &&
  (m1[5] == m2[5]) &&
  (m1[6] == m2[6]) &&
  (m1[7] == m2[7]) &&
  (m1[8] == m2[8]);
  exports.eqFMatrix4 = m1 => m2 => 
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

// FMatrix2: 
exports.addFMatrix2 = a => b => out => () =>
{
  let m = out.value;

  m[0] = a[0] + b[0];
  m[1] = a[1] + b[1];
  m[2] = a[2] + b[2];
  m[3] = a[3] + b[3];
}
exports.subFMatrix2 = a => b => out => () =>
{
  let m = out.value;

  m[0] = a[0] - b[0];
  m[1] = a[1] - b[1];
  m[2] = a[2] - b[2];
  m[3] = a[3] - b[3];
}
exports.zeroFMatrix2 = new Float32Array([0.0, 0.0, 0.0, 0.0]);
exports.invFMatrix2 = a => out => () => {
  let m = out.value;

  m[0] = -a[0];
  m[1] = -a[1];
  m[2] = -a[2];
  m[3] = -a[3];
}
exports.mulFMatrix2 = x => a => out => () => {
  let m = out.value;

  m[0] = x * a[0];
  m[1] = x * a[1];
  m[2] = x * a[2];
  m[3] = x * a[3];
}
exports.identityFMatrix2 = new Float32Array([1.0, 0.0, 0.0, 1.0]);
exports.transposeFMatrix2 = a => out => () => {
  let m = out.value;

  m[0] = a[0];
  m[1] = a[2];
  m[2] = a[1];
  m[3] = a[3];
}
exports.mulMatrixFMatrix2 = a => b => out => () => {
  let m = out.value;

  const a00 = a[0], a01 = a[1],
        a10 = a[2], a11 = a[3];

  const b00 = b[0], b01 = b[1],
        b10 = b[2], b11 = b[3];

  m[0] = a00 * b00 + a01 * b10; // m00
  m[1] = a00 * b01 + a01 * b11; // m01
  m[2] = a10 * b00 + a11 * b10; // m10
  m[3] = a10 * b01 + a11 * b11; // m11
}
exports.determinantFMatrix2 = m => m[0] * m[3] - m[1] * m[2];
exports.invertFMatrix2 = a => out => () => {
  let m = out.value;

  let det = exports.determinantFMatrix2(a);

  if (det == 0) {
    return false;
  }

  let x = 1 / det;

  m[0] =  x * a[3];
  m[1] = -x * a[1];
  m[2] = -x * a[2];
  m[3] =  x * a[0];

  return true;
}


// FMatrix3: 
exports.addFMatrix3 = a => b => out => () =>
{
  let m = out.value;

  m[0] = a[0] + b[0];
  m[1] = a[1] + b[1];
  m[2] = a[2] + b[2];
  m[3] = a[3] + b[3];
  m[4] = a[4] + b[4];
  m[5] = a[5] + b[5];
  m[6] = a[6] + b[6];
  m[7] = a[7] + b[7];
  m[8] = a[8] + b[8];
}
exports.subFMatrix3 = a => b => out => () =>
{
  let m = out.value;

  m[0] = a[0] - b[0];
  m[1] = a[1] - b[1];
  m[2] = a[2] - b[2];
  m[3] = a[3] - b[3];
  m[4] = a[4] - b[4];
  m[5] = a[5] - b[5];
  m[6] = a[6] - b[6];
  m[7] = a[7] - b[7];
  m[8] = a[8] - b[8];
}
exports.zeroFMatrix3 = new Float32Array([ 0.0, 0.0, 0.0
                                        , 0.0, 0.0, 0.0
                                        , 0.0, 0.0, 0.0]);

exports.invFMatrix3 = a => out => () => {
  let m = out.value;

  m[0] = -a[0];
  m[1] = -a[1];
  m[2] = -a[2];
  m[3] = -a[3];
  m[4] = -a[4];
  m[5] = -a[5];
  m[6] = -a[6];
  m[7] = -a[7];
  m[8] = -a[8];
}
exports.mulFMatrix3 = x => a => out => () => {
  let m = out.value;

  m[0] = x * a[0];
  m[1] = x * a[1];
  m[2] = x * a[2];
  m[3] = x * a[3];
  m[4] = x * a[4];
  m[5] = x * a[5];
  m[6] = x * a[6];
  m[7] = x * a[7];
  m[8] = x * a[8];
}
exports.identityFMatrix3 = new Float32Array([ 1.0, 0.0, 0.0
                                            , 0.0, 1.0, 0.0
                                            , 0.0, 0.0, 1.0]);

exports.transposeFMatrix3 = a => out => () => {
  let m = out.value;

  m[0] = a[0];
  m[1] = a[3];
  m[2] = a[6];
  m[3] = a[1];
  m[4] = a[4];
  m[5] = a[7];
  m[6] = a[2];
  m[7] = a[5];
  m[8] = a[8];
}
exports.mulMatrixFMatrix3 = a => b => out => () => {
  let m = out.value;

  const a00 = a[0], a01 = a[1], a02 = a[2],
        a10 = a[3], a11 = a[4], a12 = a[5],
        a20 = a[6], a21 = a[7], a22 = a[8];

  const b00 = b[0], b01 = b[1], b02 = b[2],
        b10 = b[3], b11 = b[4], b12 = b[5],
        b20 = b[6], b21 = b[7], b22 = b[8];

  m[0] = a00 * b00 + a01 * b10 + a02 * b20; // m00
  m[1] = a00 * b01 + a01 * b11 + a02 * b21; // m01
  m[2] = a00 * b02 + a01 * b12 + a02 * b22; // m02

  m[3] = a10 * b00 + a11 * b10 + a12 * b20; // m10
  m[4] = a10 * b01 + a11 * b11 + a12 * b21; // m11
  m[5] = a10 * b02 + a11 * b12 + a12 * b22; // m12

  m[6] = a20 * b00 + a21 * b10 + a22 * b20; // m20
  m[7] = a20 * b01 + a21 * b11 + a22 * b21; // m21
  m[8] = a20 * b02 + a21 * b12 + a22 * b22; // m22
}
exports.determinantFMatrix3 = m => 
  m[0] * (m[4] * m[8] - m[5] * m[7]) -
  m[1] * (m[3] * m[8] - m[5] * m[6]) +
  m[2] * (m[3] * m[7] - m[4] * m[6]);
exports.invertFMatrix3 = m => out => () => {
  let r = out.value;

  let a00 = m[0], a01 = m[1], a02 = m[2],
      a10 = m[3], a11 = m[4], a12 = m[5],
      a20 = m[6], a21 = m[7], a22 = m[7];

  let det = exports.determinantFMatrix3(m);

  if (det == 0) {
    return false;
  }

  let a = 1.0 / det;

  r[0] = (a11 * a22 - a12 * a21) * a;
  r[1] = (a02 * a21 - a01 * a22) * a;
  r[2] = (a01 * a12 - a02 * a11) * a;
  r[3] = (a12 * a20 - a10 * a22) * a;
  r[4] = (a00 * a22 - a02 * a20) * a;
  r[5] = (a02 * a10 - a00 * a12) * a;
  r[6] = (a10 * a21 - a11 * a20) * a;
  r[7] = (a01 * a20 - a00 * a21) * a;
  r[8] = (a00 * a11 - a01 * a10) * a;

  return true;
}


// FMatrix4: 
exports.addFMatrix4 = a => b => out => () =>
{
  let m = out.value;

  m[0]  = a[ 0] + b[ 0];
  m[1]  = a[ 1] + b[ 1];
  m[2]  = a[ 2] + b[ 2];
  m[3]  = a[ 3] + b[ 3];
  m[4]  = a[ 4] + b[ 4];
  m[5]  = a[ 5] + b[ 5];
  m[6]  = a[ 6] + b[ 6];
  m[7]  = a[ 7] + b[ 7];
  m[8]  = a[ 8] + b[ 8];
  m[9]  = a[ 9] + b[ 0];
  m[10] = a[10] + b[10];
  m[11] = a[11] + b[11];
  m[12] = a[12] + b[12];
  m[13] = a[13] + b[13];
  m[14] = a[14] + b[14];
  m[15] = a[15] + b[15];
}
exports.subFMatrix4 = a => b => out => () =>
{
  let m = out.value;

  m[0]  = a[ 0] - b[ 0];
  m[1]  = a[ 1] - b[ 1];
  m[2]  = a[ 2] - b[ 2];
  m[3]  = a[ 3] - b[ 3];
  m[4]  = a[ 4] - b[ 4];
  m[5]  = a[ 5] - b[ 5];
  m[6]  = a[ 6] - b[ 6];
  m[7]  = a[ 7] - b[ 7];
  m[8]  = a[ 8] - b[ 8];
  m[9]  = a[ 9] - b[ 0];
  m[10] = a[10] - b[10];
  m[11] = a[11] - b[11];
  m[12] = a[12] - b[12];
  m[13] = a[13] - b[13];
  m[14] = a[14] - b[14];
  m[15] = a[15] - b[15];
}
exports.zeroFMatrix4 = new Float32Array([ 0.0, 0.0, 0.0, 0.0
                                        , 0.0, 0.0, 0.0, 0.0
                                        , 0.0, 0.0, 0.0, 0.0
                                        , 0.0, 0.0, 0.0, 0.0]);

exports.invFMatrix4 = a => out => () => {
  let m = out.value;

  m[0]  = -a[ 0];
  m[1]  = -a[ 1];
  m[2]  = -a[ 2];
  m[3]  = -a[ 3];
  m[4]  = -a[ 4];
  m[5]  = -a[ 5];
  m[6]  = -a[ 6];
  m[7]  = -a[ 7];
  m[8]  = -a[ 8];
  m[9]  = -a[ 9];
  m[10] = -a[10];
  m[11] = -a[11];
  m[12] = -a[12];
  m[13] = -a[13];
  m[14] = -a[14];
  m[15] = -a[15];
}
exports.mulFMatrix4 = x => a => out => () => {
  let m = out.value;

  m[0]  = x * a[ 0];
  m[1]  = x * a[ 1];
  m[2]  = x * a[ 2];
  m[3]  = x * a[ 3];
  m[4]  = x * a[ 4];
  m[5]  = x * a[ 5];
  m[6]  = x * a[ 6];
  m[7]  = x * a[ 7];
  m[8]  = x * a[ 8];
  m[9]  = x * a[ 9];
  m[10] = x * a[10];
  m[11] = x * a[11];
  m[12] = x * a[12];
  m[13] = x * a[13];
  m[14] = x * a[14];
  m[15] = x * a[15];
}
exports.identityFMatrix4 = new Float32Array([ 1.0, 0.0, 0.0, 0.0
                                            , 0.0, 1.0, 0.0, 0.0
                                            , 0.0, 0.0, 1.0, 0.0
                                            , 0.0, 0.0, 0.0, 1.0]);

exports.transposeFMatrix4 = a => out => () => {
  let m = out.value;

  m[0]  = a[ 0];
  m[1]  = a[ 4];
  m[2]  = a[ 8];
  m[3]  = a[12];
  m[4]  = a[ 1];
  m[5]  = a[ 5];
  m[6]  = a[ 9];
  m[7]  = a[13];
  m[8]  = a[ 2];
  m[9]  = a[ 6];
  m[10] = a[10];
  m[11] = a[14];
  m[12] = a[ 3];
  m[13] = a[ 7];
  m[14] = a[11];
  m[15] = a[15];
}
exports.mulMatrixFMatrix4 = a => b => out => () => {
  let m = out.value;

  const a00 = a[ 0], a01 = a[ 1], a02 = a[ 2], a03 = a[ 3]
        a10 = a[ 4], a11 = a[ 5], a12 = a[ 6], a13 = a[ 7]
        a20 = a[ 8], a21 = a[ 9], a22 = a[10], a23 = a[11]
        a30 = a[12], a31 = a[13], a32 = a[14], a33 = a[15];

  const b00 = b[ 0], b01 = b[ 1], b02 = b[ 2], b03 = b[ 3]
        b10 = b[ 4], b11 = b[ 5], b12 = b[ 6], b13 = b[ 7]
        b20 = b[ 8], b21 = b[ 9], b22 = b[10], b23 = b[11]
        b30 = b[12], b31 = b[13], b32 = b[14], b33 = b[15];

  m[0] = a00 * b00 + a01 * b10 + a02 * b20 + a03 * b30; // m00
  m[1] = a00 * b01 + a01 * b11 + a02 * b21 + a03 * b31; // m01
  m[2] = a00 * b02 + a01 * b12 + a02 * b22 + a03 * b32; // m02
  m[3] = a00 * b03 + a01 * b13 + a02 * b23 + a03 * b33; // m03

  m[4] = a10 * b00 + a11 * b10 + a12 * b20 + a13 * b30; // m10
  m[5] = a10 * b01 + a11 * b11 + a12 * b21 + a13 * b31; // m11
  m[6] = a10 * b02 + a11 * b12 + a12 * b22 + a13 * b32; // m12
  m[7] = a10 * b03 + a11 * b13 + a12 * b23 + a13 * b33; // m13

  m[ 8] = a20 * b00 + a21 * b10 + a22 * b20 + a23 * b30; // m20
  m[ 9] = a20 * b01 + a21 * b11 + a22 * b21 + a23 * b31; // m21
  m[10] = a20 * b02 + a21 * b12 + a22 * b22 + a23 * b32; // m22
  m[11] = a20 * b03 + a21 * b13 + a22 * b23 + a23 * b33; // m23

  m[12] = a30 * b00 + a31 * b10 + a32 * b20 + a33 * b30; // m30
  m[13] = a30 * b01 + a31 * b11 + a32 * b21 + a33 * b31; // m31
  m[14] = a30 * b02 + a31 * b12 + a32 * b22 + a33 * b32; // m32
  m[15] = a30 * b03 + a31 * b13 + a32 * b23 + a33 * b33; // m33
  
}
exports.determinantFMatrix4 = a => 
{
  var   a00 = a[0], a01 = a[1], a02 = a[2], a03 = a[3],
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
exports.invertFMatrix4 = a => out => () => {
  let m = out.value;

  let   a00 = a[0], a01 = a[1], a02 = a[2], a03 = a[3],
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
        b11 = a22 * a33 - a23 * a32,

        det = b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06;

  if (!det) {
      return false;
  }
  det = 1.0 / det;

  
  m[0] = (a11 * b11 - a12 * b10 + a13 * b09) * det;
  m[1] = (a02 * b10 - a01 * b11 - a03 * b09) * det;
  m[2] = (a31 * b05 - a32 * b04 + a33 * b03) * det;
  m[3] = (a22 * b04 - a21 * b05 - a23 * b03) * det;
  m[4] = (a12 * b08 - a10 * b11 - a13 * b07) * det;
  m[5] = (a00 * b11 - a02 * b08 + a03 * b07) * det;
  m[6] = (a32 * b02 - a30 * b05 - a33 * b01) * det;
  m[7] = (a20 * b05 - a22 * b02 + a23 * b01) * det;
  m[8] = (a10 * b10 - a11 * b08 + a13 * b06) * det;
  m[9] = (a01 * b08 - a00 * b10 - a03 * b06) * det;
  m[10] = (a30 * b04 - a31 * b02 + a33 * b00) * det;
  m[11] = (a21 * b02 - a20 * b04 - a23 * b00) * det;
  m[12] = (a11 * b07 - a10 * b09 - a12 * b06) * det;
  m[13] = (a00 * b09 - a01 * b07 + a02 * b06) * det;
  m[14] = (a31 * b01 - a30 * b03 - a32 * b00) * det;
  m[16] = (a20 * b03 - a21 * b01 + a22 * b00) * det;
  
  return true;
}

exports.mkOrtho = r => l => t => b => n => f => 
new Float32Array(
  [ 2 / (r - l), 0, 0, - (r + l) / (r - l)
  , 0, 2 / (t - b), 0, - (t + b) / (t - b)
  , 0, 0, -2 / (f - n), -(f + n) / (f - n)
  , 0, 0, 0, 1
  ]
);

exports.mkOrtho2 = w => h => n => f => 
new Float32Array(
  [ 2 / w, 0, 0, 0
  , 0, 2 / h, 0, 0
  , 0, 0, -2 * (f - n), -(f + n) / (f - n)
  , 0, 0, 0, 1
  ]
);


exports.mkPerspective = r => l => t => b => n => f => 
new Float32Array(
  [ 2 * n / (r - l), 0, (r + l) / (r - l), 0
  , 0, 2 * n / (t - b), (t + b) / (t - b), 0
  , 0, 0, - (f + n) / (f - n), -2 * f * n / (f - n)
  , 0, 0, -1, 0
  ]
);

exports.mkPerspective2 = w => h => n => f =>
new Float32Array(
  [ 2 * n / w, 0, 0, 0
  , 0, 2 * n / h, 0, 0
  , 0, 0, - (f + n) / (f - n), -2 * f * n / (f - n)
  , 0, 0, -1, 0
  ]
);

exports.mkPerspective3 = ratio => fov => n => f => {
  let S = (n / f) * ( 1 / Math.tan(fov/2));
  return new Float32Array(
    [ S, 0, 0, 0
    , 0, ratio * S, 0, 0
    , 0, 0, - (f + n) / (f - n), -2 * f * n / (f - n)
    , 0, 0, -1, 0
    ]
  );
}

exports.mkTranslation = x => y => z => {
  return new Float32Array(
    [
      1.0, 0.0, 0.0, x,
      0.0, 1.0, 0.0, y,
      0.0, 0.0, 1.0, z,
      0.0, 0.0, 0.0, 1.0
    ]
  );
}

exports.translate = x => y => z => out => () => {
  let m = out.value;

  

}

exports.mkTranslation2 = v => exports.mkTranslation(v[0])(v[1])(v[2]);

exports.mkScale = x => y => z => {
  return new Float32Array(
    [
        x, 0.0, 0.0, 0.0,
      0.0,   y, 0.0, 0.0,
      0.0, 0.0,   z, 0.0,
      0.0, 0.0, 0.0, 1.0
    ]
  );
}

exports.mkScale2 = v => exports.mkScale(v[0])(v[1])(v[2]);

exports.mkRotateZ = a => {

  let x = a * (Math.PI / 180);
  let s = Math.sin(x);
  let c = Math.cos(x);

  return new Float32Array(
    [
        c,  -s, 0.0, 0.0,
        s,   c, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 1.0
    ] 
  );
}

exports.mkRotateY = a => {

  let x = a * (Math.PI / 180);
  let s = Math.sin(x);
  let c = Math.cos(x);

  return new Float32Array(
    [
        c, 0.0,   s, 0.0, 
      0.0, 1.0, 0.0, 0.0,
       -s, 0.0,   c, 0.0,
      0.0, 0.0, 0.0, 1.0
    ]
  );
}

exports.mkRotateX = a => {

  let x = a * (Math.PI / 180);
  let s = Math.sin(x);
  let c = Math.cos(x);

  return new Float32Array(
    [
      1.0, 0.0, 0.0, 0.0,
      0.0,   c,  -s, 0.0,
      0.0,   s,   c, 0.0,
      0.0, 0.0, 0.0, 1.0
    ]
  );
}

exports.mkRotation = v => a => {

  let norm = Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  let ux = v[0] / norm;
  let uy = v[1] / norm;
  let uz = v[2] / norm;
  let x = a * (Math.PI / 180);
  let s = Math.sin(x);
  let c = Math.cos(x);

  return new Float32Array(
    [
      c + (1 - c) * ux * ux,
      (1 - c) * ux * uy - s * uz,
      (1 - c) * ux * uz + s * uy, 0.0,

      (1 - c) * ux * uy + s * uz,
      c + (1 - c) * uy * uy,
      (1 - c) * uy * uz - s * ux, 0.0,
      
      (1 - c) * ux * uz - s * uy,
      (1 - c) * uy * uz + s * ux,
      c + (1 - c) * uz * uz, 0.0,

      0.0, 0.0, 0.0, 1.0
    ]
  );
}