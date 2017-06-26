exports.mkFVector2 = a => b => new Float32Array([a, b]);
exports.mkFVector3 = a => b => c => new Float32Array([a, b, c]);
exports.mkFVector4 = a => b => c => d => new Float32Array([a, b, c, d]);

exports.eqFVector2 = v1 => v2 => 
  (v1[0] == v2[0]) &&
  (v1[1] == v2[1]);

exports.eqFVector3 = v1 => v2 =>
  (v1[0] == v2[0]) &&
  (v1[1] == v2[1]) &&
  (v1[2] == v2[2]);

exports.eqFVector4 = v1 => v2 =>
  (v1[0] == v2[0]) &&
  (v1[1] == v2[1]) &&
  (v1[2] == v2[2]) &&
  (v1[3] == v2[3]);

exports.addFVector2 = a => b => out => () => {
  let v = out.value;
  v[0] = a[0] + b[0];
  v[1] = a[1] + b[1];
};

exports.subFVector2 = a => b => out => () => {
  let v = out.value;
  v[0] = a[0] - b[0];
  v[1] = a[1] - b[1];
};

exports.invFVector2 = a  => out => () => {
  let v = out.value;
  v[0] = -a[0];
  v[1] = -a[1];
};

exports.mulFVector2 = m => a => out => () => {
  let v = out.value;
  v[0] = a[0] * m;
  v[1] = a[1] * m;
};

exports.toFloat32ArrayFVector2 = a => a;
exports.toStringFVector2 = a => a.toString();
exports.dotFVector2 = a => b => a[0] * b[0] + a[1] * b[1];

exports.addFVector3 = a => b => out => () => {
  let v = out.value;
  v[0] = a[0] + b[0];
  v[1] = a[1] + b[1];
  v[2] = a[2] + b[2];
};

exports.subFVector3 = a => b => out => () => {
  let v = out.value;
  v[0] = a[0] - b[0];
  v[1] = a[1] - b[1];
  v[2] = a[2] - b[2];
};

exports.invFVector3 = a  => out => () => {
  let v = out.value;
  v[0] = -a[0];
  v[1] = -a[1];
  v[2] = -a[2];
};

exports.mulFVector3 = m => a => out => () => {
  let v = out.value;
  v[0] = a[0] * m;
  v[1] = a[1] * m;
  v[2] = a[2] * m;
};

exports.toFloat32ArrayFVector3 = a => a;
exports.toStringFVector3 = a => a.toString();
exports.dotFVector3 = a => b => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];

exports.addFVector4 = a => b => out => () => {
  let v = out.value;
  v[0] = a[0] + b[0];
  v[1] = a[1] + b[1];
  v[2] = a[2] + b[2];
  v[3] = a[3] + b[3];
};

exports.subFVector4 = a => b => out => () => {
  let v = out.value;
  v[0] = a[0] - b[0];
  v[1] = a[1] - b[1];
  v[2] = a[2] - b[2];
  v[3] = a[3] - b[3];
};

exports.invFVector4 = a  => out => () => {
  let v = out.value;
  v[0] = -a[0];
  v[1] = -a[1];
  v[2] = -a[2];
  v[3] = -a[3];
};

exports.mulFVector4 = m => a => out => () => {
  let v = out.value;
  v[0] = a[0] * m;
  v[1] = a[1] * m;
  v[2] = a[2] * m;
  v[3] = a[3] * m;
};

exports.toFloat32ArrayFVector4 = a => a;
exports.toStringFVector4 = a => a.toString();
exports.dotFVector4 = a => b => a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3];