exports.mkVector2 = a => b => new Float32Array([a, b]);

exports.addVector2 = v1 => v2 => ref => () => { 
  ref.value[0] = v1[0] + v2[0];
  ref.value[1] = v1[1] + v2[1];
};