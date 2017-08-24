exports.mkArrayBuffer = len => () => new ArrayBuffer(len);

exports.fromArrayFloat32Array = arr => () => new Float32Array(arr);
exports.toArrayFloat32Array = f => () => Array.from(f);
exports.lengthFloat32Array = f => f.length;
exports.getBufferFloat32Array = f => f.buffer;
exports.byteLengthFloat32Array = f => f.byteLength;
exports.fillFloat32Array = f => v => () => f.fill(v);
exports.toStringFloat32Array = f => f.toString();
exports.getAtFloat32Array = f => i => () => typeof f[i] === "undefined" ? null : f[i];
exports.setAtFloat32Array = f => i => a => () => { f[i] = a; };
exports.fromBufferFloat32Array = b => () => new Float32Array(b);
exports.fromBufferFloat32Array2 = b => o => l => () => new Float32Array(b, o, l);

exports.fromArrayInt8Array = arr => () => new Int8Array(arr);
exports.toArrayInt8Array = f => () => Array.from(f);
exports.lengthInt8Array = f => f.length;
exports.getBufferInt8Array = f => f.buffer;
exports.byteLengthInt8Array = f => f.byteLength;
exports.fillInt8Array = f => v => () => f.fill(v);
exports.toStringInt8Array = f => f.toString();
exports.getAtInt8Array = f => i => () => typeof f[i] === "undefined" ? null : f[i];
exports.setAtInt8Array = f => i => a => () => { f[i] = a; };
exports.fromBufferInt8Array = b => () => new Int8Array(b);
exports.fromBufferInt8Array2 = b => o => l => () => new Int8Array(b, o, l);


exports.fromArrayInt16Array = arr => () => new Int16Array(arr);
exports.toArrayInt16Array = f => () => Array.from(f);
exports.lengthInt16Array = f => f.length;
exports.getBufferInt16Array = f => f.buffer;
exports.byteLengthInt16Array = f => f.byteLength;
exports.fillInt16Array = f => v => () => f.fill(v);
exports.toStringInt16Array = f => f.toString();
exports.getAtInt16Array = f => i => () => typeof f[i] === "undefined" ? null : f[i];
exports.setAtInt16Array = f => i => a => () => { f[i] = a; };
exports.fromBufferInt16Array = b => () => new Int16Array(b);
exports.fromBufferInt16Array2 = b => o => l => () => new Int16Array(b, o, l);
exports._byteOffset = b => b.byteOffset;


exports.fromArrayInt32Array = arr => () => new Int32Array(arr);
exports.toArrayInt32Array = f => () => Array.from(f);
exports.lengthInt32Array = f => f.length;
exports.getBufferInt32Array = f => f.buffer;
exports.byteLengthInt32Array = f => f.byteLength;
exports.fillInt32Array = f => v => () => f.fill(v);
exports.toStringInt32Array = f => f.toString();
exports.getAtInt32Array = f => i => () => typeof f[i] === "undefined" ? null : f[i];
exports.setAtInt32Array = f => i => a => () => { f[i] = a; };
exports.fromBufferInt32Array = b => () => new Int32Array(b);
exports.fromBufferInt32Array2 = b => o => l => () => new Int32Array(b, o, l);
exports._byteOffset = b => b.byteOffset;