exports.mkDataView = b => new DataView(b);

exports._mkDataView2 = b => o => l => new DataView(b, o, l);
exports._setter = name => end => view => offset => val => () => { view[name](offset, val, end); }
exports._getter = name => len => end => view => offset => () => (offset + len) <= view.byteLength ? view[name](offset, end) : null;

