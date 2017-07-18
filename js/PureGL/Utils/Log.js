exports.logObject = str => o => () => { console.log(str, o); };
exports.secretLog = str => o => () => { console.log(str, o); };