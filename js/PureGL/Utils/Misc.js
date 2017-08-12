exports.merge = _ => a => b => {
  return {
    ...a,
    ...b
  }
}