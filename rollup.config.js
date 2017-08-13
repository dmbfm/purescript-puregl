import purs from "rollup-plugin-purs";

export default {
  //entry: "examples/ECS/Main.purs",
  entry: "test/Main.purs",
  dest: "bundle.js",
  format: "iife",
  sourceMap: true,
  plugins: [
    purs()
  ]
};