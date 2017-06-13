import purs from "rollup-plugin-purs";

export default {
  entry: "bench/Bench/Main.purs",
  dest: "output/bundle.js",
  format: "iife",
  sourceMap: true,
  plugins: [
    purs({ optimizations: {assumePureVars: false}})
  ]
};
