const fs = require("fs");
const path = require("path");
const webpack = require('webpack');

const dirs = p => fs.readdirSync(p).filter(file => fs.lstatSync(path.join(p, file)).isDirectory() && file != "dist");
const examples = dirs(path.join('.','examples'));

let entries = {};

examples.map(name => { entries[name] = "./" + path.join('./', './examples', name, 'index.js'); });
console.log(entries);

module.exports = {
  context: path.resolve(__dirname),
  entry: entries,
  output: {
    filename: "[name].bundle.js",
    publicPath: "/examples/dist/",
    path:  path.join(__dirname, "examples", "dist")
  },
  devServer: {
    hot: true,
    hotOnly: true,
    inline: true,
    host: '0.0.0.0',
    port: '8080',
    disableHostCheck: true,
    contentBase: __dirname,
    publicPath: "/examples/dist/"
  },
  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              src: [
                'bower_components/purescript-*/src/**/*.purs',
                'src/**/*.purs'
              ],
              mainModule: { Main: 'main' },
              bundle: false,
              psc: 'psa',
              //watch: isWebpackDevServer || isWatch,
              pscIde: false
            }
          }
        ]
      },
    ]
  },
  resolve: {
    modules: ['node_modules', 'bower_components'],
    extensions: ['.purs', '.js']
  },
  plugins: [
    new webpack.optimize.CommonsChunkPlugin({
      name: 'vendor',
      filename: 'vendor.js'
    }),
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NamedModulesPlugin()
  ]
}

