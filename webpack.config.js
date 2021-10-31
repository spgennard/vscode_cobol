/* eslint-disable @typescript-eslint/no-var-requires */
/* eslint-disable no-undef */
const path = require("path");
const webpack = require("webpack");

const webConfig = /** @type WebpackConfig */ {
  context: __dirname,
  mode: "none", // this leaves the source code as close as possible to the original (when packaging we set this to 'production')
  target: "webworker", // web extensions run in a webworker context
  entry: {
    "extension-web": "./src/web/extension.ts", // source of the web extension main file
    // "test/suite/index-web": "./src/test/suite/index-web.ts", // source of the web extension test runner
  },
  output: {
    filename: "[name].js",
    path: path.join(__dirname, "./dist/web"),
    libraryTarget: "commonjs",
  },
  resolve: {
    mainFields: ["browser", "module", "main"], // look for `browser` entry point in imported node modules
    extensions: [".ts", ".js"], // support ts-files and js-files
    alias: {
      // provides alternate implementation for node module and source files
    },
    fallback: {
      // Webpack 5 no longer polyfills Node.js core modules automatically.
      // see https://webpack.js.org/configuration/resolve/#resolvefallback
      // for the list of Node.js core module polyfills.
      assert: require.resolve("assert"),
      buffer: require.resolve("buffer"),
      fs: require.resolve("memfs"),
      //console: require.resolve('console-browserify'),
      //constants: require.resolve('constants-browserify'),
      crypto: require.resolve("crypto-browserify"),
      //domain: require.resolve('domain-browser'),
      //events: require.resolve('events'),
      //http: require.resolve('stream-http'),
      //https: require.resolve('https-browserify'),
      os: require.resolve("os-browserify/browser"),
      path: require.resolve("path-browserify"),
      //punycode: require.resolve('punycode'),
      process: require.resolve("process/browser"),
      //querystring: require.resolve('querystring-es3'),
      stream: require.resolve("stream-browserify"),
      //string_decoder: require.resolve('string_decoder'),
      //sys: require.resolve('util'),
      //timers: require.resolve('timers-browserify'),
      //tty: require.resolve('tty-browserify'),
      url: require.resolve("url"),
      //util: require.resolve('util'),
      //vm: require.resolve('vm-browserify'),
      //zlib: require.resolve('browserify-zlib'),
    },
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        use: [
          {
            loader: "ts-loader",
          },
        ],
      },
    ],
  },
  plugins: [
    // Work around for Buffer is undefined:
    // https://github.com/webpack/changelog-v5/issues/10
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
    new webpack.ProvidePlugin({
      process: "process/browser", // provide a shim for the global `process` variable
    }),
  ],
  externals: {
    vscode: "commonjs vscode", // ignored because it doesn't exist
  },
  performance: {
    hints: false,
  },
  devtool: "nosources-source-map", // create a source map that points to the original source file
};

/**@type {import('webpack').Configuration}*/
const config = {
  target: "node", // vscode extensions run in a Node.js-context 📖 -> https://webpack.js.org/configuration/node/

  entry: "./src/extension.ts", // the entry point of this extension, 📖 -> https://webpack.js.org/configuration/entry-context/
  output: {
    // the bundle is stored in the 'dist' folder (check package.json), 📖 -> https://webpack.js.org/configuration/output/
    path: path.resolve(__dirname, "dist"),
    filename: "extension.js",
    libraryTarget: "commonjs2",
    devtoolModuleFilenameTemplate: "../[resource-path]"
  },
  devtool: "source-map",
  externals: {
    vscode: "commonjs vscode" // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
  },
  resolve: {
    // support reading TypeScript and JavaScript files, 📖 -> https://github.com/TypeStrong/ts-loader
    extensions: [".ts", ".js"],
    alias: {
      perf_hooks: "node_modules/performance-now/lib/performance-now.js"
    }
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        use: [
          {
            loader: "ts-loader"
          }
        ]
      }
    ]
  }
};
module.exports = [webConfig, config];
