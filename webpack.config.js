const path = require('path');

module.exports = {
  entry: './static/index.js',
  mode: 'development',
  // devtool: 'inline-source-map',
  output: {
    path: path.resolve(__dirname, 'static'),
    filename: 'bundle.js'
  },
  module: {
    rules: [
      {
        test: /\.css$/i,
        use: [
          "style-loader",
          { loader: "css-loader", options: { importLoaders: 1 } },
          "postcss-loader",
        ],
      },
    ],
  },
};
