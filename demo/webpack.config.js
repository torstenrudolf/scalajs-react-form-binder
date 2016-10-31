'use strict';

var webpack = require('webpack');
var CommonsChunkPlugin = webpack.optimize.CommonsChunkPlugin;

module.exports = {

    entry: {
        index: './resources/jsBundles/index.js'
    },
    output: {
        path: __dirname + '/build',
        publicPath: __dirname  + "/build/",
        filename: 'demo-jsdeps.js'
    },
    plugins: [
        new webpack.NoErrorsPlugin(),
        new CommonsChunkPlugin({name: "index"})
    ]

};
