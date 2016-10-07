var gulp = require('gulp');
var gutil = require("gulp-util");
var stubby = require('gulp-stubby-server');
var webpack = require("webpack");
var WebpackDevServer = require("webpack-dev-server");
var webpackConfig = require("./webpack.config.js");

gulp.task('stub', function(cb) {
  var options = {
    callback: function (server, options) {
      server.get(1, function (err, endpoint) {
        if (!err)
          console.log(endpoint);
      });
    },
    stubs: 8001,
    admin: 8010,
    mute: false,
    relativeFilesPath: true,
    files: ['api_stub/stub.yml']
  };
  stubby(options, cb);
});

gulp.task("serve", ['stub'], function(callback) {
  // modify some webpack config options
  var myConfig = Object.create(webpackConfig);
  myConfig.devtool = "eval";
  myConfig.debug = true;

  // Start a webpack-dev-server
  new WebpackDevServer(webpack(myConfig), {
    stats: {
      colors: true
    }
  }).listen(8080, "localhost", function(err) {
    if(err) throw new gutil.PluginError("webpack-dev-server", err);
    gutil.log("[webpack-dev-server]", "http://localhost:8080/webpack-dev-server/index.html");
  });
});
