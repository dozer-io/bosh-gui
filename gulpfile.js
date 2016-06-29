var gulp = require('gulp');
// var browserSync = require('browser-sync').create();
// var icons = require('gulp-material-icons');
// var icon_config = require('./icons.json');
// var gulpNgConfig = require('gulp-ng-config');
var stubby = require('gulp-stubby-server');
// var less = require('gulp-less');
// var path = require('path');

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

// gulp.task('less', function () {
//   return gulp.src('./src/less/*.less')
//     .pipe(less())
//     .pipe(gulp.dest('./src/css/'));
// });

// gulp.task('serve', ['stub', 'config', 'less'], function () {

//   browserSync.init({
//     server: {
//       baseDir: './src/',
//       middleware: function (req, res, next) {
//         res.setHeader('Access-Control-Allow-Origin', '*');
//         next();
//       }
//     }
//   });

//   var watchDirs = [
//     'gulpfile.js',
//     'src/**/*.css',
//     'src/**/*.html',
//     'src/**/*.js',
//     'src/index.html',
//     'src/index.js'
//   ];

//   gulp.watch(watchDirs).on('change', browserSync.reload);
//   gulp.watch('src/less/*.less', ['less']);
// });

// gulp.task('icons', function() {
//   return icons({tasks: icon_config})
//     .pipe(gulp.dest('./src/img/svgs'));
// });

// var environment = process.env.NODE_ENV || 'development';

// gulp.task('config', function() {
//   gulp.src('config.json')
//     .pipe(gulpNgConfig('app.config', {environment: environment}))
//     .pipe(gulp.dest('src/lib/'));
// });
