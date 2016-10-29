if (process.env.NODE_ENV != 'production') {
  require("!style!css!material-design-icons/iconfont/material-icons.css");
  require("!style!css!material-design-lite/dist/material.blue_grey-amber.min.css");
}

// inject bundled Elm app into div#main
var Elm = require( './Bosh' );

Elm.Bosh.fullscreen();
