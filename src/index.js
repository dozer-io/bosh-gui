if (process.env.NODE_ENV != 'production') {
  require("!style!css!material-design-icons/iconfont/material-icons.css");
  require("!style!css!material-design-lite/dist/material.blue_grey-amber.min.css");
}

// inject bundled Elm app into div#main
var Elm = require( './Main' );

if (process.env.NODE_ENV == 'production') {
  Elm.Main.fullscreen({
    target : "https://localhost:25555"
  });
} else {
  Elm.Main.fullscreen({
    target : "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000"
  });
}
