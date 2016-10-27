if (process.env.NODE_ENV != 'production') {
  require("!style!css!material-design-icons/iconfont/material-icons.css");

  require("!style!css!material-design-lite/dist/material.blue-amber.min.css");
}

// inject bundled Elm app into div#main
var Elm = require( './Dozer' );

if (process.env.NODE_ENV == 'production') {
  Elm.Dozer.fullscreen({
    apiUrl : "https://api.dozer.io",
    authUrl : "https://login.dozer.io/oauth/authorize",
    appUrl : "https://beta-console.dozer.io",
    oAuthClient : "dozer-web-beta",
    oAuthScopes : [ "dozer_api.user", "bosh_api.user", "bosh.*.admin" ]
  });
} else {
  Elm.Dozer.fullscreen({
    apiUrl : "http://localhost:8001/dozer",
    authUrl : "http://localhost:8001/login/authorize",
    appUrl : "http://localhost:8080",
    oAuthClient : "dozer-web-beta",
    oAuthScopes : [ "dozer_api.user", "bosh_api.user", "bosh.*.admin" ]
  });
}
