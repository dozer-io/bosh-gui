// inject bundled Elm app into div#main
var Elm = require( './Dozer' );

if (process.env.NODE_ENV == 'production') {
  Elm.Dozer.fullscreen({
    apiUrl : "https://api.dozer.io",
    authUrl : "https://login.dozer.io/oauth/authorize",
    appUrl : "https://console-elm.dozer.io"
  });
} else {
  Elm.Dozer.fullscreen({
    apiUrl : "http://localhost:8001/dozer",
    authUrl : "http://localhost:8001/login/authorize",
    appUrl : "http://localhost:8080"
  });
}
