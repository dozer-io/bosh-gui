// inject bundled Elm app into div#main
var Elm = require( './Dozer' );
Elm.Dozer.fullscreen(
  { apiUrl : "http://localhost:8001/dozer",
    authUrl : "http://localhost:8001/login/authorize",
    appUrl : "http://localhost:8080"
  }
);
