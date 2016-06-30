// inject bundled Elm app into div#main
var Elm = require( './info' );
Elm.Main.embed( document.getElementById( 'main' ) );
