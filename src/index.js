// inject bundled Elm app into div#main
var Elm = require( './vms' );
Elm.Main.embed( document.getElementById( 'main' ) );
