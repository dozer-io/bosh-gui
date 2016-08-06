// inject bundled Elm app into div#main
var Elm = require( './Dozer' );
Elm.Dozer.embed( document.getElementById( 'main' ) );
