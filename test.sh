#!/bin/sh

elm-make test/TestRunner.elm --output test.js && node test.js
