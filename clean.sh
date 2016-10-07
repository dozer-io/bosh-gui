#!/bin/sh

rm -rf \
   {src,test}/elm-package.json \
   {src,test}/elm-stuff \
   {src,test}/**/elm-package.json \
   {src,test}/**/elm-stuff \
   elm-stuff/build-artifacts
