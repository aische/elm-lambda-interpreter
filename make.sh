#!/bin/sh

evm use 0.18.0
elm-make src/Main.elm --output=dist/bundle.js

cp src/main.html dist/index.html
cp src/example.txt dist/example.txt
cp src/example2.txt dist/example2.txt
