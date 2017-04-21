#!/usr/bin/env bash
./node_modules/.bin/browserify -t brfs worker/index.js > site/worker.js

cat lib/deps.js \
    node_modules/jquery/dist/jquery.min.js \
    node_modules/ace-builds/src-min-noconflict/ace.js \
    app/codeblock.js \
    app/index.js \
    > site/main.js

cat app/codeblock.css \
    > site/main.css
