#!/usr/bin/env bash

shopt -s globstar

NBIN=./node_modules/.bin

do-clean() {
    rm -rf site/*
}

do-build-worker() {
    $NBIN/browserify \
        -t brfs \
        worker/index.js \
        > site/worker.js
}

do-build-css() {
    cat app/codeblock.css \
        > site/main.css
}

do-build-main() {
    cat lib/deps.js \
        node_modules/jquery/dist/jquery.min.js \
        node_modules/ace-builds/src-min-noconflict/ace.js \
        app/codeblock.js \
        app/index.js \
        > site/main.js
}

do-org-html() {
    mkdir -p site
    emacs --batch -Q -l build.el -f gettyped-build-html
    mv index.html site/
}

do-org-src() {
    emacs --batch -Q -l build.el -f gettyped-tangle-src
}

do-org() {
    mkdir -p site
    emacs --batch -Q -l build.el -f gettyped-html-and-tangle
    mv index.html site/
}

do-build-src() {
    $NBIN/tsc
}

do-build-demo() {
    $NBIN/
}

do-test-ce() {
    errors=$($NBIN/tsc -p tsconfig-demo-ce.json)
    exit=0
    cePaths=src/demo-ce/**/*.ts
    for f in src/demo-ce/**/*.ts
    do
        if (echo $errors | grep $f) >/dev/null
        then
            true
        else
            exit=1
            echo "CE missing for $f"
        fi
    done
    exit $exit
}

do-test-re() {
    $NBIN/mocha \
        --compilers ts:ts-node/register \
        -r tsconfig-paths/register \
        test/**/*.ts
}

do-test-eval() {
    $NBIN/tsc -p tsconfig-test.json || exit 1
    NODE_PATH=out node out/test/demo-re.js
}

do-build() {
    do-build-worker \
        && do-build-main \
        && do-build-css
}

do-clean() {
    rm -rf site src out
}

do-serve() {
    do-build-site
    $NBIN/http-server site
}

for cmd in $@
do
    "do-$cmd" || exit 1
done
