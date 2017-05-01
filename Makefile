SHELL = /usr/bin/env bash -O globstar
NBIN = $(shell yarn bin)

APP_SRC := $(shell ls src/app/**/*.ts)
WORKER_SRC := $(shell ls src/worker/**/*.ts)

src/.canary: index.org build.el
	emacs --batch -Q -l build.el -f gettyped-tangle-src && touch $@ || rm $@

src/lib/typescript-simple.js: node_modules/typescript-simple/index.js
	mkdir -p src/lib
	(sed -e 's|fs\.readFileSync(defaultLibPath)|fs.readFileSync("node_modules/typescript/lib/lib.es6.d.ts")|' | sed -e 's|var defaultLibPath|// var defaultLibPath|') < $< > $@

src/lib/typescript-simple.d.ts: node_modules/typescript-simple/index.d.ts
	cp $< $@

tmp/index.expanded.org: index.org build.el
	emacs --batch -Q -l build.el -f gettyped-build-html

site/index.html: tmp/index.expanded.org
	mkdir -p site
	pandoc -f org -t html5 \
		-o site/index.html \
		--parse-raw --standalone \
		tmp/index.expanded.org \
		--metadata="header-includes:<link rel='stylesheet' href='app.css'><script src='ace/ace.js'></script><script src='app.js'></script>"

out/worker/index.js: $(WORKER_SRC) src/lib/typescript-simple.js src/lib/typescript-simple.d.ts
	$(NBIN)/tsc -p src/worker || rm $@

out/app/index.js: $(APP_SRC)
	$(NBIN)/tsc -p src/app || rm $@

site/worker.js: out/worker/index.js
	mkdir -p site
	NODE_PATH=out $(NBIN)/browserify -t brfs $< > $@ || rm $@

site/app.js: out/app/index.js
	mkdir -p site
	NODE_PATH=out $(NBIN)/browserify $< > $@ || rm $@

site/ace/ace.js: node_modules/ace-builds/package.json
	cp -R node_modules/ace-builds/src-noconflict site/ace

.PHONY: lib
lib: src/lib/typescript-simple.js src/lib/typescript-simple.d.ts

.PHONY: src
src: lib tangle

.PHONY: statics
statics: $(shell ls static/**/*)
	mkdir -p site
	cp -R static site/

.PHONY: worker
worker: site/worker.js

.PHONY: app
app: site/app.js site/ace/ace.js

.PHONY: tangle
tangle: src/.canary

.PHONY: html
html: site/index.html

.PHONY: site
site: html worker app statics

.PHONY: serve
serve: site
	$(NBIN)/http-server site

.PHONY: lint-worker
lint-worker:
	$(NBIN)/tslint src/worker/**/*.ts -p src/worker/tsconfig.json --type-check

.PHONY: clean
clean:
	(cd src; rm -rf `ls . | grep -v -e test -e app -e worker -e external.d.ts -e tsconfig.json`)
	rm -rf out site tmp src/.canary
