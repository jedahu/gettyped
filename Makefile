SHELL = /usr/bin/env bash -O globstar
NBIN = $(shell yarn bin)

APP_SRC := $(shell ls app/main/**/*)
WORKER_SRC := $(shell ls app/worker/**/*)

src/.canary: index.org build.el
	emacs --batch -Q -l build.el -f gettyped-tangle-src && touch $@ || (rm $@ && exit 1)

tmp/index.expanded.org: index.org build.el
	emacs --batch -Q -l build.el -f gettyped-build-html

site/index.html: tmp/index.expanded.org header.html
	mkdir -p site
	pandoc -f org -t html5 \
		-o site/index.html \
		--parse-raw --standalone \
		-H header.html \
		tmp/index.expanded.org

out/app/worker/index.js: $(WORKER_SRC)
	$(NBIN)/tsc -p app/worker/tsconfig.json || (rm $@ && exit 1)

out/app/main/index.js: $(APP_SRC)
	$(NBIN)/tsc -p app/main/tsconfig.json || (rm $@ && exit 1)

site/worker.js: out/app/worker/index.js
	mkdir -p site
	NODE_PATH=out $(NBIN)/browserify $< > $@ || (rm $@ && exit 1)

site/app.js: out/app/main/index.js
	mkdir -p site
	NODE_PATH=out $(NBIN)/browserify $< > $@ || (rm $@ && exit 1)

.PHONY: src
src: tangle

.PHONY: statics
statics: $(shell ls static/**/*)
	mkdir -p site/lib
	cp -R static site/
	cp node_modules/react/dist/react.js site/lib/
	cp node_modules/react-dom/dist/react-dom.js site/lib/
	cp -R node_modules/ace-builds/src-noconflict site/lib/ace

.PHONY: worker
worker: site/worker.js

.PHONY: app
app: site/app.js

.PHONY: tangle
tangle: src/.canary

.PHONY: html
html: site/index.html

.PHONY: site
site: html worker app statics

.PHONY: webpack
webpack: html
	$(NBIN)/webpack

.PHONY: serve
serve: site
	$(NBIN)/http-server site

.PHONY: lint-worker
lint-worker:
	$(NBIN)/tslint src/worker/**/*.ts -p src/worker/tsconfig.json --type-check

.PHONY: clean
clean:
	rm -rf out site tmp src
