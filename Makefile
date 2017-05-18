SHELL = /usr/bin/env bash -O globstar
NBIN = $(shell yarn bin)

APP_SRC := $(shell ls app/main/**/*)
WORKER_SRC := $(shell ls app/worker/**/*)
ORG_SRC := $(shell ls doc/**/*.org)
ORG_HTML := $(ORG_SRC:%.org=site/%.html)
PANDOC_DEPS := $(shell ls scripts/pandoc-*)

site/doc/typenav: doc/typenav
	mkdir -p $(dir $@)
	cp $< $@

site/%.html: %.org $(PANDOC_DEPS)
	mkdir -p $(dir $@)
	pandoc -f org -t html5 \
		-o $@ \
		--parse-raw \
		--no-highlight \
		--section-divs \
		--toc \
		--standalone \
		--template scripts/pandoc-template.html \
		--filter scripts/pandoc-site-filter.js \
		$<

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

.PHONY: statics
statics: $(shell ls static/**/*)
	mkdir -p site
	cp -R static site/

.PHONY: worker
worker: site/worker.js

.PHONY: app
app: site/app.js

.PHONY: html
html: $(ORG_HTML)

.PHONY: site
site: html statics site/doc/typenav
# site: html worker app statics site/doc/typenav

.PHONY: webpack
webpack: html
	$(NBIN)/webpack

.PHONY: webpack-dev
webpack-dev: html
	$(NBIN)/webpack-dev-server --hot --watch

.PHONY: serve
serve:
	$(NBIN)/http-server site

.PHONY: lint-worker
lint-worker:
	$(NBIN)/tslint src/worker/**/*.ts -p src/worker/tsconfig.json --type-check

.PHONY: clean
clean:
	rm -rf out site tmp demo
