SHELL := /usr/bin/env bash -O globstar
NBIN := $(shell yarn bin)

SRC = $(shell find src -type f)
DEMO = $(shell find demo -type f)
DOC := $(shell find doc -type f)
ORG := $(shell find doc -type f -iname '*.org')

SITE_SRC = $(SRC:%=site/%)
SITE_DEMO = $(DEMO:%=site/%)
SITE_DOC := $(DOC:%=site/%)
SITE_HTML := $(ORG:%.org=site/%.html)

PANDOC_DEPS := $(shell find scripts -type f -iname 'pandoc-*')

.PHONY: default
default: site

site/doc/%: doc/%
	@mkdir -p $(dir $@)
	cp $< $@

site/%.html: %.org $(PANDOC_DEPS)
	@mkdir -p $(dir $@)
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

site/src/%: src/%
	@mkdir -p $(dir $@)
	cp $< $@

site/demo/%: demo/%
	@mkdir -p $(dir $@)
	cp $< $@

site/module.nav: $(SITE_HTML)
	@rm -rf $@
	for path in $(SRC) $(DEMO); do echo $$path >>$@; done

site/worker.js: out/app/worker/index.js
	@mkdir -p site
	NODE_PATH=out $(NBIN)/browserify $< > $@ || (rm $@ && exit 1)

site/app.js: out/app/main/index.js
	@mkdir -p site
	NODE_PATH=out $(NBIN)/browserify $< > $@ || (rm $@ && exit 1)

.PHONY: test
test: site
	NODE_PATH=./:./src/ $(NBIN)/ts-node -P test test/demo.ts

.PHONY: test-repl
test-repl: site
	NODE_PATH=./:./src/ $(NBIN)/ts-node -P test

.PHONY: statics
statics: $(shell ls static/**/*)
	@mkdir -p site
	cp -R static site/

.PHONY: worker
worker: site/worker.js

.PHONY: app
app: site/app.js

.PHONY: html
html: $(SITE_HTML)

.PHONY: site
site: html statics $(SITE_SRC) $(SITE_DEMO) $(SITE_DOC) site/module.nav

.PHONY: webpack
webpack: html
	$(NBIN)/webpack

.PHONY: webpack-dev
webpack-dev: html
	NODE_ENV=development \
	$(NBIN)/nodemon --watch webpack.config.ts \
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
