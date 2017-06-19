SHELL := /usr/bin/env bash -O globstar
NBIN := $(shell yarn bin)

DOC := $(shell find doc -type f)
ORG := $(shell find doc -type f -iname '*.org')

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

site/module.nav: $(SITE_HTML)
	@rm -rf $@
	for path in $$(find src demo -type f); \
		do echo $$path >>$@; \
	done

.PHONY: test
test: site
	NODE_PATH=./:./src/ $(NBIN)/ts-node -P test test/demo.ts

.PHONY: test-repl
test-repl: site
	NODE_PATH=./:./src/ $(NBIN)/ts-node -P test

.PHONY: statics
statics:
	rsync -a static site

.PHONY: html
html: $(SITE_HTML)

.PHONY: site-src
site-src: html
	rsync -a src site

.PHONY: site-demo
site-demo: html
	rsync -a demo site

.PHONY: site
site: html statics site-src site-demo $(SITE_DOC) site/module.nav

.PHONY: webpack
webpack: site
	$(NBIN)/webpack

.PHONY: webpack-dev
webpack-dev: site
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
