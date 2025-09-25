DESTDIR     =public

EMACS ?= emacs
BATCH  = $(EMACS) -Q -batch -l ./setup-org-export.el
HUGO ?= /opt/homebrew/bin/hugo

.PHONY: all
all: build

deploy:
	rsync -avz --del ./public/ mbpi:bofh.org.uk

build: webmentions org-export hugo-build

targets = content/*/*/index.md

$(targets) &: ./Content.org
	$(BATCH) ./Content.org --eval='(script/export-to-hugo)'

org-export: $(targets)

webmentions:
	@echo "Exporting webmentions data"
	$(BATCH) --eval='(wm-fetch-mentions)'

hugo-build:
	@echo "Building site with Hugo"
	$(HUGO) --gc --minify -d $(DESTDIR) -b https://bofh.org.uk/

clean: clean-content clean-public

clean-content:
	rm -r content
	git checkout content

clean-public:
	@echo "Cleaning old build"
	rm -rf $(DESTDIR)/*
