DESTDIR     =public

EMACS ?= emacs
BATCH  = $(EMACS) -Q -batch -l ./setup-org-export.el

.PHONY: all
all: clean build

deploy:
	rsync -avz --del ./public/ mbpi:bofh.org.uk

build: webmentions org-export hugo-build

org-export:
	@echo "Exporting from Org"
	$(BATCH) ./Content.org --eval='(script/export-to-hugo)'

webmentions:
	@echo "Exporting webmentions data"
	$(BATCH) --eval='(wm-fetch-mentions)'

hugo-build:
	@echo "Building site with Hugo"
	hugo --gc --minify -d $(DESTDIR)

clean: clean-content clean-public

clean-content:
	rm -r content
	git checkout content

clean-public:
	@echo "Cleaning old build"
	rm -rf $(DESTDIR)/*
