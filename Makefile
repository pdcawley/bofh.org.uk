DESTDIR     =public

EMACS ?= emacs
BATCH  = $(EMACS) -Q -batch -l ./setup-org-export.el

.PHONY: all
all: clean build

deploy:
	rsync -avz --del ./public/ mbpi:bofh.org.uk

build: webmentions org-posts org-pages hugo-build

org-posts:
	@echo "Exporting posts from Org"
	$(BATCH) ./all-posts.org --eval='(script/export-to-hugo)'

org-pages:
	@echo "Exporting pages from Org"
	$(BATCH) ./all-pages.org --eval='(script/export-to-hugo)'

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
