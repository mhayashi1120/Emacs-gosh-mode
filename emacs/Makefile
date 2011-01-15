# -*- mode: makefile; -*-
#
# Makefile for gosh-mode
#

TAR	= tar
RM	= /bin/rm -f

EMACS	= emacs

FLAGS   = -batch -q -no-site-file -l gosh-mode-make.el

# For windows emacs
CHECKFLAGS = $(FLAGS)

VERSION = 0.1.2

RELEASE_FILES = \
	gosh-mode.el gosh-config.el gosh-const.el \
	gosh-mode-test.el gosh-mode-make.el \
	refactor.el \
	MAKE-CFG.el Makefile \
	README

ARCHIVE_DIR_PREFIX = ..

GOMI	= *.elc *~

default: elc

check: clean
	$(EMACS) $(CHECKFLAGS) -f check-gosh-mode $(CONFIG)

elc:
	$(EMACS) $(FLAGS) -f compile-gosh-mode $(CONFIG)

what-where:
	$(EMACS) $(FLAGS) -f what-where-gosh-mode $(CONFIG)

install: elc
	$(EMACS) $(FLAGS) -f install-gosh-mode $(CONFIG)

uninstall:
	$(EMACS) $(FLAGS) -f uninstall-gosh-mode $(CONFIG)

clean:
	-$(RM) $(GOMI)

release: archive
	$(RM) -f $(ARCHIVE_DIR_PREFIX)/gosh-mode-$(VERSION).tar.bz2 $(ARCHIVE_DIR_PREFIX)/gosh-mode-$(VERSION).tar.gz
	mv /tmp/gosh-mode-$(VERSION).tar.bz2 /tmp/gosh-mode-$(VERSION).tar.gz $(ARCHIVE_DIR_PREFIX)/

archive:
	rm -rf /tmp/gosh-mode-$(VERSION)
	mkdir /tmp/gosh-mode-$(VERSION)
	cp -p $(RELEASE_FILES) /tmp/gosh-mode-$(VERSION)
	chmod 644 /tmp/gosh-mode-$(VERSION)/*
	cd /tmp ; tar cjf gosh-mode-$(VERSION).tar.bz2 gosh-mode-$(VERSION)
	cd /tmp ; tar czf gosh-mode-$(VERSION).tar.gz gosh-mode-$(VERSION)


