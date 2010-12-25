# -*- mode: makefile; -*-
#
# Makefile for gauche-mode
#

TAR	= tar
RM	= /bin/rm -f

EMACS	= emacs

FLAGS   = -batch -q -no-site-file -l gauche-mode-make.el

# For windows emacs
CHECKFLAGS = $(FLAGS)

VERSION = 0.1.0

RELEASE_FILES = \
	gauche-mode.el gauche-config.el gauche-const.el gauche-refactor.el \
	refactor.el \
	Makefile

ARCHIVE_DIR_PREFIX = ..

GOMI	= *.elc *~

default: elc

check: clean
	$(EMACS) $(CHECKFLAGS) -f check-gauche-mode $(CONFIG)

elc:
	$(EMACS) $(FLAGS) -f compile-gauche-mode $(CONFIG)

what-where:
	$(EMACS) $(FLAGS) -f what-where-gauche-mode $(CONFIG)

install: elc
	$(EMACS) $(FLAGS) -f install-gauche-mode $(CONFIG)

clean:
	-$(RM) $(GOMI)

release: archive
	$(RM) -f $(ARCHIVE_DIR_PREFIX)/gauche-mode-$(VERSION).tar.bz2 $(ARCHIVE_DIR_PREFIX)/gauche-mode-$(VERSION).tar.gz
	mv /tmp/gauche-mode-$(VERSION).tar.bz2 /tmp/gauche-mode-$(VERSION).tar.gz $(ARCHIVE_DIR_PREFIX)/

archive:
	rm -rf /tmp/gahche-mod-$(VERSION)
	mkdir /tmp/gahche-mod-$(VERSION)
	cp -p $(RELEASE_FILES) /tmp/gahche-mod-$(VERSION)
	chmod 644 /tmp/gahche-mod-$(VERSION)/*
	cd /tmp ; tar cjf gahche-mod-$(VERSION).tar.bz2 gahche-mod-$(VERSION)
	cd /tmp ; tar czf gahche-mod-$(VERSION).tar.gz gahche-mod-$(VERSION)


