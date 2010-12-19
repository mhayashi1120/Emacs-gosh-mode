# -*- mode: makefile; -*-
#
# Makefile for gauche-ext
#

TAR	= tar
RM	= /bin/rm -f

EMACS	= emacs

FLAGS   = -batch -q -no-site-file -l gauche-ext-make.el

# For windows emacs
CHECKFLAGS = $(FLAGS)

VERSION = 0.1.0

RELEASE_FILES = \
	gauche-browse.el gauche-config.el gauche-const.el \
	gauche-env.el gauche-refactor.el \
	refactor.el \
	scm-browse.el scm-const.el\
	scm-edit.el scm-env.el\
	Makefile

ARCHIVE_DIR_PREFIX = ..

GOMI	= *.elc *~

default: elc

check: clean
	$(EMACS) $(CHECKFLAGS) -f check-gauche-ext $(CONFIG)

elc:
	$(EMACS) $(FLAGS) -f compile-gauche-ext $(CONFIG)

what-where:
	$(EMACS) $(FLAGS) -f what-where-gauche-ext $(CONFIG)

install: elc
	$(EMACS) $(FLAGS) -f install-gauche-ext $(CONFIG)

clean:
	-$(RM) $(GOMI)

release: archive
	$(RM) -f $(ARCHIVE_DIR_PREFIX)/gauche-ext-$(VERSION).tar.bz2 $(ARCHIVE_DIR_PREFIX)/gauche-ext-$(VERSION).tar.gz
	mv /tmp/gauche-ext-$(VERSION).tar.bz2 /tmp/gauche-ext-$(VERSION).tar.gz $(ARCHIVE_DIR_PREFIX)/

archive:
	rm -rf /tmp/gahche-mod-$(VERSION)
	mkdir /tmp/gahche-mod-$(VERSION)
	cp -p $(RELEASE_FILES) /tmp/gahche-mod-$(VERSION)
	chmod 644 /tmp/gahche-mod-$(VERSION)/*
	cd /tmp ; tar cjf gahche-mod-$(VERSION).tar.bz2 gahche-mod-$(VERSION)
	cd /tmp ; tar czf gahche-mod-$(VERSION).tar.gz gahche-mod-$(VERSION)


