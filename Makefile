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

VERSION = $(shell grep "^;; Version:" gosh-mode.el | sed -e "s/;; Version: \(.*\)/\1/")

RELEASE_FILES = \
	gosh-mode.el gosh-config.el gosh-const.el \
	gosh-mode-test.el gosh-mode-make.el gosh-stub.el \
	gosh-mode-pkg.el \
	MAKE-CFG.el Makefile \
	README.md

PACKAGE = gosh-mode

EXCLUDE_PACKAGE = \
	gosh-mode-test.el gosh-mode-make.el \
	MAKE-CFG.el Makefile \
	README.md

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

release: archive package
	$(RM) -f $(ARCHIVE_DIR_PREFIX)/$(PACKAGE)-$(VERSION).tar.bz2 $(ARCHIVE_DIR_PREFIX)/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar /tmp/$(PACKAGE)-$(VERSION).tar.bz2 /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ARCHIVE_DIR_PREFIX)/

archive: prepare
	cd /tmp ; tar cjf $(PACKAGE)-$(VERSION).tar.bz2 $(PACKAGE)-$(VERSION)
	cd /tmp ; tar czf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)

package: prepare
	cd /tmp/$(PACKAGE)-$(VERSION) ; \
	rm -rf $(EXCLUDE_PACKAGE) ; \
	cd .. ; \
	tar cf $(PACKAGE)-$(VERSION).tar $(PACKAGE)-$(VERSION)

prepare:
	rm -rf /tmp/$(PACKAGE)-$(VERSION)
	mkdir /tmp/$(PACKAGE)-$(VERSION)
	cp -pr $(RELEASE_FILES) /tmp/$(PACKAGE)-$(VERSION)
	chmod 644 /tmp/$(PACKAGE)-$(VERSION)/*
