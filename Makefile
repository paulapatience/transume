#* Makefile — Recipes for building Transume artifacts
#
# SPDX-FileCopyrightText: Copyright (c) 2023 Paul A. Patience <paul@apatience.com>
# SPDX-License-Identifier: MIT

#* Variables
#** Configuration variables

# Keep zero-indexed line number in sync in ASD file.
VERSION =\
0.0

INSTALL = install
INSTALL_DATA = ${INSTALL} -m 0644

SHA256 = sha256sum --tag

SBCL = sbcl
BUILD_DOCUMENTATION = ${SBCL} --script scripts/build-documentation.lisp

#** Internal variables

LISPSRCS =\
	documentation.lisp\
	transume.asd\
	transume.lisp

# Makefile added separately to dist.
DATAFILES =\
	LICENSE\
	README.markdown

CONTRIBFILES =\
	contrib/generate-documentation.lisp\
	contrib/mgl-pax-filter.lua

SCRIPTFILES = scripts/build-documentation.lisp

DISTFILES = ${LISPSRCS} ${DATAFILES} ${CONTRIBFILES} ${SCRIPTFILES}

#* Targets
#** User targets

.PHONY: all
all:

.PHONY: check
check:

.PHONY: markdown
markdown: transume.markdown

transume.markdown: ${LISPSRCS} contrib/generate-documentation.lisp scripts/build-documentation.lisp
	${BUILD_DOCUMENTATION} -o $@

.PHONY: pdf
pdf: transume.pdf

transume.pdf: ${LISPSRCS} contrib/generate-documentation.lisp scripts/build-documentation.lisp
	${BUILD_DOCUMENTATION} -o $@

.PHONY: clean
clean:
	rm -f transume-*.tar.gz transume-*.sha256

.PHONY: distclean
distclean: clean

#** Maintainer targets

.PHONY: dist
dist: transume-${VERSION}.sha256

# bmake recognizes $< only in suffix rules.
transume-${VERSION}.sha256: transume-${VERSION}.tar.gz
	${SHA256} transume-${VERSION}.tar.gz >$@

# Intentionally fail if the directory already exists.
transume-${VERSION}.tar.gz: ${DISTFILES}
	mkdir transume-${VERSION}
	mkdir transume-${VERSION}/contrib
	mkdir transume-${VERSION}/scripts
	sed '/^VERSION/{n;s/.*/${VERSION}/;}' Makefile >transume-${VERSION}/Makefile
	${INSTALL_DATA} ${LISPSRCS} ${DATAFILES} transume-${VERSION}
	${INSTALL_DATA} ${CONTRIBFILES} transume-${VERSION}/contrib
	${INSTALL_DATA} ${SCRIPTFILES} transume-${VERSION}/scripts
	tar -czf $@ transume-${VERSION}
	rm -rf transume-${VERSION}

#* Suffix rules

.SUFFIXES:
