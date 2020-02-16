## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I meta/autotools
AUTOMAKE_OPTIONS	= foreign
EXTRA_DIST		= INSTALL			\
			  meta/slackware/slack-desc.in
dist_doc_DATA		= README COPYING
AM_CFLAGS		= $(MMUX_CFLAGS)
AM_CPPFLAGS		= -I$(srcdir)/src -I$(builddir)/src
CLEANFILES		=

AM_DISTCHECK_CONFIGURE_FLAGS = ELCFLAGS="$(MMUX_EMACS_ELCFLAGS)"

## --------------------------------------------------------------------

nodist_pkgconfig_DATA	= meta/scripts/mmux-emacs-core.pc


#### documentation

EXTRA_DIST		+= doc/mmux-emacs-core.style.css
CLEANFILES		+= doc/mmux-emacs-core.css

AM_MAKEINFOFLAGS	= --no-split
AM_MAKEINFOHTMLFLAGS	= --split=node -c WORDS_IN_PAGE=0 --css-ref=mmux-emacs-gmp.css \
	-c PRE_BODY_CLOSE="<p>This document describes version <tt>$(PACKAGE_VERSION)</tt> of <em>$(PACKAGE_NAME)</em>.</p>"

info_TEXINFOS		= doc/mmux-emacs-core.texi
doc_mmux_emacs_core_TEXINFOS = \
	doc/macros.texi			\
	doc/elisp-macros.texi		\
	doc/overview-linking.texi	\
	doc/gpl-3.0.texi		\
	doc/fdl-1.3.texi

## --------------------------------------------------------------------

doc/mmux-emacs-core.html/$(am__dirstamp): doc/$(am__dirstamp)
	@$(MKDIR_P) doc/mmux-emacs-core.html/
	@: > doc/mmux-emacs-core.html/$(am__dirstamp)

doc/mmux-emacs-core.html/mmux-emacs-core.css: doc/mmux-emacs-core.style.css doc/mmux-emacs-core.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/doc/mmux-emacs-core.style.css" "$(builddir)/doc/mmux-emacs-core.html/mmux-emacs-core.css"

## --------------------------------------------------------------------

html-local: doc/mmux-emacs-core.html/mmux-emacs-core.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)/mmux-emacs-core.html/"
	$(INSTALL) -m 0444 $(builddir)/doc/mmux-emacs-core.html/mmux-emacs-core.css "$(DESTDIR)$(htmldir)/mmux-emacs-core.html/"


#### libraries

mmux_emacs_core_CURRENT		= @mmux_emacs_core_VERSION_INTERFACE_CURRENT@
mmux_emacs_core_REVISION	= @mmux_emacs_core_VERSION_INTERFACE_REVISION@
mmux_emacs_core_AGE		= @mmux_emacs_core_VERSION_INTERFACE_AGE@
mmux_emacs_core_LINKER_VERSION	= -version-info $(mmux_emacs_core_CURRENT):$(mmux_emacs_core_REVISION):$(mmux_emacs_core_AGE)

include_HEADERS			= \
	src/mmux-emacs-core.h		\
	src/mmux-emacs-core-constants.h

lib_LTLIBRARIES			= libmmux-emacs-core.la
libmmux_emacs_core_la_CPPFLAGS	= $(AM_CPPFLAGS)
libmmux_emacs_core_la_CFLAGS	= $(AM_CFLAGS)
libmmux_emacs_core_la_LDFLAGS	= $(mmux_emacs_core_LINKER_VERSION) $(AM_LDFLAGS)
libmmux_emacs_core_la_SOURCES	= \
	src/mmux-emacs-core.h				\
	src/mmux-emacs-core-constants.h			\
	src/mmux-emacs-core-internals.h			\
	src/mmux-emacs-core-library.c			\
	src/mmux-emacs-core-numbers.c			\
	src/mmux-emacs-core-bytevectors.c		\
	src/mmux-emacs-core-errors.c

MMUX_EMACS_CORE_OBJ_PREREQ	= \
	src/mmux-emacs-core.h		\
	src/mmux-emacs-core-constants.h	\
	src/mmux-emacs-core-internals.h

src/mmux-emacs-core-errors.$(OBJEXT):			$(MMUX_EMACS_CORE_OBJ_PREREQ)
src/mmux-emacs-core-library.$(OBJEXT):			$(MMUX_EMACS_CORE_OBJ_PREREQ)
src/mmux-emacs-core-user-pointer-objects.$(OBJEXT):	$(MMUX_EMACS_CORE_OBJ_PREREQ)
src/mmux-emacs-core-numbers.$(OBJEXT):			$(MMUX_EMACS_CORE_OBJ_PREREQ)
src/mmux-emacs-core-bytevectors.$(OBJEXT):		$(MMUX_EMACS_CORE_OBJ_PREREQ)

.PHONY: library

library: libmmux-emacs-core.la


#### Emacs code

# This  variable is  reserved to  the  package author.   Users of  the
# package can set ELCFLAGS.
AM_ELCFLAGS	= --quick --directory $(builddir)/src --directory $(PWD)/.libs $(MMUX_EMACS_ELCFLAGS)

dist_lisp_LISP	= \
	src/mmec-constants.el				\
	src/mmec-basics.el				\
	src/mmec-number-objects.el			\
	src/mmec-bytevector-objects.el			\
	src/mmec.el

src/mmec-basics.elc: \
	src/mmec-constants.elc				\
	libmmux-emacs-core.la

src/mmec-number-objects.elc: \
	src/mmec-basics.elc				\
	src/mmec-constants.elc				\
	libmmux-emacs-core.la

src/mmec-bytevector-objects.elc: \
	src/mmec-number-objects.elc			\
	src/mmec-basics.elc				\
	src/mmec-constants.elc				\
	libmmux-emacs-core.la

src/mmec.elc: \
	src/mmec-bytevector-objects.elc			\
	src/mmec-number-objects.elc			\
	src/mmec-basics.elc				\
	src/mmec-constants.elc				\
	libmmux-emacs-core.la


#### tests

MMUX_EMACS_CORE_TESTS_SRC = \
	tests/version.el		\
	tests/user-pointer-objects.el	\
	tests/ctypes-comparison.el	\
	tests/bytevector-objects.el

MMUX_EMACS_CORE_TESTS = \
	tests/version.elc		\
	tests/user-pointer-objects.elc	\
	tests/ctypes-comparison.elc	\
	tests/bytevector-objects.elc

EXTRA_DIST	+= $(MMUX_EMACS_CORE_TESTS_SRC)
TESTS		 = $(MMUX_EMACS_CORE_TESTS)

MMUX_EMACS_CHECK_FLAGS	= --batch --quick --directory $(builddir)/.libs --directory $(builddir)/src $(MMUX_EMACS_ELCFLAGS) --load

# We must make sure that this ends with a semicolon!!!
AM_TESTS_ENVIRONMENT	=
TEST_EXTENSIONS		= .elc
ELC_LOG_COMPILER	= $(EMACS)
AM_ELC_LOG_FLAGS	= $(MMUX_EMACS_CHECK_FLAGS)
CLEANFILES		+= $(MMUX_EMACS_CORE_TESTS)

## --------------------------------------------------------------------

MMUX_EMACS_INSTALLCHECK	= $(EMACS) --batch --quick --directory $(libdir) --directory $(lispdir) --load

installcheck-local:
	@for f in $(MMUX_EMACS_CORE_TESTS_SRC); do $(MMUX_EMACS_INSTALLCHECK) $(srcdir)/$$f; done

## --------------------------------------------------------------------

MMUX_EMACS_CORE_TEST_SELECTED_TESTS	= $(srcdir)/tests/*$(file)*.el
MMUX_EMACS_CORE_TEST_RUNTESTS		= $(EMACS) $(MMUX_EMACS_CHECK_FLAGS)

test:
	for f in $(MMUX_EMACS_CORE_TEST_SELECTED_TESTS) ; do $(MMUX_EMACS_CORE_TEST_RUNTESTS) $$f; done


#### common rules

include $(top_srcdir)/meta/automake/mmux-makefile-rules

### end of file
