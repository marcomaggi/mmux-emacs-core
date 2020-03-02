# @configure_input@
#

AC_PREREQ([2.69])
MMUX_PKG_VERSIONS([0],[1],[0],[devel.0])
AC_INIT([MMUX Emacs Core],[MMUX_PACKAGE_VERSION],
  [mrc.mgg@gmail.com],
  [mmux-emacs-core],[http://github.com/marcomaggi/mmux-emacs-core])
MMUX_INIT
MMUX_LIBTOOL_LIBRARY_VERSIONS([mmux_emacs_core],1,0,0)
AC_REVISION([0.1])
AC_COPYRIGHT([Copyright (c) 2020 Marco Maggi <mrc.mgg@gmail.com>

This program is free software: you  can redistribute it and/or modify it
under the  terms of the GNU  General Public License as  published by the
Free Software Foundation,  either version 3 of the License,  or (at your
option) any later version.

This program  is distributed  in the  hope that it  will be  useful, but
WITHOUT   ANY   WARRANTY;  without   even   the   implied  warranty   of
MERCHANTABILITY  or  FITNESS FOR  A  PARTICULAR  PURPOSE.  See  the  GNU
General Public License for more details.

You should have received a copy  of the GNU General Public License along
with this program.  If not, see <http://www.gnu.org/licenses/>.
])
AC_CONFIG_SRCDIR([src/])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([1.16 foreign subdir-objects])
AM_MAINTAINER_MODE

AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P

AX_PROG_EMACS
AM_PATH_LISPDIR
AC_CHECK_HEADER([emacs-module.h],,[AC_MSG_ERROR([missing emacs-module.h],1)])
# If the variabe "ELCFLAGS" is used  on the command line of "configure":
# we cache it into "mmux_emacs_cv_ELCFLAGS" and make its value available
# in the Makefile with the substitution "MMUX_EMACS_ELCFLAGS".
AC_CACHE_VAL([mmux_emacs_cv_ELCFLAGS],
  [AS_VAR_SET([mmux_emacs_cv_ELCFLAGS],["$ELCFLAGS"])])
AC_SUBST([MMUX_EMACS_ELCFLAGS],[$mmux_emacs_cv_ELCFLAGS])

LT_PREREQ([2.4])
LT_INIT


#### basic system inspection

MMUX_CHECK_TARGET_OS
AX_IS_RELEASE([git-directory])

AC_LANG([C])
MMUX_LANG_C11

AM_PROG_AS
AM_PROG_CC_C_O
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([assert.h stdint.h strings.h errno.h stddef.h limits.h unistd.h float.h math.h])

AC_CACHE_SAVE


#### external libraries

PKG_PREREQ([0.29])
PKG_INSTALLDIR
MMUX_CHECK_PKG_CONFIG_MACROS

AC_CHECK_LIB(m,sin)

AC_CACHE_SAVE


#### checks for library functions

m4_define([MMUX_EMACS_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif
#ifdef HAVE_STDDEF_H
#  include <stddef.h>
#endif
#ifdef HAVE_STDINT_H
#  include <stdint.h>
#endif
#ifdef HAVE_FLOAT_H
#  include <float.h>
#endif
#ifdef HAVE_MATH_H
#  include <math.h>
#endif
])

MMUX_EMACS_CONSTANT_TESTS([CHAR_MIN CHAR_MAX SCHAR_MIN SCHAR_MAX UCHAR_MAX WCHAR_MAX])
MMUX_EMACS_CONSTANT_TESTS([SHRT_MIN SHRT_MAX USHRT_MAX INT_MIN INT_MAX UINT_MAX LONG_MIN LONG_MAX ULONG_MAX LLONG_MIN LLONG_MAX ULLONG_MAX])

dnl Commented out because not all the platforms implement these.  (Marco
dnl Maggi; Feb 1, 2020)
dnl
dnl MMUX_EMACS_CONSTANT_TESTS([INTMAX_MIN INTMAX_MAX UINTMAX_MAX])

MMUX_DETERMINE_SIZEOF([CHAR],[char],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([SIGNED_CHAR],[signed char],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([UNSIGNED_CHAR],[unsigned char],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([WCHAR],[wchar_t],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([SIGNED_SHORT_INT],[signed short int],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([UNSIGNED_SHORT_INT],[unsigned short int],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([SIGNED_INT],[signed int],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([UNSIGNED_INT],[unsigned int],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([SIGNED_LONG_INT],[signed long int],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([UNSIGNED_LONG_INT],[unsigned long int],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([SIGNED_LONG_LONG_INT],[signed long long int],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([UNSIGNED_LONG_LONG_INT],[unsigned long long int],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([SIZE_T],[size_t],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([SSIZE_T],[ssize_t],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([INTMAX_T],[intmax_t],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([UINTMAX_T],[uintmax_t],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([PTRDIFF_T],[ptrdiff_t],[MMUX_EMACS_INCLUDES])
MMUX_DETERMINE_SIZEOF([FLOAT],[float])
MMUX_DETERMINE_SIZEOF([DOUBLE],[double])
MMUX_DETERMINE_SIZEOF([LDOUBLE],[long double])

AC_SUBST([VALUEOF_MMUX_SIZEOF_CHAR],[$MMUX_SIZEOF_CHAR])
AC_SUBST([VALUEOF_MMUX_SIZEOF_SIGNED_CHAR],[$MMUX_SIZEOF_SIGNED_CHAR])
AC_SUBST([VALUEOF_MMUX_SIZEOF_UNSIGNED_CHAR],[$MMUX_SIZEOF_UNSIGNED_CHAR])
AC_SUBST([VALUEOF_MMUX_SIZEOF_WCHAR],[$MMUX_SIZEOF_WCHAR])
AC_SUBST([VALUEOF_MMUX_SIZEOF_SIGNED_SHORT_INT],[$MMUX_SIZEOF_SIGNED_SHORT_INT])
AC_SUBST([VALUEOF_MMUX_SIZEOF_UNSIGNED_SHORT_INT],[$MMUX_SIZEOF_UNSIGNED_SHORT_INT])
AC_SUBST([VALUEOF_MMUX_SIZEOF_SIGNED_INT],[$MMUX_SIZEOF_SIGNED_INT])
AC_SUBST([VALUEOF_MMUX_SIZEOF_UNSIGNED_INT],[$MMUX_SIZEOF_UNSIGNED_INT])
AC_SUBST([VALUEOF_MMUX_SIZEOF_SIGNED_LONG_INT],[$MMUX_SIZEOF_SIGNED_LONG_INT])
AC_SUBST([VALUEOF_MMUX_SIZEOF_UNSIGNED_LONG_INT],[$MMUX_SIZEOF_UNSIGNED_LONG_INT])
AC_SUBST([VALUEOF_MMUX_SIZEOF_SIGNED_LONG_LONG_INT],[$MMUX_SIZEOF_SIGNED_LONG_LONG_INT])
AC_SUBST([VALUEOF_MMUX_SIZEOF_UNSIGNED_LONG_LONG_INT],[$MMUX_SIZEOF_UNSIGNED_LONG_LONG_INT])
AC_SUBST([VALUEOF_MMUX_SIZEOF_SIZE_T],[$MMUX_SIZEOF_SIZE_T])
AC_SUBST([VALUEOF_MMUX_SIZEOF_SSIZE_T],[$MMUX_SIZEOF_SSIZE_T])
AC_SUBST([VALUEOF_MMUX_SIZEOF_INTMAX_T],[$MMUX_SIZEOF_INTMAX_T])
AC_SUBST([VALUEOF_MMUX_SIZEOF_UINTMAX_T],[$MMUX_SIZEOF_UINTMAX_T])
AC_SUBST([VALUEOF_MMUX_SIZEOF_PTRDIFF_T],[$MMUX_SIZEOF_PTRDIFF_T])
AC_SUBST([VALUEOF_MMUX_SIZEOF_FLOAT],[$MMUX_SIZEOF_FLOAT])
AC_SUBST([VALUEOF_MMUX_SIZEOF_DOUBLE],[$MMUX_SIZEOF_DOUBLE])
AC_SUBST([VALUEOF_MMUX_SIZEOF_LDOUBLE],[$MMUX_SIZEOF_LDOUBLE])

MMUX_EMACS_CONSTANT_TESTS([FLT_ROUNDS FLT_RADIX FLT_MANT_DIG DBL_MANT_DIG LDBL_MANT_DIG FLT_DIG DBL_DIG LDBL_DIG FLT_MIN_EXP DBL_MIN_EXP LDBL_MIN_EXP FLT_MIN_10_EXP DBL_MIN_10_EXP LDBL_MIN_10_EXP FLT_MAX_EXP DBL_MAX_EXP LDBL_MAX_EXP FLT_MAX_10_EXP DBL_MAX_10_EXP LDBL_MAX_10_EXP])

MMUX_EMACS_DOUBLE_CONSTANT_TESTS([FLT_MAX DBL_MAX FLT_MIN DBL_MIN FLT_EPSILON DBL_EPSILON])
MMUX_EMACS_LDOUBLE_CONSTANT_TESTS([LDBL_MAX LDBL_MIN LDBL_EPSILON])

AC_CHECK_FUNCS([pow sqrt cbrt hypot expm1 log1p exp exp2 exp10 log log2 log10 logb])
AC_CHECK_FUNCS([powf sqrtf cbrtf hypotf expm1f log1pf expf exp2f exp10f logf log2f log10f logbf])
AC_CHECK_FUNCS([powl sqrtl cbrtl hypotl expm1l log1pl expl exp2l exp10l logl log2l log10l logbl])

AC_CHECK_FUNCS([sin cos tan asin acos atan atan2])
AC_CHECK_FUNCS([sinf cosf tanf asinf acosf atanf atan2f])
AC_CHECK_FUNCS([sinl cosl tanl asinl acosl atanl atan2l])

AC_CHECK_FUNCS([sinh cosh tanh asinh acosh atanh])
AC_CHECK_FUNCS([sinhf coshf tanhf asinhf acoshf atanhf])
AC_CHECK_FUNCS([sinhl coshl tanhl asinhl acoshl atanhl])

AC_CACHE_SAVE


#### finish

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
  [src/mmec-constants.el]
  [src/mmux-emacs-core-constants.h]
  [meta/scripts/mmux-emacs-core.pc]
  [meta/slackware/slack-desc])
MMUX_OUTPUT
AC_OUTPUT

### end of file
