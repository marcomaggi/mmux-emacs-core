## mmux-emacs-constant-tests.m4 --
#
# Determine value of C language constants.
#
# LICENSE
#
#   Copyright (c) 2020 Marco Maggi <mrc.mgg@gmail.com>
#
#   Copying and distribution of this file, with or without modification,
#   are permitted in  any medium without royalty  provided the copyright
#   notice and this  notice are preserved.  This file  is offered as-is,
#   without any warranty.
#

dnl $1 - C symbol name
dnl $2 - defined variable stem
AC_DEFUN([MMUX_EMACS_VALUEOF_TEST],[
  VALUEOF_$1="nil"
  AC_CACHE_CHECK([the value of '$2'],
    [mmux_emacs_cv_valueof_$1],
    [AC_COMPUTE_INT([mmux_emacs_cv_valueof_$1],
       [$2],
       [MMUX_EMACS_INCLUDES],
       [mmux_emacs_cv_valueof_$1="#f"])])
   VALUEOF_$1="$mmux_emacs_cv_valueof_$1"
   AC_SUBST([VALUEOF_$1])])

AC_DEFUN([MMUX_EMACS_CONSTANT_TEST],[MMUX_EMACS_VALUEOF_TEST([$1],[$1])])
AC_DEFUN([MMUX_EMACS_CONSTANT_TESTS],[m4_map_args_w($1,[MMUX_EMACS_CONSTANT_TEST(],[)])])

dnl $1 - C symbol name
dnl $2 - defined variable stem
AC_DEFUN([MMUX_EMACS_STRINGOF_TEST],
  [VALUEOF_$1=""
   AC_CACHE_CHECK([the string value of '$1'],
     [mmux_emacs_cv_stringof_$1],
     [AC_RUN_IFELSE([AC_LANG_SOURCE([MMUX_EMACS_INCLUDES
        int main (void)
        {
           FILE *f = fopen ("conftest.val", "w");
           fprintf(f, "%s", $2);
           return ferror (f) || fclose (f) != 0;
        }])],
        [mmux_emacs_cv_stringof_$1=`cat conftest.val`],
        [mmux_emacs_cv_stringof_$1=""],
	[mmux_emacs_cv_stringof_$1=""])
      rm -f conftest.val])
   VALUEOF_$1="$mmux_emacs_cv_stringof_$1"
   AC_SUBST([VALUEOF_$1])])

AC_DEFUN([MMUX_EMACS_STRING_CONSTANT_TEST],[MMUX_EMACS_STRINGOF_TEST([$1],[$1])])
AC_DEFUN([MMUX_EMACS_STRING_CONSTANT_TESTS],[m4_map_args_w($1,[MMUX_EMACS_STRING_CONSTANT_TEST(],[)])])

dnl $1 - C symbol name
dnl $2 - defined variable stem
AC_DEFUN([MMUX_EMACS_DOUBLEOF_TEST],
  [VALUEOF_$1=""
   AC_CACHE_CHECK([the floating point value of '$1'],
     [mmux_emacs_cv_doubleof_$1],
     [AC_RUN_IFELSE([AC_LANG_SOURCE([MMUX_EMACS_INCLUDES
        int main (void)
        {
           FILE *f = fopen ("conftest.val", "w");
           fprintf(f, "%f", $1);
           return ferror (f) || fclose (f) != 0;
        }])],
        [mmux_emacs_cv_doubleof_$1=`cat conftest.val`],
        [mmux_emacs_cv_doubleof_$1=""],
	[mmux_emacs_cv_doubleof_$1="0.0"])
      rm -f conftest.val])
   VALUEOF_$1="$mmux_emacs_cv_doubleof_$1"
   AC_SUBST([VALUEOF_$1])])

AC_DEFUN([MMUX_EMACS_DOUBLE_CONSTANT_TEST],[MMUX_EMACS_DOUBLEOF_TEST([$1],[$1])])
AC_DEFUN([MMUX_EMACS_DOUBLE_CONSTANT_TESTS],[m4_map_args_w($1,[MMUX_EMACS_DOUBLE_CONSTANT_TEST(],[)])])

### end of file
# Local Variables:
# mode: autoconf
# End:
