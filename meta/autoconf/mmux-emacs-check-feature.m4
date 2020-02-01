## mmux-check-emacs-feature.m4 --
#
# Checking GNU Emacs features availability.
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


# Set the shell variable "mmux_emacs_cv_feature_$1" to "yes" or "no".
#
# 1 OUTPUT_VARIABLE_COMPONENT_NAME
# 2 FEATURE_SYMBOL
#
# Usage example:
#
#   MMUX_CHECK_EMACS_FEATURE([GMP],[gmp])
#   AS_IF([test "$mmux_emacs_cv_feature_GMP" = no],[AC_MSG_ERROR([missing feature gmp],1)])
#
AC_DEFUN([MMUX_CHECK_EMACS_FEATURE],
  [AC_CACHE_CHECK([availability of Emacs feature $2],
     [mmux_emacs_cv_feature_$1],
     [AS_IF(["$EMACS" --batch --quick --directory "$am_cv_lispdir" $ELCFLAGS --eval "(require (quote $2))"],
            [AS_VAR_SET([mmux_emacs_cv_feature_$1],[yes])],
            [AS_VAR_SET([mmux_emacs_cv_feature_$1],[no])])])])

### end of file
# Local Variables:
# mode: autoconf
# End:
