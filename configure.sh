# configure.sh --
#

set -xe

prefix=/usr/local
if test -d /lib64
then libdir=${prefix}/lib64
else libdir=${prefix}/lib
fi

ELCFLAGS='--directory /usr/local/lib64 --directory /usr/local/share/emacs/site-lisp'

../configure \
    --enable-maintainer-mode				\
    --config-cache					\
    --cache-file=../config.cache			\
    --prefix="${prefix}"				\
    --libdir="${libdir}"				\
    ELCFLAGS="$ELCFLAGS"				\
    "$@"

### end of file
