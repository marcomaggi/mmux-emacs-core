/*
  Part of: MMUX Emacs Core
  Contents: facilities for number objects
  Date: Feb  4, 2020

  Abstract

	This module implements facilitie for number objects.

  Copyright (C) 2020 Marco Maggi <mrc.mgg@gmail.com>

  This program is free  software: you can redistribute it and/or  modify it under the
  terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
  Foundation, either version 3 of the License, or (at your option) any later version.

  This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
  WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
  PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  You should have received  a copy of the GNU General Public  License along with this
  program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "mmux-emacs-core-internals.h"
#include <string.h>
#include <stdlib.h>


/** --------------------------------------------------------------------
 ** Range checker: is a number fitting into a C language type?
 ** ----------------------------------------------------------------- */

#undef  MMUX_EMACS_CORE_FITS_SIGNED_P
#define MMUX_EMACS_CORE_FITS_SIGNED_P(CTYPESTEM, FUNCSTEM, LIMSTEM)	\
  static emacs_value							\
  Fmmux_emacs_core_fits_ ## FUNCSTEM ## _p (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(1 == nargs);							\
    mmux_emacs_core_type_ ## CTYPESTEM ## _t	val = mmux_emacs_core_get_ ## CTYPESTEM(env, args[0]); \
    mmux_emacs_core_type_ ## CTYPESTEM ## _t	min = MMUX_EMACS_CORE_ ## LIMSTEM ## _MIN; \
    mmux_emacs_core_type_ ## CTYPESTEM ## _t	max = MMUX_EMACS_CORE_ ## LIMSTEM ## _MAX; \
    return mmux_emacs_core_make_boolean(env, (((min <= val) && (val <= max))? 1 : 0)); \
  }

#undef  MMUX_EMACS_CORE_FITS_UNSIGNED_P
#define MMUX_EMACS_CORE_FITS_UNSIGNED_P(CTYPESTEM, FUNCSTEM, LIMSTEM)	\
  static emacs_value							\
  Fmmux_emacs_core_fits_ ## FUNCSTEM ## _p (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(1 == nargs);							\
    mmux_emacs_core_type_ ## CTYPESTEM ## _t	val = mmux_emacs_core_get_ ## CTYPESTEM(env, args[0]); \
    mmux_emacs_core_type_ ## CTYPESTEM ## _t	max = MMUX_EMACS_CORE_ ## LIMSTEM ## _MAX; \
    return mmux_emacs_core_make_boolean(env, ((val <= max)? 1 : 0));	\
  }

MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		char,		CHAR)
MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		schar,		SCHAR)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		uchar,		UCHAR)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		wchar,		WCHAR)

MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		sshrt,		SSHRT)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		ushrt,		USHRT)
MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		sint,		SINT)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		uint,		UINT)
MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		slong,		SLONG)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		ulong,		ULONG)
MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		sllong,		SLLONG)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		ullong,		ULLONG)

MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		ssize,		SSIZE)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		usize,		USIZE)
MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		sintmax,	SINTMAX)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		uintmax,	UINTMAX)
MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		ptrdiff,	PTRDIFF)

MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		sint8,		SINT8)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		uint8,		UINT8)
MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		sint16,		SINT16)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		uint16,		UINT16)
MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		sint32,		SINT32)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		uint32,		UINT32)
MMUX_EMACS_CORE_FITS_SIGNED_P(sint64,		sint64,		SINT64)
MMUX_EMACS_CORE_FITS_UNSIGNED_P(uint64,		uint64,		UINT64)

MMUX_EMACS_CORE_FITS_SIGNED_P(long_double,	float,		FLOAT)
MMUX_EMACS_CORE_FITS_SIGNED_P(long_double,	double,		DOUBLE)
MMUX_EMACS_CORE_FITS_SIGNED_P(long_double,	long_double,	LONG_DOUBLE)


/** --------------------------------------------------------------------
 ** Conversion functions.
 ** ----------------------------------------------------------------- */

#undef  MMUX_EMACS_CORE_CONVERSION_TO_SINT64
#define MMUX_EMACS_CORE_CONVERSION_TO_SINT64(STEM)			\
  static emacs_value							\
  Fmmux_emacs_core_ ## STEM ## _to_sint64 (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(1 == nargs);							\
    int64_t	rv = mmux_emacs_core_get_ ## STEM(env, args[0]);	\
    if (0) {								\
      fprintf(stderr, "%s: %ld\n", __func__, (long)rv);			\
    }									\
    return mmux_emacs_core_make_sint64(env, rv);			\
  }

#undef  MMUX_EMACS_CORE_CONVERSION_TO_UINT64
#define MMUX_EMACS_CORE_CONVERSION_TO_UINT64(STEM)			\
  static emacs_value							\
  Fmmux_emacs_core_ ## STEM ## _to_uint64 (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(1 == nargs);							\
    uint64_t	rv = mmux_emacs_core_get_ ## STEM(env, args[0]);	\
    if (0) {								\
      fprintf(stderr, "%s: %ld\n", __func__, (long)rv);			\
    }									\
    return mmux_emacs_core_make_uint64(env, rv);			\
  }

#undef  MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64
#define MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(STEM)	\
  MMUX_EMACS_CORE_CONVERSION_TO_SINT64(s ## STEM)		\
  MMUX_EMACS_CORE_CONVERSION_TO_UINT64(u ## STEM)

MMUX_EMACS_CORE_CONVERSION_TO_SINT64(char)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(char)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(shrt)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(int)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(long)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(llong)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(intmax)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(size)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(int8)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(int16)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64_AND_UINT64(int32)
MMUX_EMACS_CORE_CONVERSION_TO_SINT64(ptrdiff)
MMUX_EMACS_CORE_CONVERSION_TO_UINT64(wchar)

static emacs_value
Fmmux_emacs_core_float_to_long_double (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(1 == nargs);
  return mmux_emacs_core_make_long_double(env, mmux_emacs_core_get_float(env, args[0]));
}

static emacs_value
Fmmux_emacs_core_double_to_long_double (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(1 == nargs);
  return mmux_emacs_core_make_long_double(env, mmux_emacs_core_get_double(env, args[0]));
}

static emacs_value
Fmmux_emacs_core_sint64_to_long_double (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(1 == nargs);
  return mmux_emacs_core_make_long_double(env, (long double)mmux_emacs_core_get_sint64(env, args[0]));
}

static emacs_value
Fmmux_emacs_core_uint64_to_long_double (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(1 == nargs);
  return mmux_emacs_core_make_long_double(env, (long double)mmux_emacs_core_get_uint64(env, args[0]));
}


/** --------------------------------------------------------------------
 ** Comparison functions.
 ** ----------------------------------------------------------------- */

#undef  MMUX_EMACS_CORE_COMPARISON_OPERATION
#define MMUX_EMACS_CORE_COMPARISON_OPERATION(OPNAME, OPERATOR, STEM, CTYPE) \
  static emacs_value							\
  Fmmux_emacs_core_compare_ ## STEM ## _ ## OPNAME(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(2 == nargs);							\
    CTYPE	op1	= mmux_emacs_core_get_ ## STEM(env, args[0]);	\
    CTYPE	op2	= mmux_emacs_core_get_ ## STEM(env, args[1]);	\
    int		rv	= ((op1 OPERATOR op2)? 1 : 0);			\
									\
    if (0) {								\
      fprintf(stderr, "%s: op1=%ld op2=%ld rv=%d\n", __func__,		\
	      (long)op1, (long)op2, rv);				\
    }									\
    return mmux_emacs_core_make_boolean(env, rv);			\
  }

#define MMUX_EMACS_CORE_COMPARISON_OPERATIONS(STEM, CTYPE)		\
  MMUX_EMACS_CORE_COMPARISON_OPERATION(equal,		==,	STEM,	CTYPE) \
  MMUX_EMACS_CORE_COMPARISON_OPERATION(less,		<,	STEM,	CTYPE) \
  MMUX_EMACS_CORE_COMPARISON_OPERATION(greater,		>,	STEM,	CTYPE) \
  MMUX_EMACS_CORE_COMPARISON_OPERATION(less_equal,	<=,	STEM,	CTYPE) \
  MMUX_EMACS_CORE_COMPARISON_OPERATION(greater_equal,	>=,	STEM,	CTYPE) \
  MMUX_EMACS_CORE_COMPARISON_OPERATION(not_equal,	!=,	STEM,	CTYPE)

MMUX_EMACS_CORE_COMPARISON_OPERATIONS(uint64,		uint64_t)
MMUX_EMACS_CORE_COMPARISON_OPERATIONS(sint64,		int64_t)
MMUX_EMACS_CORE_COMPARISON_OPERATIONS(long_double,	long double)

/* ------------------------------------------------------------------ */

/* FIXME I do not know how to compare a signed integer and an unsigne integer without
   loosing bits; maybe, in future, GNU Emacs will support big integers with GMP.  For
   now it is an error.  (Marco Maggi; Feb 7, 2020)*/
#undef  MMUX_EMACS_CORE_COMPARISON_OPERATION2
#define MMUX_EMACS_CORE_COMPARISON_OPERATION2(OPNAME, OPERATOR, STEM1, CTYPE1, STEM2, CTYPE2) \
  static emacs_value							\
  Fmmux_emacs_core_compare_ ## STEM1 ## _ ## STEM2 ## _ ## OPNAME(emacs_env *env, ptrdiff_t nargs, \
								  emacs_value args[] MMUX_EMACS_CORE_UNUSED, \
								  void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(2 == nargs);							\
    return mmux_emacs_core_error_signed_unsigned_integer_comparison(env); \
  }
#if 0
  {									\
    assert(2 == nargs);							\
    CTYPE1	op1	= mmux_emacs_core_get_ ## STEM1(env, args[0]);	\
    CTYPE2	op2	= mmux_emacs_core_get_ ## STEM2(env, args[1]);	\
    int		rv	= ((op1 OPERATOR op2)? 1 : 0);			\
									\
    if (0) {								\
      fprintf(stderr, "%s: op1=%ld op2=%ld rv=%d\n", __func__,		\
	      (long)op1, (long)op2, rv);				\
    }									\
    return mmux_emacs_core_make_boolean(env, rv);			\
  }
#endif

#define MMUX_EMACS_CORE_COMPARISON_OPERATIONS2(STEM1, CTYPE1, STEM2, CTYPE2)	\
  MMUX_EMACS_CORE_COMPARISON_OPERATION2(equal,		==,	STEM1,	CTYPE1,	STEM2,	CTYPE2) \
  MMUX_EMACS_CORE_COMPARISON_OPERATION2(less,		<,	STEM1,	CTYPE1,	STEM2,	CTYPE2) \
  MMUX_EMACS_CORE_COMPARISON_OPERATION2(greater,	>,	STEM1,	CTYPE1,	STEM2,	CTYPE2) \
  MMUX_EMACS_CORE_COMPARISON_OPERATION2(less_equal,	<=,	STEM1,	CTYPE1,	STEM2,	CTYPE2) \
  MMUX_EMACS_CORE_COMPARISON_OPERATION2(greater_equal,	>=,	STEM1,	CTYPE1,	STEM2,	CTYPE2) \
  MMUX_EMACS_CORE_COMPARISON_OPERATION2(not_equal,	!=,	STEM1,	CTYPE1,	STEM2,	CTYPE2)

MMUX_EMACS_CORE_COMPARISON_OPERATIONS2(sint64,  int64_t, uint64, uint64_t)
MMUX_EMACS_CORE_COMPARISON_OPERATIONS2(uint64, uint64_t, sint64,  int64_t)


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_FUNCTIONS	85
static mmux_emacs_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  /* Conversion functions to "sint64". */
  {
    .name		= "mmux-core-c-char-to-sint64",
    .implementation	= Fmmux_emacs_core_char_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `char' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-schar-to-sint64",
    .implementation	= Fmmux_emacs_core_schar_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `schar' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-sshrt-to-sint64",
    .implementation	= Fmmux_emacs_core_sshrt_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `sshrt' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-sint-to-sint64",
    .implementation	= Fmmux_emacs_core_sint_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `sint' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-slong-to-sint64",
    .implementation	= Fmmux_emacs_core_slong_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `slong' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-sllong-to-sint64",
    .implementation	= Fmmux_emacs_core_sllong_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `sllong' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-sint8-to-sint64",
    .implementation	= Fmmux_emacs_core_sint8_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `sint8' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-sint16-to-sint64",
    .implementation	= Fmmux_emacs_core_sint16_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `sint16' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-sint32-to-sint64",
    .implementation	= Fmmux_emacs_core_sint32_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `sint32' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-ssize-to-sint64",
    .implementation	= Fmmux_emacs_core_ssize_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `ssize' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-sintmax-to-sint64",
    .implementation	= Fmmux_emacs_core_sintmax_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `sintmax' to a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-ptrdiff-to-sint64",
    .implementation	= Fmmux_emacs_core_ptrdiff_to_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `ptrdiff' to a user-pointer object of type `sint64'.",
  },

  /* Conversion functions to "uint64". */
  {
    .name		= "mmux-core-c-uchar-to-uint64",
    .implementation	= Fmmux_emacs_core_uchar_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `uchar' to a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmux-core-c-ushrt-to-uint64",
    .implementation	= Fmmux_emacs_core_ushrt_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `ushrt' to a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmux-core-c-uint-to-uint64",
    .implementation	= Fmmux_emacs_core_uint_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `uint' to a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmux-core-c-ulong-to-uint64",
    .implementation	= Fmmux_emacs_core_ulong_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `ulong' to a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmux-core-c-ullong-to-uint64",
    .implementation	= Fmmux_emacs_core_ullong_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `ullong' to a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmux-core-c-uint8-to-uint64",
    .implementation	= Fmmux_emacs_core_uint8_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `uint8' to a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmux-core-c-uint16-to-uint64",
    .implementation	= Fmmux_emacs_core_uint16_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `uint16' to a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmux-core-c-uint32-to-uint64",
    .implementation	= Fmmux_emacs_core_uint32_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `uint32' to a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmux-core-c-usize-to-uint64",
    .implementation	= Fmmux_emacs_core_usize_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `usize' to a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmux-core-c-uintmax-to-uint64",
    .implementation	= Fmmux_emacs_core_uintmax_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `uintmax' to a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmux-core-c-wchar-to-uint64",
    .implementation	= Fmmux_emacs_core_wchar_to_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `wchar' to a user-pointer object of type `uint64'.",
  },

  /* Conversion functions to "long double". */
  {
    .name		= "mmux-core-c-float-to-long-double",
    .implementation	= Fmmux_emacs_core_float_to_long_double,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a user-pointer object of type `float' to a user-pointer object of type `long-double'.",
  },
  {
    .name		= "mmux-core-c-double-to-long-double",
    .implementation	= Fmmux_emacs_core_double_to_long_double,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a built-int of type `float' to a user-pointer object of type `long-double'.",
  },
  {
    .name		= "mmux-core-c-uint64-to-long-double",
    .implementation	= Fmmux_emacs_core_uint64_to_long_double,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a custom user-pointer object of type `uint64' to a user-pointer object of type `long-double'.",
  },
  {
    .name		= "mmux-core-c-sint64-to-long-double",
    .implementation	= Fmmux_emacs_core_sint64_to_long_double,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Convert a custom user-pointer object of type `sint64' to a user-pointer object of type `long-double'.",
  },

  /* Comparison functions. */
  {
    .name		= "mmux-core-c-sint64=",
    .implementation	= Fmmux_emacs_core_compare_sint64_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is equal to OP2.",
  },
  {
    .name		= "mmux-core-c-sint64/=",
    .implementation	= Fmmux_emacs_core_compare_sint64_not_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is not equal to OP2.",
  },
  {
    .name		= "mmux-core-c-sint64<",
    .implementation	= Fmmux_emacs_core_compare_sint64_less,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than OP2.",
  },
  {
    .name		= "mmux-core-c-sint64>",
    .implementation	= Fmmux_emacs_core_compare_sint64_greater,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than OP2.",
  },
  {
    .name		= "mmux-core-c-sint64<=",
    .implementation	= Fmmux_emacs_core_compare_sint64_less_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than or equal to OP2.",
  },
  {
    .name		= "mmux-core-c-sint64>=",
    .implementation	= Fmmux_emacs_core_compare_sint64_greater_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than or equal to OP2.",
  },
  {
    .name		= "mmux-core-c-uint64=",
    .implementation	= Fmmux_emacs_core_compare_uint64_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is equal to OP2.",
  },
  {
    .name		= "mmux-core-c-uint64/=",
    .implementation	= Fmmux_emacs_core_compare_uint64_not_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is not equal to OP2.",
  },
  {
    .name		= "mmux-core-c-uint64<",
    .implementation	= Fmmux_emacs_core_compare_uint64_less,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than OP2.",
  },
  {
    .name		= "mmux-core-c-uint64>",
    .implementation	= Fmmux_emacs_core_compare_uint64_greater,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than OP2.",
  },
  {
    .name		= "mmux-core-c-uint64<=",
    .implementation	= Fmmux_emacs_core_compare_uint64_less_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than or equal to OP2.",
  },
  {
    .name		= "mmux-core-c-uint64>=",
    .implementation	= Fmmux_emacs_core_compare_uint64_greater_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than or equal to OP2.",
  },
  {
    .name		= "mmux-core-c-long-double=",
    .implementation	= Fmmux_emacs_core_compare_long_double_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is equal to OP2.",
  },
  {
    .name		= "mmux-core-c-long-double/=",
    .implementation	= Fmmux_emacs_core_compare_long_double_not_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is not equal to OP2.",
  },
  {
    .name		= "mmux-core-c-long-double<",
    .implementation	= Fmmux_emacs_core_compare_long_double_less,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than OP2.",
  },
  {
    .name		= "mmux-core-c-long-double>",
    .implementation	= Fmmux_emacs_core_compare_long_double_greater,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than OP2.",
  },
  {
    .name		= "mmux-core-c-long-double<=",
    .implementation	= Fmmux_emacs_core_compare_long_double_less_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than or equal to OP2.",
  },
  {
    .name		= "mmux-core-c-long-double>=",
    .implementation	= Fmmux_emacs_core_compare_long_double_greater_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than or equal to OP2.",
  },

  /* Comparison operations between sint64 and uint64. */
  {
    .name		= "mmux-core-c-sint64-uint64=",
    .implementation	= Fmmux_emacs_core_compare_sint64_uint64_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is equal to OP2.",
  },
  {
    .name		= "mmux-core-c-sint64-uint64/=",
    .implementation	= Fmmux_emacs_core_compare_sint64_uint64_not_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is not equal to OP2.",
  },
  {
    .name		= "mmux-core-c-sint64-uint64<",
    .implementation	= Fmmux_emacs_core_compare_sint64_uint64_less,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than OP2.",
  },
  {
    .name		= "mmux-core-c-sint64-uint64>",
    .implementation	= Fmmux_emacs_core_compare_sint64_uint64_greater,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than OP2.",
  },
  {
    .name		= "mmux-core-c-sint64-uint64<=",
    .implementation	= Fmmux_emacs_core_compare_sint64_uint64_less_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than or equal to OP2.",
  },
  {
    .name		= "mmux-core-c-sint64-uint64>=",
    .implementation	= Fmmux_emacs_core_compare_sint64_uint64_greater_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than or equal to OP2.",
  },

  /* Comparison operations between uint64 and sint64. */
  {
    .name		= "mmux-core-c-uint64-sint64=",
    .implementation	= Fmmux_emacs_core_compare_uint64_sint64_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is equal to OP2.",
  },
  {
    .name		= "mmux-core-c-uint64-sint64/=",
    .implementation	= Fmmux_emacs_core_compare_uint64_sint64_not_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is not equal to OP2.",
  },
  {
    .name		= "mmux-core-c-uint64-sint64<",
    .implementation	= Fmmux_emacs_core_compare_uint64_sint64_less,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than OP2.",
  },
  {
    .name		= "mmux-core-c-uint64-sint64>",
    .implementation	= Fmmux_emacs_core_compare_uint64_sint64_greater,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than OP2.",
  },
  {
    .name		= "mmux-core-c-uint64-sint64<=",
    .implementation	= Fmmux_emacs_core_compare_uint64_sint64_less_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than or equal to OP2.",
  },
  {
    .name		= "mmux-core-c-uint64-sint64>=",
    .implementation	= Fmmux_emacs_core_compare_uint64_sint64_greater_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than or equal to OP2.",
  },

  /* Number range fit predicates. */
  {
    .name		= "mmux-core-c-fits-char-p",
    .implementation	= Fmmux_emacs_core_fits_char_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `char'.",
  },
  {
    .name		= "mmux-core-c-fits-schar-p",
    .implementation	= Fmmux_emacs_core_fits_schar_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `schar'.",
  },
  {
    .name		= "mmux-core-c-fits-uchar-p",
    .implementation	= Fmmux_emacs_core_fits_uchar_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uchar'.",
  },
  {
    .name		= "mmux-core-c-fits-wchar-p",
    .implementation	= Fmmux_emacs_core_fits_wchar_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `wchar'.",
  },

  {
    .name		= "mmux-core-c-fits-sshrt-p",
    .implementation	= Fmmux_emacs_core_fits_sshrt_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sshrt'.",
  },
  {
    .name		= "mmux-core-c-fits-ushrt-p",
    .implementation	= Fmmux_emacs_core_fits_ushrt_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ushrt'.",
  },
  {
    .name		= "mmux-core-c-fits-sint-p",
    .implementation	= Fmmux_emacs_core_fits_sint_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sint'.",
  },
  {
    .name		= "mmux-core-c-fits-uint-p",
    .implementation	= Fmmux_emacs_core_fits_uint_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uint'.",
  },
  {
    .name		= "mmux-core-c-fits-slong-p",
    .implementation	= Fmmux_emacs_core_fits_slong_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `slong'.",
  },
  {
    .name		= "mmux-core-c-fits-ulong-p",
    .implementation	= Fmmux_emacs_core_fits_ulong_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ulong'.",
  },
  {
    .name		= "mmux-core-c-fits-sllong-p",
    .implementation	= Fmmux_emacs_core_fits_sllong_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sllong'.",
  },
  {
    .name		= "mmux-core-c-fits-ullong-p",
    .implementation	= Fmmux_emacs_core_fits_ullong_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ullong'.",
  },

  {
    .name		= "mmux-core-c-fits-ssize-p",
    .implementation	= Fmmux_emacs_core_fits_ssize_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ssize'.",
  },
  {
    .name		= "mmux-core-c-fits-usize-p",
    .implementation	= Fmmux_emacs_core_fits_usize_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `usize'.",
  },
  {
    .name		= "mmux-core-c-fits-sintmax-p",
    .implementation	= Fmmux_emacs_core_fits_sintmax_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sintmax'.",
  },
  {
    .name		= "mmux-core-c-fits-uintmax-p",
    .implementation	= Fmmux_emacs_core_fits_uintmax_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uintmax'.",
  },
  {
    .name		= "mmux-core-c-fits-ptrdiff-p",
    .implementation	= Fmmux_emacs_core_fits_ptrdiff_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ptrdiff'.",
  },

  {
    .name		= "mmux-core-c-fits-sint8-p",
    .implementation	= Fmmux_emacs_core_fits_sint8_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sint8'.",
  },
  {
    .name		= "mmux-core-c-fits-uint8-p",
    .implementation	= Fmmux_emacs_core_fits_uint8_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uint8'.",
  },
  {
    .name		= "mmux-core-c-fits-sint16-p",
    .implementation	= Fmmux_emacs_core_fits_sint16_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sint16'.",
  },
  {
    .name		= "mmux-core-c-fits-uint16-p",
    .implementation	= Fmmux_emacs_core_fits_uint16_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uint16'.",
  },
  {
    .name		= "mmux-core-c-fits-sint32-p",
    .implementation	= Fmmux_emacs_core_fits_sint32_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sint32'.",
  },
  {
    .name		= "mmux-core-c-fits-uint32-p",
    .implementation	= Fmmux_emacs_core_fits_uint32_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uint32'.",
  },
  {
    .name		= "mmux-core-c-fits-sint64-p",
    .implementation	= Fmmux_emacs_core_fits_sint64_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmux-core-c-fits-uint64-p",
    .implementation	= Fmmux_emacs_core_fits_uint64_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uint64'.",
  },

  {
    .name		= "mmux-core-c-fits-float-p",
    .implementation	= Fmmux_emacs_core_fits_float_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `float'.",
  },
  {
    .name		= "mmux-core-c-fits-double-p",
    .implementation	= Fmmux_emacs_core_fits_double_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `double'.",
  },
  {
    .name		= "mmux-core-c-fits-long-double-p",
    .implementation	= Fmmux_emacs_core_fits_long_double_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `long-double'.",
  },
};


/** --------------------------------------------------------------------
 ** Module initialisation.
 ** ----------------------------------------------------------------- */

void
mmux_emacs_core_user_number_objects_init (emacs_env * env)
{
  mmux_emacs_define_functions_from_table(env, module_functions_table, NUMBER_OF_MODULE_FUNCTIONS, 0);
}

/* end of file */
