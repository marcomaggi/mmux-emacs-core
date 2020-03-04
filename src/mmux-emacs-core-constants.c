/*
  Part of: MMUX Emacs Core
  Contents: facilities for system constants
  Date: Feb 17, 2020

  Abstract

	This module implements facilities to access the system's constants.

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
#include <limits.h>
#include <float.h>


/** --------------------------------------------------------------------
 ** Constant values constructors.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_CONSTANT_CONSTRUCTOR
#define MMEC_DEFINE_CONSTANT_CONSTRUCTOR(TYPESTEM, FUNCSTEM, CONSTANT)	\
  static emacs_value							\
  Fmmec_c_constant_ ## FUNCSTEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[] MMEC_UNUSED, void * data MMEC_UNUSED) \
  {									\
    assert(0 == nargs);							\
    return mmec_new_emacs_value_from_clang_ ## TYPESTEM(env, CONSTANT);	\
  }

MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sint,		sint_min,	INT_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sint,		sint_max,	INT_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uint,		uint_min,	0)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uint,		uint_max,	UINT_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(slong,		slong_min,	LONG_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(slong,		slong_max,	LONG_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ulong,		ulong_min,	0UL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ulong,		ulong_max,	ULONG_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sllong,	sllong_min,	LLONG_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sllong,	sllong_max,	LLONG_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ullong,	ullong_min,	0ULL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ullong,	ullong_max,	ULLONG_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(wchar,		wchar_min,	WCHAR_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(wchar,		wchar_max,	WCHAR_MAX)

#if (MMUX_SIZEOF_SIZE_T == MMUX_SIZEOF_SIGNED_INT)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ssize,		ssize_min,	INT_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ssize,		ssize_max,	INT_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(usize,		usize_min,	0)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(usize,		usize_max,	UINT_MAX)
#elif (MMUX_SIZEOF_SIZE_T == MMUX_SIZEOF_SIGNED_LONG_INT)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ssize,		ssize_min,	LONG_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ssize,		ssize_max,	LONG_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(usize,		usize_min,	0UL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(usize,		usize_max,	ULONG_MAX)
#elif (MMUX_SIZEOF_SIZE_T == MMUX_SIZEOF_SIGNED_LONG_LONG_INT)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ssize,		ssize_min,	LLONG_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ssize,		ssize_max,	LLONG_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(usize,		usize_min,	0ULL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(usize,		usize_max,	ULLONG_MAX)
#endif

#if (MMUX_SIZEOF_INTMAX_T == MMUX_SIZEOF_SIGNED_INT)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sintmax,	sintmax_min,	INT_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sintmax,	sintmax_max,	INT_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uintmax,	uintmax_min,	0)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uintmax,	uintmax_max,	UINT_MAX)
#elif (MMUX_SIZEOF_INTMAX_T == MMUX_SIZEOF_SIGNED_LONG_INT)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sintmax,	sintmax_min,	LONG_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sintmax,	sintmax_max,	LONG_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uintmax,	uintmax_min,	0UL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uintmax,	uintmax_max,	ULONG_MAX)
#elif (MMUX_SIZEOF_INTMAX_T == MMUX_SIZEOF_SIGNED_LONG_LONG_INT)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sintmax,	sintmax_min,	LLONG_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sintmax,	sintmax_max,	LLONG_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uintmax,	uintmax_min,	0ULL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uintmax,	uintmax_max,	ULLONG_MAX)
#endif

#if (MMUX_SIZEOF_PTRDIFF_T == MMUX_SIZEOF_SIGNED_INT)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ptrdiff,	ptrdiff_min,	INT_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ptrdiff,	ptrdiff_max,	INT_MAX)
#elif (MMUX_SIZEOF_PTRDIFF_T == MMUX_SIZEOF_SIGNED_LONG_INT)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ptrdiff,	ptrdiff_min,	LONG_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ptrdiff,	ptrdiff_max,	LONG_MAX)
#elif (MMUX_SIZEOF_PTRDIFF_T == MMUX_SIZEOF_SIGNED_LONG_LONG_INT)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ptrdiff,	ptrdiff_min,	LLONG_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ptrdiff,	ptrdiff_max,	LLONG_MAX)
#endif

MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sint8,		sint8_min,	-128)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sint8,		sint8_max,	+127)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uint8,		uint8_min,	0)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uint8,		uint8_max,	255)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sint16,	sint16_min,	-32768L)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sint16,	sint16_max,	32767L)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uint16,	uint16_min,	0L)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uint16,	uint16_max,	65535L)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sint32,	sint32_min,	-2147483648L)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sint32,	sint32_max,	2147483647L)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uint32,	uint32_min,	0UL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uint32,	uint32_max,	4294967295UL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sint64,	sint64_min,	-9223372036854775808ULL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(sint64,	sint64_max,	+9223372036854775807ULL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uint64,	uint64_min,	0ULL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(uint64,	uint64_max,	+18446744073709551615ULL)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(float,		flt_min,	FLT_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(float,		flt_max,	FLT_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(double,	dbl_min,	DBL_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(double,	dbl_max,	DBL_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ldouble,	ldbl_min,	LDBL_MIN)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ldouble,	ldbl_max,	LDBL_MAX)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(float,		flt_epsilon,	FLT_EPSILON)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(double,	dbl_epsilon,	DBL_EPSILON)
MMEC_DEFINE_CONSTANT_CONSTRUCTOR(ldouble,	ldbl_epsilon,	LDBL_EPSILON)


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_FUNCTIONS	49
static mmec_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  {
    .name		= "mmec-c-constant-sint-min",
    .implementation	= Fmmec_c_constant_sint_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sint' representing the minimum value representable by a `signed int'.",
  },
  {
    .name		= "mmec-c-constant-sint-max",
    .implementation	= Fmmec_c_constant_sint_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sint' representing the maximum value representable by a `signed int'.",
  },
  {
    .name		= "mmec-c-constant-uint-min",
    .implementation	= Fmmec_c_constant_uint_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uint' representing the minimum value representable by a `unsigned int'.",
  },
  {
    .name		= "mmec-c-constant-uint-max",
    .implementation	= Fmmec_c_constant_uint_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uint' representing the maximum value representable by a `unsigned int'.",
  },

  /* ------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-slong-min",
    .implementation	= Fmmec_c_constant_slong_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `slong' representing the minimum value representable by a `signed long int'.",
  },
  {
    .name		= "mmec-c-constant-slong-max",
    .implementation	= Fmmec_c_constant_slong_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `slong' representing the maximum value representable by a `signed long int'.",
  },
  {
    .name		= "mmec-c-constant-ulong-min",
    .implementation	= Fmmec_c_constant_ulong_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `ulong' representing the minimum value representable by a `unsigned long int'.",
  },
  {
    .name		= "mmec-c-constant-ulong-max",
    .implementation	= Fmmec_c_constant_ulong_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `ulong' representing the maximum value representable by a `unsigned long int'.",
  },

  /* ------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-sllong-min",
    .implementation	= Fmmec_c_constant_sllong_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sllong' representing the minimum value representable by a `signed long long int'.",
  },
  {
    .name		= "mmec-c-constant-sllong-max",
    .implementation	= Fmmec_c_constant_sllong_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sllong' representing the maximum value representable by a `signed long long int'.",
  },
  {
    .name		= "mmec-c-constant-ullong-min",
    .implementation	= Fmmec_c_constant_ullong_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `ullong' representing the minimum value representable by a `unsigned long long int'.",
  },
  {
    .name		= "mmec-c-constant-ullong-max",
    .implementation	= Fmmec_c_constant_ullong_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `ullong' representing the maximum value representable by a `unsigned long long int'.",
  },

  /* ------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-wchar-min",
    .implementation	= Fmmec_c_constant_wchar_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `wchar' representing the minimum value representable by a `wchar_t'.",
  },
  {
    .name		= "mmec-c-constant-wchar-max",
    .implementation	= Fmmec_c_constant_wchar_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `wchar' representing the maximum value representable by a `wchar_t'.",
  },

  /* ------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-ssize-min",
    .implementation	= Fmmec_c_constant_ssize_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `ssize' representing the minimum value representable by a `ssize_t'.",
  },
  {
    .name		= "mmec-c-constant-ssize-max",
    .implementation	= Fmmec_c_constant_ssize_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `ssize' representing the maximum value representable by a `ssize_t'.",
  },
  {
    .name		= "mmec-c-constant-usize-min",
    .implementation	= Fmmec_c_constant_usize_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `usize' representing the minimum value representable by a `size_t'.",
  },
  {
    .name		= "mmec-c-constant-usize-max",
    .implementation	= Fmmec_c_constant_usize_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `usize' representing the maximum value representable by a `size_t'.",
  },

  /* ------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-sintmax-min",
    .implementation	= Fmmec_c_constant_sintmax_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sintmax' representing the minimum value representable by a `intmax_t'.",
  },
  {
    .name		= "mmec-c-constant-sintmax-max",
    .implementation	= Fmmec_c_constant_sintmax_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sintmax' representing the maximum value representable by a `intmax_t'.",
  },
  {
    .name		= "mmec-c-constant-uintmax-min",
    .implementation	= Fmmec_c_constant_uintmax_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uintmax' representing the minimum value representable by a `intmax_t'.",
  },
  {
    .name		= "mmec-c-constant-uintmax-max",
    .implementation	= Fmmec_c_constant_uintmax_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uintmax' representing the maximum value representable by a `intmax_t'.",
  },

  /* ------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-ptrdiff-min",
    .implementation	= Fmmec_c_constant_ptrdiff_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `ptrdiff' representing the minimum value representable by a `ptrdiff_t'.",
  },
  {
    .name		= "mmec-c-constant-ptrdiff-max",
    .implementation	= Fmmec_c_constant_ptrdiff_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `ptrdiff' representing the maximum value representable by a `ptrdiff_t'.",
  },

  /* ------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-sint8-min",
    .implementation	= Fmmec_c_constant_sint8_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sint8' representing the minimum value representable by a `int8_t'.",
  },
  {
    .name		= "mmec-c-constant-sint8-max",
    .implementation	= Fmmec_c_constant_sint8_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sint8' representing the maximum value representable by a `int8_t'.",
  },
  {
    .name		= "mmec-c-constant-uint8-min",
    .implementation	= Fmmec_c_constant_uint8_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uint8' representing the minimum value representable by a `uint8_t'.",
  },
  {
    .name		= "mmec-c-constant-uint8-max",
    .implementation	= Fmmec_c_constant_uint8_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uint8' representing the maximum value representable by a `uint8_t'.",
  },

  /* ------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-sint16-min",
    .implementation	= Fmmec_c_constant_sint16_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sint16' representing the minimum value representable by a `int16_t'.",
  },
  {
    .name		= "mmec-c-constant-sint16-max",
    .implementation	= Fmmec_c_constant_sint16_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sint16' representing the maximum value representable by a `int16_t'.",
  },
  {
    .name		= "mmec-c-constant-uint16-min",
    .implementation	= Fmmec_c_constant_uint16_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uint16' representing the minimum value representable by a `uint16_t'.",
  },
  {
    .name		= "mmec-c-constant-uint16-max",
    .implementation	= Fmmec_c_constant_uint16_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uint16' representing the maximum value representable by a `uint16_t'.",
  },

  /* ------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-sint32-min",
    .implementation	= Fmmec_c_constant_sint32_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sint32' representing the minimum value representable by a `int32_t'.",
  },
  {
    .name		= "mmec-c-constant-sint32-max",
    .implementation	= Fmmec_c_constant_sint32_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sint32' representing the maximum value representable by a `int32_t'.",
  },
  {
    .name		= "mmec-c-constant-uint32-min",
    .implementation	= Fmmec_c_constant_uint32_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uint32' representing the minimum value representable by a `uint32_t'.",
  },
  {
    .name		= "mmec-c-constant-uint32-max",
    .implementation	= Fmmec_c_constant_uint32_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uint32' representing the maximum value representable by a `uint32_t'.",
  },

  /* ------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-sint64-min",
    .implementation	= Fmmec_c_constant_sint64_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sint64' representing the minimum value representable by a `int64_t'.",
  },
  {
    .name		= "mmec-c-constant-sint64-max",
    .implementation	= Fmmec_c_constant_sint64_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `sint64' representing the maximum value representable by a `int64_t'.",
  },
  {
    .name		= "mmec-c-constant-uint64-min",
    .implementation	= Fmmec_c_constant_uint64_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return a user-pointer value `uint64' representing the minimum value representable by a `uint64_t'.",
  },
  {
    .name		= "mmec-c-constant-uint64-max",
    .implementation	= Fmmec_c_constant_uint64_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Value of type `mmec-uint64' representing the maximum value representable by a `uint64_t'.",
  },

  /* ------------------------------------------------------------------ */

  {
    .name		= "mmec-c-constant-flt-min",
    .implementation	= Fmmec_c_constant_flt_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Value of the `mmec-float' representing system constant `FLT_MIN'.",
  },
  {
    .name		= "mmec-c-constant-flt-max",
    .implementation	= Fmmec_c_constant_flt_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Value of type `mmec-float' representing the system constant `FLT_MAX'.",
  },
  {
    .name		= "mmec-c-constant-dbl-min",
    .implementation	= Fmmec_c_constant_dbl_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Value of type `mmec-double' representing the system constant `DBL_MIN'.",
  },
  {
    .name		= "mmec-c-constant-dbl-max",
    .implementation	= Fmmec_c_constant_dbl_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Value of type `mmec-double' representing the system constant `DBL_MAX'.",
  },
  {
    .name		= "mmec-c-constant-ldbl-min",
    .implementation	= Fmmec_c_constant_ldbl_min,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Value of type `mmec-ldouble' representing the system constant `LDBL_MIN'.",
  },
  {
    .name		= "mmec-c-constant-ldbl-max",
    .implementation	= Fmmec_c_constant_ldbl_max,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Value of type `mmec-ldouble' representing the system constant `LDBL_MAX'.",
  },
  {
    .name		= "mmec-c-constant-flt-epsilon",
    .implementation	= Fmmec_c_constant_flt_epsilon,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Value of type `mmec-float' representing the system constant `FLT_EPSILON'.",
  },
  {
    .name		= "mmec-c-constant-dbl-epsilon",
    .implementation	= Fmmec_c_constant_dbl_epsilon,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Value of type `mmec-double' representing the system constant `DBL_EPSILON'.",
  },
  {
    .name		= "mmec-c-constant-ldbl-epsilon",
    .implementation	= Fmmec_c_constant_ldbl_epsilon,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Value of type `mmec-ldouble' the system constant `LDBL_EPSILON'.",
  },
};


/** --------------------------------------------------------------------
 ** Module initialisation.
 ** ----------------------------------------------------------------- */

void
mmec_number_constants_init (emacs_env * env)
{
  mmec_define_elisp_functions_from_table(env, module_functions_table, NUMBER_OF_MODULE_FUNCTIONS, 0);
}

/* end of file */
