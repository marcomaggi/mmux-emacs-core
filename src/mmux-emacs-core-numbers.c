/*
  Part of: MMUX Emacs Core
  Contents: facilities for number objects
  Date: Feb  4, 2020

  Abstract

	This module implements facilities for number objects.

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
#include <errno.h>
#include <math.h>
#include <inttypes.h>


/** --------------------------------------------------------------------
 ** Fitting functions for C normalised language types.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_ELISP_FUNC
#define MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_ELISP_FUNC(NSTEM, BSTEM) \
  static emacs_value							\
  Fmmec_ ## NSTEM ## _fits_ ## BSTEM ## _range_p (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(1 == nargs);							\
    mmec_clang_ ## NSTEM ## _t nval;					\
    bool fits;								\
    nval = mmec_extract_clang_ ## NSTEM ## _from_emacs_value(env, args[0]); \
    fits = mmec_clang_ ## NSTEM ## _fits_ ## BSTEM ## _range_p(nval);	\
    return mmec_new_emacs_value_boolean(env, fits);			\
  }

/* We  normalise:   every  exact   signed  integer   number  to   a  value   of  type
 * "mmec_clang_sint64_t";  every exact  unsigned integer  number to  a value  of type
 * "mmec_clang_uint64_t";   every  floating-point   number   to  a   value  of   type
 * "mmec_clang_ldouble_t".
 *
 * In  reverse: we  need functions  to  check if  a  normalised type  fits the  range
 * representable by a basic type.
 */

/* Define a  range Fit checker function  for normalised signed values.   The argument
   NSTEM is the stem of the normalised type  name.  The argument BSTEM is the stem of
   the base  type name.  The  argument BCAPSTEM is the  capitalised stem of  the base
   type name. */
#undef  MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC
#define MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(NSTEM, BSTEM, BCAPSTEM) \
  bool									\
  mmec_clang_ ## NSTEM ## _fits_ ## BSTEM ## _range_p (mmec_clang_ ## NSTEM ## _t nval)	\
  {									\
    if (0) {								\
      fprintf(stderr, "%s: nval=%lld, min=%lld, max=%lld\n", __func__,	\
	      (long long)nval,						\
	      (long long)MMEC_ ## BCAPSTEM ## _MIN,			\
	      (long long)MMEC_ ## BCAPSTEM ## _MAX);			\
    }									\
    return (((MMEC_ ## BCAPSTEM ## _MIN <= nval) && (nval <= MMEC_ ## BCAPSTEM ## _MAX))? true : false); \
  }									\
  MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_ELISP_FUNC(NSTEM, BSTEM)

/* Define a range fit checker function  for normalised unsigned values.  The argument
   NSTEM is the stem of the normalised type  name.  The argument BSTEM is the stem of
   the base  type name.  The  argument BCAPSTEM is the  capitalised stem of  the base
   type name. */
#undef  MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC
#define MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(NSTEM, BSTEM, BCAPSTEM) \
  bool									\
  mmec_clang_ ## NSTEM ## _fits_ ## BSTEM ## _range_p (mmec_clang_ ## NSTEM ## _t nval)	\
  {									\
    return ((nval <= MMEC_ ## BCAPSTEM ## _MAX)? true : false);		\
  }									\
  MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_ELISP_FUNC(NSTEM, BSTEM)

MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	char,		CHAR)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	schar,		SCHAR)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	uchar,		UCHAR)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	wchar,		WCHAR)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	sshrt,		SSHRT)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	ushrt,		USHRT)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	sint,		SINT)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	uint,		UINT)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	slong,		SLONG)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	ulong,		ULONG)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	sllong,		SLLONG)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	ullong,		ULLONG)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	sintmax,	SINTMAX)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	uintmax,	UINTMAX)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	ssize,		SSIZE)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	usize,		USIZE)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	ptrdiff,	PTRDIFF)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	sint8,		SINT8)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	uint8,		UINT8)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	sint16,		SINT16)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	uint16,		UINT16)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	sint32,		SINT32)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	uint32,		UINT32)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(sint64,	sint64,		SINT64)
MMEC_DEFINE_UNSIGNED_NORMALISED_TYPE_FITS_FUNC(uint64,	uint64,		UINT64)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(ldouble,	float,		FLOAT)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(ldouble,	double,		FLOAT)
MMEC_DEFINE_SIGNED_NORMALISED_TYPE_FITS_FUNC(ldouble,	ldouble,	LDOUBLE)


/** --------------------------------------------------------------------
 ** Custom numbers with built-in "integer" internal representation.
 ** ----------------------------------------------------------------- */

/* Define  the  C language  implementation  function  of  an Emacs  Lisp  constructor
 * function for a base C language type; the  C language type has an Emacs Lisp object
 * of  type  "integer"  as  internal   representation.   The  constructor  accepts  a
 * normalised Emacs Lisp value as argument.
 *
 * The argument NSTEM is the stem of the normalised type name.
 *
 * The argument BSTEM is the stem of the base type name.
 */
#undef  MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR
#define MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR(BSTEM, NSTEM)		\
  emacs_value								\
  Fmmec_make_elisp_integer_ ## BSTEM ## _from_usrptr_ ## NSTEM		\
  (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(nargs == 1);							\
    mmec_clang_ ## NSTEM ## _t nval =					\
      mmec_extract_clang_ ## NSTEM ## _from_emacs_value(env, args[0]);	\
									\
    if (mmec_clang_ ## NSTEM ## _fits_ ## BSTEM ## _range_p(nval)) {	\
      MMEC_CAST(mmec_clang_ ## BSTEM ## _t, val, nval);			\
      return mmec_new_emacs_value_from_clang_ ## BSTEM(env, val);	\
    } else {								\
      return mmec_error_value_out_of_range(env);			\
    }									\
  }

MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR(char,	sint64)
MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR(schar,	sint64)
MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR(uchar,	uint64)
MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR(sshrt,	sint64)
MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR(ushrt,	uint64)
MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR(sint8,	sint64)
MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR(uint8,	uint64)
MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR(sint16,	sint64)
MMEC_DEFINE_INTEGER_NUMBER_CONSTRUCTOR(uint16,	uint64)


/** --------------------------------------------------------------------
 ** Custom numbers with built-in "float" internal representation.
 ** ----------------------------------------------------------------- */

/* Define  the  C language  implementation  function  of  an Emacs  Lisp  constructor
 * function for a base C language type; the  C language type has an Emacs Lisp object
 * of type "float" as internal  representation.  The constructor accepts a normalised
 * Emacs Lisp value as argument.
 *
 * The argument NSTEM is the stem of the normalised type name.
 *
 * The argument BSTEM is the stem of the base type name.
 */
#undef  MMEC_DEFINE_FLOAT_NUMBER_CONSTRUCTOR
#define MMEC_DEFINE_FLOAT_NUMBER_CONSTRUCTOR(BSTEM, NSTEM)		\
  emacs_value								\
  Fmmec_make_elisp_float_ ## BSTEM ## _from_usrptr_ ## NSTEM		\
  (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(nargs == 1);							\
    mmec_clang_ ## NSTEM ## _t nval =					\
      mmec_extract_clang_ ## NSTEM ## _from_emacs_value(env, args[0]);	\
									\
    if (mmec_clang_ ## NSTEM ## _fits_ ## BSTEM ## _range_p(nval)) {	\
      MMEC_CAST(mmec_clang_ ## BSTEM ## _t, val, nval);			\
      return mmec_new_emacs_value_from_clang_ ## BSTEM(env, val);	\
    } else {								\
      return mmec_error_value_out_of_range(env);			\
    }									\
  }

MMEC_DEFINE_FLOAT_NUMBER_CONSTRUCTOR(double,	ldouble)


/** --------------------------------------------------------------------
 ** Custom numbers with user-pointer object internal representation.
 ** ----------------------------------------------------------------- */

/* At the  Emacs Lisp  level, these  types are constructed  from a  normalised number
 * representation:
 *
 * - The initialisation  value of signed  integers is  a user-pointer object  of type
 *   "sint64".
 *
 * - The initialisation value  of unsigned integers is a user-pointer  object of type
 *   "uint64".
 *
 * - The initialisation value  of floating-point numbers is a  user-pointer object of
 *   type "ldouble".
 */

#undef  MMEC_DEFINE_USRPTR_BASE_TYPE_FUNCTIONS
#define MMEC_DEFINE_USRPTR_BASE_TYPE_FUNCTIONS(BSTEM)			\
  mmec_intrep_ ## BSTEM ## _t						\
  mmec_new_intrep_ ## BSTEM ## _from_clang_ ## BSTEM (mmec_clang_ ## BSTEM ## _t val) \
  {									\
    mmec_intrep_ ## BSTEM ##_t irep;					\
									\
    errno = 0;								\
    irep   = (mmec_intrep_ ## BSTEM ##_t)malloc(sizeof(mmec_intrep_ ## BSTEM ## _stru_t)); \
    if (irep) {								\
      irep->val	= val;							\
      return irep;							\
    } else {								\
      return NULL;							\
    }									\
  }									\
									\
  emacs_value								\
  mmec_new_emacs_value_from_intrep_ ## BSTEM (emacs_env * env, mmec_intrep_ ## BSTEM ## _t irep) \
  {									\
    return mmec_new_emacs_value_from_usrptr_object(env, mmec_intrep_ ## BSTEM ## _finalizer, irep); \
  }									\
									\
  emacs_value								\
  mmec_new_emacs_value_from_clang_ ## BSTEM (emacs_env * env, mmec_clang_ ## BSTEM ## _t val) \
  {									\
    mmec_intrep_ ## BSTEM ## _t	irep =					\
      mmec_new_intrep_ ## BSTEM ## _from_clang_ ## BSTEM(val);		\
    if (irep) {								\
      return mmec_new_emacs_value_from_intrep_ ## BSTEM(env, irep);	\
    } else {								\
      return mmec_error_memory_allocation(env);				\
    }									\
  }									\
									\
  mmec_intrep_ ## BSTEM ## _t						\
  mmec_get_intrep_ ## BSTEM ## _from_emacs_value (emacs_env * env, emacs_value arg) \
  {									\
    return ((mmec_intrep_ ## BSTEM ## _t)mmec_get_usrptr_object_from_emacs_value(env, arg)); \
  }									\
									\
  mmec_clang_ ## BSTEM ## _t						\
  mmec_extract_clang_ ## BSTEM ## _from_intrep_ ## BSTEM (mmec_intrep_ ## BSTEM ## _t irep) \
  {									\
    return irep->val;							\
  }									\
									\
  mmec_clang_ ## BSTEM ## _t						\
  mmec_extract_clang_ ## BSTEM ## _from_emacs_value (emacs_env * env, emacs_value arg) \
  {									\
    mmec_intrep_ ## BSTEM ## _t	irep;					\
									\
    irep = mmec_get_intrep_ ## BSTEM ## _from_emacs_value(env, arg);	\
    return mmec_extract_clang_ ## BSTEM ## _from_intrep_ ## BSTEM(irep); \
  }

/* Define a C language function acting  as user-pointer object finalizer.  The object
 * being finalized is the internal representation of a custom number.
 *
 * The argument BSTEM is the stem of the base type name.
 */
#undef  MMEC_DEFINE_USRPTR_FINALIZER_FUNCTION
#define MMEC_DEFINE_USRPTR_FINALIZER_FUNCTION(BSTEM)	\
  static void						\
  mmec_intrep_ ## BSTEM ## _finalizer (void * _obj)	\
  {							\
    MMEC_PC(mmec_intrep_ ## BSTEM ##_t, obj, _obj);	\
    free(obj);						\
  }

/* Define  the  C language  implementation  function  of  an Emacs  Lisp  constructor
 * function  for a  base C  language type;  the  C language  type has  an Emacs  Lisp
 * user-pointer  object  as  internal  representation.   The  constructor  accepts  a
 * normalised Emacs Lisp value as argument.
 *
 * The argument NSTEM is the stem of the normalised type name.
 *
 * The argument BSTEM is the stem of the base type name.
 */
#undef  MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR
#define MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(BSTEM, NSTEM)	\
  emacs_value								\
  Fmmec_make_usrptr_ ## BSTEM ## _from_usrptr_ ## NSTEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(nargs == 1);							\
    mmec_clang_ ## NSTEM ## _t nval =					\
      mmec_extract_clang_ ## NSTEM ## _from_emacs_value(env, args[0]);	\
									\
    if (mmec_clang_ ## NSTEM ## _fits_ ## BSTEM ## _range_p(nval)) {	\
      MMEC_CAST(mmec_clang_ ## BSTEM ## _t, val, nval);			\
      return mmec_new_emacs_value_from_clang_ ## BSTEM(env, val);	\
    } else {								\
      return mmec_error_value_out_of_range(env);			\
    }									\
  }

#define MMEC_DEFINE_USRPTR_BASE_TYPE(BSTEM, NSTEM)		\
  MMEC_DEFINE_USRPTR_FINALIZER_FUNCTION(BSTEM)			\
  MMEC_DEFINE_USRPTR_BASE_TYPE_FUNCTIONS(BSTEM)			\
  MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(BSTEM, NSTEM)

MMEC_DEFINE_USRPTR_BASE_TYPE(sint,	sint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(uint,	uint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(slong,	sint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(ulong,	uint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(sllong,	sint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(ullong,	uint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(sintmax,	sint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(uintmax,	uint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(ssize,	sint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(usize,	uint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(wchar,	sint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(ptrdiff,	sint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(sint32,	sint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(uint32,	uint64)
MMEC_DEFINE_USRPTR_BASE_TYPE(float,	ldouble)


/** --------------------------------------------------------------------
 ** Custom numbers with user-pointer object internal representation.
 ** ----------------------------------------------------------------- */

/* The following definitions are for normalised types. */

MMEC_DEFINE_USRPTR_FINALIZER_FUNCTION(sint64)
MMEC_DEFINE_USRPTR_BASE_TYPE_FUNCTIONS(sint64)

MMEC_DEFINE_USRPTR_FINALIZER_FUNCTION(uint64)
MMEC_DEFINE_USRPTR_BASE_TYPE_FUNCTIONS(uint64)

MMEC_DEFINE_USRPTR_FINALIZER_FUNCTION(ldouble)
MMEC_DEFINE_USRPTR_BASE_TYPE_FUNCTIONS(ldouble)

/* Define  the  C language  implementation  function  of  an Emacs  Lisp  constructor
 * function  for a  base C  language type;  the  C language  type has  an Emacs  Lisp
 * user-pointer  object  as  internal  representation.   The  constructor  accepts  a
 * normalised Emacs Lisp value as argument.
 *
 * The argument NSTEM is the stem of the normalised type name.
 *
 * The argument BSTEM is the stem of the base type name.
 */
#undef  MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR
#define MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(BSTEM, ARGSTEM, ARGINTREP) \
  static emacs_value								\
  Fmmec_make_usrptr_ ## BSTEM ## _from_ ## ARGINTREP ## _ ## ARGSTEM	\
  (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(nargs == 1);							\
    mmec_clang_ ## ARGSTEM ## _t nval =					\
      mmec_extract_clang_ ## ARGSTEM ## _from_emacs_value(env, args[0]); \
    MMEC_CAST(mmec_clang_ ## BSTEM ## _t, val, nval);			\
    return mmec_new_emacs_value_from_clang_ ## BSTEM(env, val);		\
  }

MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		char,		integer)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		schar,		integer)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		uchar,		integer)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		wchar,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		sshrt,		integer)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		ushrt,		integer)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		sint,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		uint,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		slong,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		ulong,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		sllong,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		ullong,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		sintmax,	usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		uintmax,	usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		ssize,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		usize,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		ptrdiff,	usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		sint8,		integer)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		uint8,		integer)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		sint16,		integer)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		uint16,		integer)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		sint32,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		uint32,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(sint64,		sint64,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(uint64,		uint64,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(ldouble,		float,		usrptr)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(ldouble,		double,		float)
MMEC_DEFINE_USRPTR_BASE_TYPE_ELISP_CONSTRUCTOR(ldouble,		ldouble,	usrptr)

/* ------------------------------------------------------------------ */

static emacs_value
Fmmec_make_usrptr_sint64_from_elisp_integer (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert(nargs == 1);
  mmec_clang_sintmax_t argval = mmec_extract_elisp_integer_from_emacs_value(env, args[0]);
  MMEC_CAST(mmec_clang_sint64_t, val, argval);
  return mmec_new_emacs_value_from_clang_sint64(env, val);
}

static emacs_value
Fmmec_make_usrptr_sint64_from_elisp_float (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert(nargs == 1);
  mmec_clang_double_t	argval = mmec_extract_elisp_float_from_emacs_value(env, args[0]);
  MMEC_CAST(mmec_clang_sint64_t, val, round(argval));
  return mmec_new_emacs_value_from_clang_sint64(env, val);
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmec_make_usrptr_uint64_from_elisp_integer (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert(nargs == 1);
  mmec_clang_sintmax_t argval = mmec_extract_elisp_integer_from_emacs_value(env, args[0]);
  MMEC_CAST(mmec_clang_uint64_t, val, argval);
  return mmec_new_emacs_value_from_clang_uint64(env, val);
}

static emacs_value
Fmmec_make_usrptr_uint64_from_elisp_float (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert(nargs == 1);
  mmec_clang_double_t	argval = mmec_extract_elisp_float_from_emacs_value(env, args[0]);
  MMEC_CAST(mmec_clang_uint64_t, val, round(argval));
  return mmec_new_emacs_value_from_clang_uint64(env, val);
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmec_make_usrptr_ldouble_from_usrptr_sint64 (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert(nargs == 1);
  mmec_clang_sint64_t argval = mmec_extract_clang_sint64_from_emacs_value(env, args[0]);
  MMEC_CAST(mmec_clang_ldouble_t, val, argval);
  return mmec_new_emacs_value_from_clang_ldouble(env, val);
}

static emacs_value
Fmmec_make_usrptr_ldouble_from_usrptr_uint64 (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert(nargs == 1);
  mmec_clang_uint64_t argval = mmec_extract_clang_uint64_from_emacs_value(env, args[0]);
  MMEC_CAST(mmec_clang_ldouble_t, val, argval);
  return mmec_new_emacs_value_from_clang_ldouble(env, val);
}

static emacs_value
Fmmec_make_usrptr_ldouble_from_elisp_float (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert(nargs == 1);
  mmec_clang_double_t argval = mmec_extract_elisp_float_from_emacs_value(env, args[0]);
  MMEC_CAST(mmec_clang_ldouble_t, val, argval);
  return mmec_new_emacs_value_from_clang_ldouble(env, val);
}

static emacs_value
Fmmec_make_usrptr_ldouble_from_elisp_integer (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert(nargs == 1);
  mmec_clang_double_t argval = mmec_extract_elisp_integer_from_emacs_value(env, args[0]);
  MMEC_CAST(mmec_clang_ldouble_t, val, argval);
  return mmec_new_emacs_value_from_clang_ldouble(env, val);
}


/** --------------------------------------------------------------------
 ** Comparison functions.
 ** ----------------------------------------------------------------- */

#undef  MMEC_COMPARISON_OPERATION
#define MMEC_COMPARISON_OPERATION(OPNAME, OPERATOR, STEM)		\
  static emacs_value							\
  Fmmec_compare_ ## STEM ## _ ## OPNAME(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(2 == nargs);							\
    mmec_clang_ ## STEM ## _t op1 = mmec_extract_clang_ ## STEM ## _from_emacs_value(env, args[0]); \
    mmec_clang_ ## STEM ## _t op2 = mmec_extract_clang_ ## STEM ## _from_emacs_value(env, args[1]); \
    int		rv	= ((op1 OPERATOR op2)? 1 : 0);			\
									\
    if (0) {								\
      fprintf(stderr, "%s: op1=%ld op2=%ld rv=%d\n", __func__,		\
	      (long)op1, (long)op2, rv);				\
    }									\
    return mmec_new_emacs_value_boolean(env, rv);			\
  }

#define MMEC_COMPARISON_OPERATIONS(STEM)		\
  MMEC_COMPARISON_OPERATION(equal,		==,	STEM) \
  MMEC_COMPARISON_OPERATION(less,		<,	STEM) \
  MMEC_COMPARISON_OPERATION(greater,		>,	STEM) \
  MMEC_COMPARISON_OPERATION(less_equal,		<=,	STEM) \
  MMEC_COMPARISON_OPERATION(greater_equal,	>=,	STEM) \
  MMEC_COMPARISON_OPERATION(not_equal,		!=,	STEM)

MMEC_COMPARISON_OPERATIONS(uint64)
MMEC_COMPARISON_OPERATIONS(sint64)
MMEC_COMPARISON_OPERATIONS(ldouble)

/* ------------------------------------------------------------------ */

/* FIXME I do not know how to compare a signed integer and an unsigne integer without
   loosing bits; maybe, in future, GNU Emacs will support big integers with GMP.  For
   now it is an error.  (Marco Maggi; Feb 7, 2020)*/
#undef  MMEC_COMPARISON_OPERATION2
#define MMEC_COMPARISON_OPERATION2(OPNAME, OPERATOR, STEM1, STEM2) \
  static emacs_value							\
  Fmmec_compare_ ## STEM1 ## _ ## STEM2 ## _ ## OPNAME(emacs_env *env, ptrdiff_t nargs, \
						       emacs_value args[] MMEC_UNUSED, \
						       void * data MMEC_UNUSED) \
  {									\
    assert(2 == nargs);							\
    return mmec_error_signed_unsigned_integer_comparison(env);		\
  }

#define MMEC_COMPARISON_OPERATIONS2(STEM1, STEM2)			\
  MMEC_COMPARISON_OPERATION2(equal,		==,	STEM1,	STEM2)	\
  MMEC_COMPARISON_OPERATION2(less,		<,	STEM1,	STEM2)	\
  MMEC_COMPARISON_OPERATION2(greater,		>,	STEM1,	STEM2)	\
  MMEC_COMPARISON_OPERATION2(less_equal,	<=,	STEM1,	STEM2)	\
  MMEC_COMPARISON_OPERATION2(greater_equal,	>=,	STEM1,	STEM2)	\
  MMEC_COMPARISON_OPERATION2(not_equal,		!=,	STEM1,	STEM2)

MMEC_COMPARISON_OPERATIONS2(sint64, uint64)
MMEC_COMPARISON_OPERATIONS2(uint64, sint64)


/** --------------------------------------------------------------------
 ** Printing functions.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_PRINT_FUNCTION
#define MMEC_DEFINE_PRINT_FUNCTION(STEM, TEMPLATE, CASTER)		\
  static emacs_value								\
  Fmmec_c_ ## STEM ## _print_to_string (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(1 == nargs);							\
    mmec_clang_ ## STEM ## _t	val = mmec_extract_clang_ ## STEM ## _from_emacs_value(env, args[0]); \
    int				required_len_minus_termzero = 0;	\
									\
    /* First attempt at writing the output. */				\
    {									\
      char	buffer[64];						\
									\
      required_len_minus_termzero = snprintf(buffer, 64, TEMPLATE, CASTER val);	\
      if (64 > required_len_minus_termzero) {				\
	if (0) { fprintf(stderr, "%s: %s\n", __func__, buffer); }	\
	return mmec_new_emacs_value_string(env, buffer, required_len_minus_termzero); \
      }									\
    }									\
									\
    /* Second attempt at writing the output.  We assume that this will succeed.  */ \
    {									\
      char	buffer[required_len_minus_termzero];			\
      int	this_len;						\
									\
      this_len = snprintf(buffer, required_len_minus_termzero, TEMPLATE, CASTER val); \
      assert(this_len <= required_len_minus_termzero);			\
      return mmec_new_emacs_value_string(env, buffer, this_len);	\
    }									\
  }

MMEC_DEFINE_PRINT_FUNCTION(wchar,	"%lu"		, (signed long int))
MMEC_DEFINE_PRINT_FUNCTION(sint,	"%d"		,)
MMEC_DEFINE_PRINT_FUNCTION(uint,	"%u"		,)
MMEC_DEFINE_PRINT_FUNCTION(slong,	"%ld"		,)
MMEC_DEFINE_PRINT_FUNCTION(ulong,	"%lu"		,)
MMEC_DEFINE_PRINT_FUNCTION(sllong,	"%lld"		,)
MMEC_DEFINE_PRINT_FUNCTION(ullong,	"%llu"		,)
MMEC_DEFINE_PRINT_FUNCTION(sintmax,	"%" PRIdMAX	,)
MMEC_DEFINE_PRINT_FUNCTION(uintmax,	"%" PRIuMAX	,)
MMEC_DEFINE_PRINT_FUNCTION(ssize,	"%lld"		, (signed long long int))
MMEC_DEFINE_PRINT_FUNCTION(usize,	"%llu"		, (unsigned long long int))
MMEC_DEFINE_PRINT_FUNCTION(ptrdiff,	"%lld"		, (signed long long int))
MMEC_DEFINE_PRINT_FUNCTION(sint32,	"%" PRId32	,)
MMEC_DEFINE_PRINT_FUNCTION(uint32,	"%" PRIu32	,)
MMEC_DEFINE_PRINT_FUNCTION(sint64,	"%" PRId64	,)
MMEC_DEFINE_PRINT_FUNCTION(uint64,	"%" PRIu64	,)
MMEC_DEFINE_PRINT_FUNCTION(float,	"%g"		, (double))
MMEC_DEFINE_PRINT_FUNCTION(ldouble,	"%Lg"		,)


/** --------------------------------------------------------------------
 ** Sign inspection functions.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_SIGN_FUNCTION_TEMPLATE
#define MMEC_DEFINE_SIGN_FUNCTION_TEMPLATE(FUNCSTEM, TYPESTEM, ZERO, OP) \
  static emacs_value							\
  Fmmec_c_ ## TYPESTEM ## _ ## FUNCSTEM ##_p				\
  (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(1 == nargs);							\
    mmec_clang_ ## TYPESTEM ## _t val = mmec_extract_clang_ ## TYPESTEM ## _from_emacs_value(env, args[0]); \
    return mmec_new_emacs_value_boolean(env, (ZERO OP val)? true : false); \
  }

#undef  MMEC_DEFINE_SIGN_FUNCTION_TRUE
#define MMEC_DEFINE_SIGN_FUNCTION_TRUE(FUNCSTEM, TYPESTEM)		\
  static emacs_value							\
  Fmmec_c_ ## TYPESTEM ## _ ## FUNCSTEM ##_p				\
  (emacs_env *env, ptrdiff_t nargs, emacs_value args[] MMEC_UNUSED, void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(1 == nargs);							\
    return mmec_new_emacs_value_true(env);				\
  }

#undef  MMEC_DEFINE_SIGN_FUNCTION_FALSE
#define MMEC_DEFINE_SIGN_FUNCTION_FALSE(FUNCSTEM, TYPESTEM)		\
  static emacs_value							\
  Fmmec_c_ ## TYPESTEM ## _ ## FUNCSTEM ##_p				\
  (emacs_env *env, ptrdiff_t nargs, emacs_value args[] MMEC_UNUSED, void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(1 == nargs);							\
    return mmec_new_emacs_value_nil(env);				\
  }

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_SIGN_FUNCTIONS
#define MMEC_DEFINE_SIGN_FUNCTIONS(TYPESTEM, ZERO)			\
  MMEC_DEFINE_SIGN_FUNCTION_TEMPLATE(zero,		TYPESTEM, ZERO, ==) \
    MMEC_DEFINE_SIGN_FUNCTION_TEMPLATE(positive,	TYPESTEM, ZERO,  <) \
    MMEC_DEFINE_SIGN_FUNCTION_TEMPLATE(negative,	TYPESTEM, ZERO,  >) \
    MMEC_DEFINE_SIGN_FUNCTION_TEMPLATE(non_positive,	TYPESTEM, ZERO, >=) \
    MMEC_DEFINE_SIGN_FUNCTION_TEMPLATE(non_negative,	TYPESTEM, ZERO, <=)

MMEC_DEFINE_SIGN_FUNCTIONS(char,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(schar,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(sshrt,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(sint,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(slong,	0L)
MMEC_DEFINE_SIGN_FUNCTIONS(sllong,	0LL)
MMEC_DEFINE_SIGN_FUNCTIONS(sintmax,	0L)
MMEC_DEFINE_SIGN_FUNCTIONS(ssize,	0L)
MMEC_DEFINE_SIGN_FUNCTIONS(wchar,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(ptrdiff,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(sint8,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(sint16,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(sint32,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(sint64,	0LL)
MMEC_DEFINE_SIGN_FUNCTIONS(float,	0.0F)
MMEC_DEFINE_SIGN_FUNCTIONS(double,	0.0)
MMEC_DEFINE_SIGN_FUNCTIONS(ldouble,	0.0L)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_SIGN_FUNCTIONS
#define MMEC_DEFINE_SIGN_FUNCTIONS(TYPESTEM, ZERO)			\
  MMEC_DEFINE_SIGN_FUNCTION_TEMPLATE(zero,		TYPESTEM, ZERO, ==) \
    MMEC_DEFINE_SIGN_FUNCTION_TRUE(positive,		TYPESTEM)	\
    MMEC_DEFINE_SIGN_FUNCTION_FALSE(negative,		TYPESTEM)	\
    MMEC_DEFINE_SIGN_FUNCTION_TEMPLATE(non_positive,	TYPESTEM, ZERO, ==) \
    MMEC_DEFINE_SIGN_FUNCTION_TRUE(non_negative,	TYPESTEM)

MMEC_DEFINE_SIGN_FUNCTIONS(uchar,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(ushrt,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(uint,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(ulong,	0UL)
MMEC_DEFINE_SIGN_FUNCTIONS(ullong,	0ULL)
MMEC_DEFINE_SIGN_FUNCTIONS(uintmax,	0L)
MMEC_DEFINE_SIGN_FUNCTIONS(usize,	0UL)
MMEC_DEFINE_SIGN_FUNCTIONS(uint8,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(uint16,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(uint32,	0)
MMEC_DEFINE_SIGN_FUNCTIONS(uint64,	0ULL)


/** --------------------------------------------------------------------
 ** Elisp functions table: constructors, signed integers, integer intrep.
 ** ----------------------------------------------------------------- */

/* Constructors  for  custom  number  objects  representing  signed  integers,  whose
 * internal representation is an Emacs built-in "integer" value.
 */
#define MODULE_FUNCTIONS__CONSTRUCTORS__SIGNED_INTEGERS__INTREP_INTEGER		5
static mmec_module_function_t const
  module_functions__constructors__signed_integers__intrep_integer[MODULE_FUNCTIONS__CONSTRUCTORS__SIGNED_INTEGERS__INTREP_INTEGER] = {
  {
    .name		= "mmec-c-make-elisp-integer-char-from-usrptr-sint64",
    .implementation	= Fmmec_make_elisp_integer_char_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return an Emacs built-in `integer' value being the internal representation of a `mmec-char' value.",
  },
  {
    .name		= "mmec-c-make-elisp-integer-schar-from-usrptr-sint64",
    .implementation	= Fmmec_make_elisp_integer_schar_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return an Emacs built-in `integer' value being the internal representation of a `mmec-schar' value.",
  },
  {
    .name		= "mmec-c-make-elisp-integer-sshrt-from-usrptr-sint64",
    .implementation	= Fmmec_make_elisp_integer_sshrt_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return an Emacs built-in `integer' value being the internal representation of a `mmec-sshrt' value.",
  },
  {
    .name		= "mmec-c-make-elisp-integer-sint8-from-usrptr-sint64",
    .implementation	= Fmmec_make_elisp_integer_sint8_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return an Emacs built-in `integer' value being the internal representation of a `mmec-sint8' value.",
  },
  {
    .name		= "mmec-c-make-elisp-integer-sint16-from-usrptr-sint64",
    .implementation	= Fmmec_make_elisp_integer_sint16_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return an Emacs built-in `integer' value being the internal representation of a `mmec-sint16' value.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: constructors, unsigned integers, integer intrep.
 ** ----------------------------------------------------------------- */

/* Constructors  for  custom number  objects  representing  unsigned integers,  whose
 * internal representation is an Emacs built-in "integer" value.
 */
#define MODULE_FUNCTIONS__CONSTRUCTORS__UNSIGNED_INTEGERS__INTREP_INTEGER	4
static mmec_module_function_t const
  module_functions__constructors__unsigned_integers__intrep_integer[MODULE_FUNCTIONS__CONSTRUCTORS__UNSIGNED_INTEGERS__INTREP_INTEGER] = {
  {
    .name		= "mmec-c-make-elisp-integer-uchar-from-usrptr-uint64",
    .implementation	= Fmmec_make_elisp_integer_uchar_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return an Emacs built-in `integer' value being the internal representation of a `mmec-uchar' value.",
  },
  {
    .name		= "mmec-c-make-elisp-integer-ushrt-from-usrptr-uint64",
    .implementation	= Fmmec_make_elisp_integer_ushrt_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return an Emacs built-in `integer' value being the internal representation of a `mmec-ushrt' value.",
  },
  {
    .name		= "mmec-c-make-elisp-integer-uint8-from-usrptr-uint64",
    .implementation	= Fmmec_make_elisp_integer_uint8_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return an Emacs built-in `integer' value being the internal representation of a `mmec-uint8' value.",
  },
  {
    .name		= "mmec-c-make-elisp-integer-uint16-from-usrptr-uint64",
    .implementation	= Fmmec_make_elisp_integer_uint16_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return an Emacs built-in `integer' value being the internal representation of a `mmec-uint16' value.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: constructors, floating-point numbers, float intrep.
 ** ----------------------------------------------------------------- */

/* Constructors for  custom number objects representing  floating-point values, whose
 * internal representation is an Emacs built-in "float" value.
 */
#define MODULE_FUNCTIONS__CONSTRUCTORS__FLOATING_POINT_NUMBERS__INTREP_FLOAT	1
static mmec_module_function_t const
  module_functions__constructors__floating_point_numbers__intrep_float[MODULE_FUNCTIONS__CONSTRUCTORS__FLOATING_POINT_NUMBERS__INTREP_FLOAT] = {
  {
    .name		= "mmec-c-make-elisp-float-double-from-usrptr-ldouble",
    .implementation	= Fmmec_make_elisp_float_double_from_usrptr_ldouble,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return an Emacs built-in `float' value being the internal representation of a `mmec-double' value.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: constructors, signed integers, usrptr intrep.
 ** ----------------------------------------------------------------- */

/* Constructors  for  custom  number  objects  representing  signed  integers,  whose
 * internal representation is a user-pointer object.
 */
#define MODULE_FUNCTIONS__CONSTRUCTORS__SIGNED_INTEGERS__INTREP_USRPTR		8
static mmec_module_function_t const
  module_functions__constructors__signed_integers__intrep_usrptr[MODULE_FUNCTIONS__CONSTRUCTORS__SIGNED_INTEGERS__INTREP_USRPTR] = {
  {
    .name		= "mmec-c-make-usrptr-sint-from-usrptr-sint64",
    .implementation	= Fmmec_make_usrptr_sint_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint'.",
  },
  {
    .name		= "mmec-c-make-usrptr-slong-from-usrptr-sint64",
    .implementation	= Fmmec_make_usrptr_slong_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `slong'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sllong-from-usrptr-sint64",
    .implementation	= Fmmec_make_usrptr_sllong_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sllong'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint32-from-usrptr-sint64",
    .implementation	= Fmmec_make_usrptr_sint32_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint32'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sintmax-from-usrptr-sint64",
    .implementation	= Fmmec_make_usrptr_sintmax_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sintmax'.",
  },
  {
    .name		= "mmec-c-make-usrptr-ssize-from-usrptr-sint64",
    .implementation	= Fmmec_make_usrptr_ssize_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ssize'.",
  },
  {
    .name		= "mmec-c-make-usrptr-wchar-from-usrptr-sint64",
    .implementation	= Fmmec_make_usrptr_wchar_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `wchar'.",
  },
  {
    .name		= "mmec-c-make-usrptr-ptrdiff-from-usrptr-sint64",
    .implementation	= Fmmec_make_usrptr_ptrdiff_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ptrdiff'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: constructors, unsigned integers, usrptr intrep.
 ** ----------------------------------------------------------------- */

/* Constructors  for  custom number  objects  representing  unsigned integers,  whose
 * internal representation is a user-pointer object.
 */
#define MODULE_FUNCTIONS__CONSTRUCTORS__UNSIGNED_INTEGERS__INTREP_USRPTR	6
static mmec_module_function_t const
  module_functions__constructors__unsigned_integers__intrep_usrptr[MODULE_FUNCTIONS__CONSTRUCTORS__UNSIGNED_INTEGERS__INTREP_USRPTR] = {
  {
    .name		= "mmec-c-make-usrptr-uint-from-usrptr-uint64",
    .implementation	= Fmmec_make_usrptr_uint_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint'.",
  },
  {
    .name		= "mmec-c-make-usrptr-ulong-from-usrptr-uint64",
    .implementation	= Fmmec_make_usrptr_ulong_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ulong'.",
  },
  {
    .name		= "mmec-c-make-usrptr-ullong-from-usrptr-uint64",
    .implementation	= Fmmec_make_usrptr_ullong_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ullong'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint32-from-usrptr-uint64",
    .implementation	= Fmmec_make_usrptr_uint32_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint32'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uintmax-from-usrptr-uint64",
    .implementation	= Fmmec_make_usrptr_uintmax_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uintmax'.",
  },
  {
    .name		= "mmec-c-make-usrptr-usize-from-usrptr-uint64",
    .implementation	= Fmmec_make_usrptr_usize_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `usize'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: constructors, floating-point numbers, usrptr intrep.
 ** ----------------------------------------------------------------- */

/* Constructors for  custom number objects representing  floating-point values, whose
 * internal representation is a user-pointer object.
 */
#define MODULE_FUNCTIONS__CONSTRUCTORS__FLOATING_POINT_NUMBERS__INTREP_USRPTR	1
static mmec_module_function_t const
  module_functions__constructors__floating_point_numbers__intrep_usrptr[MODULE_FUNCTIONS__CONSTRUCTORS__FLOATING_POINT_NUMBERS__INTREP_USRPTR] = {
  {
    .name		= "mmec-c-make-usrptr-float-from-usrptr-ldouble",
    .implementation	= Fmmec_make_usrptr_float_from_usrptr_ldouble,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `float'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: constructors, sint64.
 ** ----------------------------------------------------------------- */

/* Constructors for custom number objects of type "sint64".
 */
#define MODULE_FUNCTIONS__CONSTRUCTORS__SINT64		16
static mmec_module_function_t const
  module_functions__constructors__sint64[MODULE_FUNCTIONS__CONSTRUCTORS__SINT64] = {
  {
    .name		= "mmec-c-make-usrptr-sint64-from-integer-char",
    .implementation	= Fmmec_make_usrptr_sint64_from_integer_char,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-integer-schar",
    .implementation	= Fmmec_make_usrptr_sint64_from_integer_schar,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-integer-sshrt",
    .implementation	= Fmmec_make_usrptr_sint64_from_integer_sshrt,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-usrptr-sint",
    .implementation	= Fmmec_make_usrptr_sint64_from_usrptr_sint,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-usrptr-slong",
    .implementation	= Fmmec_make_usrptr_sint64_from_usrptr_slong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-usrptr-sllong",
    .implementation	= Fmmec_make_usrptr_sint64_from_usrptr_sllong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-integer-sint8",
    .implementation	= Fmmec_make_usrptr_sint64_from_integer_sint8,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-integer-sint16",
    .implementation	= Fmmec_make_usrptr_sint64_from_integer_sint16,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-usrptr-sint32",
    .implementation	= Fmmec_make_usrptr_sint64_from_usrptr_sint32,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-usrptr-sint64",
    .implementation	= Fmmec_make_usrptr_sint64_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-usrptr-ssize",
    .implementation	= Fmmec_make_usrptr_sint64_from_usrptr_ssize,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-usrptr-sintmax",
    .implementation	= Fmmec_make_usrptr_sint64_from_usrptr_sintmax,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-usrptr-wchar",
    .implementation	= Fmmec_make_usrptr_sint64_from_usrptr_wchar,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-usrptr-ptrdiff",
    .implementation	= Fmmec_make_usrptr_sint64_from_usrptr_ptrdiff,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-elisp-integer",
    .implementation	= Fmmec_make_usrptr_sint64_from_elisp_integer,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-sint64-from-elisp-float",
    .implementation	= Fmmec_make_usrptr_sint64_from_elisp_float,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `sint64'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: constructors, uint64.
 ** ----------------------------------------------------------------- */

/* Constructors for custom number objects of type "uint64".
 */
#define MODULE_FUNCTIONS__CONSTRUCTORS__UINT64		13
static mmec_module_function_t const
  module_functions__constructors__uint64[MODULE_FUNCTIONS__CONSTRUCTORS__UINT64] = {
  {
    .name		= "mmec-c-make-usrptr-uint64-from-integer-uchar",
    .implementation	= Fmmec_make_usrptr_uint64_from_integer_uchar,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-integer-ushrt",
    .implementation	= Fmmec_make_usrptr_uint64_from_integer_ushrt,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-usrptr-uint",
    .implementation	= Fmmec_make_usrptr_uint64_from_usrptr_uint,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-usrptr-ulong",
    .implementation	= Fmmec_make_usrptr_uint64_from_usrptr_ulong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-usrptr-ullong",
    .implementation	= Fmmec_make_usrptr_uint64_from_usrptr_ullong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-integer-uint8",
    .implementation	= Fmmec_make_usrptr_uint64_from_integer_uint8,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-integer-uint16",
    .implementation	= Fmmec_make_usrptr_uint64_from_integer_uint16,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-usrptr-uint32",
    .implementation	= Fmmec_make_usrptr_uint64_from_usrptr_uint32,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-usrptr-uint64",
    .implementation	= Fmmec_make_usrptr_uint64_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-usrptr-usize",
    .implementation	= Fmmec_make_usrptr_uint64_from_usrptr_usize,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-usrptr-uintmax",
    .implementation	= Fmmec_make_usrptr_uint64_from_usrptr_uintmax,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-elisp-integer",
    .implementation	= Fmmec_make_usrptr_uint64_from_elisp_integer,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-make-usrptr-uint64-from-elisp-float",
    .implementation	= Fmmec_make_usrptr_uint64_from_elisp_float,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `uint64'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: constructors, ldouble.
 ** ----------------------------------------------------------------- */

/* Constructors for custom number objects of type "ldouble".
 */
#define MODULE_FUNCTIONS__CONSTRUCTORS__LDOUBLE		7
static mmec_module_function_t const
  module_functions__constructors__ldouble[MODULE_FUNCTIONS__CONSTRUCTORS__LDOUBLE] = {
  {
    .name		= "mmec-c-make-usrptr-ldouble-from-usrptr-sint64",
    .implementation	= Fmmec_make_usrptr_ldouble_from_usrptr_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-make-usrptr-ldouble-from-usrptr-uint64",
    .implementation	= Fmmec_make_usrptr_ldouble_from_usrptr_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-make-usrptr-ldouble-from-usrptr-float",
    .implementation	= Fmmec_make_usrptr_ldouble_from_usrptr_float,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-make-usrptr-ldouble-from-float-double",
    .implementation	= Fmmec_make_usrptr_ldouble_from_float_double,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-make-usrptr-ldouble-from-usrptr-ldouble",
    .implementation	= Fmmec_make_usrptr_ldouble_from_usrptr_ldouble,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-make-usrptr-ldouble-from-elisp-integer",
    .implementation	= Fmmec_make_usrptr_ldouble_from_elisp_integer,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-make-usrptr-ldouble-from-elisp-float",
    .implementation	= Fmmec_make_usrptr_ldouble_from_elisp_float,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Build and return a user-pointer object of type `ldouble'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: number objects fit predicate functions.
 ** ----------------------------------------------------------------- */

#define MODULE_FUNCTIONS__FIT_PREDICATES	28
static mmec_module_function_t const
  module_functions__fit_predicates[MODULE_FUNCTIONS__FIT_PREDICATES] = {
  {
    .name		= "mmec-c-sint64-fits-char-p",
    .implementation	= Fmmec_sint64_fits_char_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `char'.",
  },
  {
    .name		= "mmec-c-sint64-fits-schar-p",
    .implementation	= Fmmec_sint64_fits_schar_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `schar'.",
  },
  {
    .name		= "mmec-c-uint64-fits-uchar-p",
    .implementation	= Fmmec_uint64_fits_uchar_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uchar'.",
  },

  {
    .name		= "mmec-c-sint64-fits-sshrt-p",
    .implementation	= Fmmec_sint64_fits_sshrt_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sshrt'.",
  },
  {
    .name		= "mmec-c-uint64-fits-ushrt-p",
    .implementation	= Fmmec_uint64_fits_ushrt_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ushrt'.",
  },
  {
    .name		= "mmec-c-sint64-fits-sint-p",
    .implementation	= Fmmec_sint64_fits_sint_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sint'.",
  },
  {
    .name		= "mmec-c-uint64-fits-uint-p",
    .implementation	= Fmmec_uint64_fits_uint_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uint'.",
  },
  {
    .name		= "mmec-c-sint64-fits-slong-p",
    .implementation	= Fmmec_sint64_fits_slong_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `slong'.",
  },
  {
    .name		= "mmec-c-uint64-fits-ulong-p",
    .implementation	= Fmmec_uint64_fits_ulong_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ulong'.",
  },
  {
    .name		= "mmec-c-sint64-fits-sllong-p",
    .implementation	= Fmmec_sint64_fits_sllong_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sllong'.",
  },
  {
    .name		= "mmec-c-uint64-fits-ullong-p",
    .implementation	= Fmmec_uint64_fits_ullong_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ullong'.",
  },

  {
    .name		= "mmec-c-sint64-fits-ssize-p",
    .implementation	= Fmmec_sint64_fits_ssize_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ssize'.",
  },
  {
    .name		= "mmec-c-uint64-fits-usize-p",
    .implementation	= Fmmec_uint64_fits_usize_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `usize'.",
  },
  {
    .name		= "mmec-c-sint64-fits-sintmax-p",
    .implementation	= Fmmec_sint64_fits_sintmax_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sintmax'.",
  },
  {
    .name		= "mmec-c-uint64-fits-uintmax-p",
    .implementation	= Fmmec_uint64_fits_uintmax_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uintmax'.",
  },
  {
    .name		= "mmec-c-sint64-fits-wchar-p",
    .implementation	= Fmmec_sint64_fits_wchar_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `wchar'.",
  },
  {
    .name		= "mmec-c-sint64-fits-ptrdiff-p",
    .implementation	= Fmmec_sint64_fits_ptrdiff_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ptrdiff'.",
  },

  {
    .name		= "mmec-c-sint64-fits-sint8-p",
    .implementation	= Fmmec_sint64_fits_sint8_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sint8'.",
  },
  {
    .name		= "mmec-c-uint64-fits-uint8-p",
    .implementation	= Fmmec_uint64_fits_uint8_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uint8'.",
  },
  {
    .name		= "mmec-c-sint64-fits-sint16-p",
    .implementation	= Fmmec_sint64_fits_sint16_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sint16'.",
  },
  {
    .name		= "mmec-c-uint64-fits-uint16-p",
    .implementation	= Fmmec_uint64_fits_uint16_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uint16'.",
  },
  {
    .name		= "mmec-c-sint64-fits-sint32-p",
    .implementation	= Fmmec_sint64_fits_sint32_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sint32'.",
  },
  {
    .name		= "mmec-c-uint64-fits-uint32-p",
    .implementation	= Fmmec_uint64_fits_uint32_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uint32'.",
  },
  {
    .name		= "mmec-c-sint64-fits-sint64-p",
    .implementation	= Fmmec_sint64_fits_sint64_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-uint64-fits-uint64-p",
    .implementation	= Fmmec_uint64_fits_uint64_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `uint64'.",
  },

  {
    .name		= "mmec-c-ldouble-fits-float-p",
    .implementation	= Fmmec_ldouble_fits_float_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-ldouble-fits-double-p",
    .implementation	= Fmmec_ldouble_fits_double_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `double'.",
  },
  {
    .name		= "mmec-c-ldouble-fits-ldouble-p",
    .implementation	= Fmmec_ldouble_fits_ldouble_range_p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Return true if the argument fits a user-pointer object of type `ldouble'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: number objects printer functions.
 ** ----------------------------------------------------------------- */

#define MODULE_FUNCTIONS__PRINTERS	18
static mmec_module_function_t const
  module_functions__printers[MODULE_FUNCTIONS__PRINTERS] = {
  {
    .name		= "mmec-c-sint-print-to-string",
    .implementation	= Fmmec_c_sint_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `sint'.",
  },
  {
    .name		= "mmec-c-uint-print-to-string",
    .implementation	= Fmmec_c_uint_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `uint'.",
  },
  {
    .name		= "mmec-c-slong-print-to-string",
    .implementation	= Fmmec_c_slong_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `slong'.",
  },
  {
    .name		= "mmec-c-ulong-print-to-string",
    .implementation	= Fmmec_c_ulong_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `ulong'.",
  },
  {
    .name		= "mmec-c-sllong-print-to-string",
    .implementation	= Fmmec_c_sllong_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `sllong'.",
  },
  {
    .name		= "mmec-c-ullong-print-to-string",
    .implementation	= Fmmec_c_ullong_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `ullong'.",
  },
  {
    .name		= "mmec-c-sintmax-print-to-string",
    .implementation	= Fmmec_c_sintmax_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `sintmax'.",
  },
  {
    .name		= "mmec-c-uintmax-print-to-string",
    .implementation	= Fmmec_c_uintmax_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `uintmax'.",
  },
  {
    .name		= "mmec-c-ssize-print-to-string",
    .implementation	= Fmmec_c_ssize_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `ssize'.",
  },
  {
    .name		= "mmec-c-usize-print-to-string",
    .implementation	= Fmmec_c_usize_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `usize'.",
  },
  {
    .name		= "mmec-c-wchar-print-to-string",
    .implementation	= Fmmec_c_wchar_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `wchar'.",
  },
  {
    .name		= "mmec-c-ptrdiff-print-to-string",
    .implementation	= Fmmec_c_ptrdiff_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `ptrdiff'.",
  },
  {
    .name		= "mmec-c-sint32-print-to-string",
    .implementation	= Fmmec_c_sint32_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `sint32'.",
  },
  {
    .name		= "mmec-c-uint32-print-to-string",
    .implementation	= Fmmec_c_uint32_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `uint32'.",
  },
  {
    .name		= "mmec-c-sint64-print-to-string",
    .implementation	= Fmmec_c_sint64_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-uint64-print-to-string",
    .implementation	= Fmmec_c_uint64_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-float-print-to-string",
    .implementation	= Fmmec_c_float_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-ldouble-print-to-string",
    .implementation	= Fmmec_c_ldouble_print_to_string,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Print to string a user-pointer object of type `ldouble'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: sint64 comparison functions.
 ** ----------------------------------------------------------------- */

#define MODULE_FUNCTIONS__SINT64_COMPARISON	6
static mmec_module_function_t const
  module_functions__sint64_comparison[MODULE_FUNCTIONS__SINT64_COMPARISON] = {
  {
    .name		= "mmec-c-sint64=",
    .implementation	= Fmmec_compare_sint64_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is equal to OP2.",
  },
  {
    .name		= "mmec-c-sint64/=",
    .implementation	= Fmmec_compare_sint64_not_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is not equal to OP2.",
  },
  {
    .name		= "mmec-c-sint64<",
    .implementation	= Fmmec_compare_sint64_less,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than OP2.",
  },
  {
    .name		= "mmec-c-sint64>",
    .implementation	= Fmmec_compare_sint64_greater,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than OP2.",
  },
  {
    .name		= "mmec-c-sint64<=",
    .implementation	= Fmmec_compare_sint64_less_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than or equal to OP2.",
  },
  {
    .name		= "mmec-c-sint64>=",
    .implementation	= Fmmec_compare_sint64_greater_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than or equal to OP2.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: uint64 comparison functions.
 ** ----------------------------------------------------------------- */

#define MODULE_FUNCTIONS__UINT64_COMPARISON	6
static mmec_module_function_t const
  module_functions__uint64_comparison[MODULE_FUNCTIONS__UINT64_COMPARISON] = {
  {
    .name		= "mmec-c-uint64=",
    .implementation	= Fmmec_compare_uint64_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is equal to OP2.",
  },
  {
    .name		= "mmec-c-uint64/=",
    .implementation	= Fmmec_compare_uint64_not_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is not equal to OP2.",
  },
  {
    .name		= "mmec-c-uint64<",
    .implementation	= Fmmec_compare_uint64_less,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than OP2.",
  },
  {
    .name		= "mmec-c-uint64>",
    .implementation	= Fmmec_compare_uint64_greater,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than OP2.",
  },
  {
    .name		= "mmec-c-uint64<=",
    .implementation	= Fmmec_compare_uint64_less_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than or equal to OP2.",
  },
  {
    .name		= "mmec-c-uint64>=",
    .implementation	= Fmmec_compare_uint64_greater_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than or equal to OP2.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: ldouble comparison functions.
 ** ----------------------------------------------------------------- */

#define MODULE_FUNCTIONS__LDOUBLE_COMPARISON	6
static mmec_module_function_t const
  module_functions__ldouble_comparison[MODULE_FUNCTIONS__LDOUBLE_COMPARISON] = {
  {
    .name		= "mmec-c-ldouble=",
    .implementation	= Fmmec_compare_ldouble_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is equal to OP2.",
  },
  {
    .name		= "mmec-c-ldouble/=",
    .implementation	= Fmmec_compare_ldouble_not_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is not equal to OP2.",
  },
  {
    .name		= "mmec-c-ldouble<",
    .implementation	= Fmmec_compare_ldouble_less,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than OP2.",
  },
  {
    .name		= "mmec-c-ldouble>",
    .implementation	= Fmmec_compare_ldouble_greater,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than OP2.",
  },
  {
    .name		= "mmec-c-ldouble<=",
    .implementation	= Fmmec_compare_ldouble_less_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than or equal to OP2.",
  },
  {
    .name		= "mmec-c-ldouble>=",
    .implementation	= Fmmec_compare_ldouble_greater_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than or equal to OP2.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: sint64-uint64 comparison functions.
 ** ----------------------------------------------------------------- */

#define MODULE_FUNCTIONS__SINT64_UINT64_COMPARISON	6
static mmec_module_function_t const
  module_functions__sint64_uint64_comparison[MODULE_FUNCTIONS__SINT64_UINT64_COMPARISON] = {
  {
    .name		= "mmec-c-sint64-uint64=",
    .implementation	= Fmmec_compare_sint64_uint64_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is equal to OP2.",
  },
  {
    .name		= "mmec-c-sint64-uint64/=",
    .implementation	= Fmmec_compare_sint64_uint64_not_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is not equal to OP2.",
  },
  {
    .name		= "mmec-c-sint64-uint64<",
    .implementation	= Fmmec_compare_sint64_uint64_less,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than OP2.",
  },
  {
    .name		= "mmec-c-sint64-uint64>",
    .implementation	= Fmmec_compare_sint64_uint64_greater,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than OP2.",
  },
  {
    .name		= "mmec-c-sint64-uint64<=",
    .implementation	= Fmmec_compare_sint64_uint64_less_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than or equal to OP2.",
  },
  {
    .name		= "mmec-c-sint64-uint64>=",
    .implementation	= Fmmec_compare_sint64_uint64_greater_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than or equal to OP2.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: uint64-sint64 comparison functions.
 ** ----------------------------------------------------------------- */

#define MODULE_FUNCTIONS__UINT64_SINT64_COMPARISON	6
static mmec_module_function_t const
  module_functions__uint64_sint64_comparison[MODULE_FUNCTIONS__UINT64_SINT64_COMPARISON] = {
  {
    .name		= "mmec-c-uint64-sint64=",
    .implementation	= Fmmec_compare_uint64_sint64_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is equal to OP2.",
  },
  {
    .name		= "mmec-c-uint64-sint64/=",
    .implementation	= Fmmec_compare_uint64_sint64_not_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is not equal to OP2.",
  },
  {
    .name		= "mmec-c-uint64-sint64<",
    .implementation	= Fmmec_compare_uint64_sint64_less,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than OP2.",
  },
  {
    .name		= "mmec-c-uint64-sint64>",
    .implementation	= Fmmec_compare_uint64_sint64_greater,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than OP2.",
  },
  {
    .name		= "mmec-c-uint64-sint64<=",
    .implementation	= Fmmec_compare_uint64_sint64_less_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is less than or equal to OP2.",
  },
  {
    .name		= "mmec-c-uint64-sint64>=",
    .implementation	= Fmmec_compare_uint64_sint64_greater_equal,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return true if the OP1 is greater than or equal to OP2.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: sign inspection functions.
 ** ----------------------------------------------------------------- */

#undef  SIGN_INSPECTION_BLOB
#define SIGN_INSPECTION_BLOB(STEM)					\
  {									\
    .name		= "mmec-c-" #STEM "-zero-p",			\
      .implementation	= Fmmec_c_ ## STEM ## _zero_p,			\
      .min_arity		= 1,					\
      .max_arity		= 1,					\
      .documentation	= "Return true if the operand is zero, otherwise return false.", \
      },								\
  {									\
    .name		= "mmec-c-" #STEM "-positive-p",		\
      .implementation	= Fmmec_c_ ## STEM ## _positive_p,		\
      .min_arity		= 1,					\
      .max_arity		= 1,					\
      .documentation	= "Return true if the operand is positive, otherwise return false.", \
      },								\
  {									\
    .name		= "mmec-c-" #STEM "-negative-p",		\
      .implementation	= Fmmec_c_ ## STEM ## _negative_p,		\
      .min_arity		= 1,					\
      .max_arity		= 1,					\
      .documentation	= "Return true if the operand is negative, otherwise return false.", \
      },								\
  {									\
    .name		= "mmec-c-" #STEM "-non-positive-p",		\
      .implementation	= Fmmec_c_ ## STEM ## _non_positive_p,		\
      .min_arity		= 1,					\
      .max_arity		= 1,					\
      .documentation	= "Return true if the operand is non-positive, otherwise return false.", \
      },								\
  {									\
    .name		= "mmec-c-" #STEM "-non-negative-p",		\
      .implementation	= Fmmec_c_ ## STEM ## _non_negative_p,		\
      .min_arity		= 1,					\
      .max_arity		= 1,					\
      .documentation	= "Return true if the operand is non-negative, otherwise return false.", \
      },

#define MODULE_FUNCTIONS__SIGN_INSPECTION	(5 * 28)
static mmec_module_function_t const
  module_functions__sign_inspection[MODULE_FUNCTIONS__SIGN_INSPECTION] = {
  SIGN_INSPECTION_BLOB(char)
  SIGN_INSPECTION_BLOB(schar)
  SIGN_INSPECTION_BLOB(sshrt)
  SIGN_INSPECTION_BLOB(sint)
  SIGN_INSPECTION_BLOB(slong)
  SIGN_INSPECTION_BLOB(sllong)
  SIGN_INSPECTION_BLOB(sintmax)
  SIGN_INSPECTION_BLOB(ssize)
  SIGN_INSPECTION_BLOB(wchar)
  SIGN_INSPECTION_BLOB(ptrdiff)
  SIGN_INSPECTION_BLOB(sint8)
  SIGN_INSPECTION_BLOB(sint16)
  SIGN_INSPECTION_BLOB(sint32)
  SIGN_INSPECTION_BLOB(sint64)
  SIGN_INSPECTION_BLOB(float)
  SIGN_INSPECTION_BLOB(double)
  SIGN_INSPECTION_BLOB(ldouble)

  SIGN_INSPECTION_BLOB(uchar)
  SIGN_INSPECTION_BLOB(ushrt)
  SIGN_INSPECTION_BLOB(uint)
  SIGN_INSPECTION_BLOB(ulong)
  SIGN_INSPECTION_BLOB(ullong)
  SIGN_INSPECTION_BLOB(uintmax)
  SIGN_INSPECTION_BLOB(usize)
  SIGN_INSPECTION_BLOB(uint8)
  SIGN_INSPECTION_BLOB(uint16)
  SIGN_INSPECTION_BLOB(uint32)
  SIGN_INSPECTION_BLOB(uint64)
};


/** --------------------------------------------------------------------
 ** Module initialisation.
 ** ----------------------------------------------------------------- */

void
mmec_number_objects_init (emacs_env * env)
{
  mmec_define_elisp_functions_from_table(env,
					 module_functions__constructors__signed_integers__intrep_integer,
					 MODULE_FUNCTIONS__CONSTRUCTORS__SIGNED_INTEGERS__INTREP_INTEGER,
					 0);

  mmec_define_elisp_functions_from_table(env,
  					 module_functions__constructors__unsigned_integers__intrep_integer,
  					 MODULE_FUNCTIONS__CONSTRUCTORS__UNSIGNED_INTEGERS__INTREP_INTEGER,
  					 0);

  mmec_define_elisp_functions_from_table(env,
  					 module_functions__constructors__floating_point_numbers__intrep_float,
  					 MODULE_FUNCTIONS__CONSTRUCTORS__FLOATING_POINT_NUMBERS__INTREP_FLOAT,
  					 0);

  mmec_define_elisp_functions_from_table(env,
  					 module_functions__constructors__signed_integers__intrep_usrptr,
  					 MODULE_FUNCTIONS__CONSTRUCTORS__SIGNED_INTEGERS__INTREP_USRPTR,
  					 0);

  mmec_define_elisp_functions_from_table(env,
  					 module_functions__constructors__unsigned_integers__intrep_usrptr,
  					 MODULE_FUNCTIONS__CONSTRUCTORS__UNSIGNED_INTEGERS__INTREP_USRPTR,
  					 0);

  mmec_define_elisp_functions_from_table(env,
  					 module_functions__constructors__floating_point_numbers__intrep_usrptr,
  					 MODULE_FUNCTIONS__CONSTRUCTORS__FLOATING_POINT_NUMBERS__INTREP_USRPTR,
  					 0);

  mmec_define_elisp_functions_from_table(env,
  					 module_functions__constructors__sint64,
  					 MODULE_FUNCTIONS__CONSTRUCTORS__SINT64,
  					 0);

  mmec_define_elisp_functions_from_table(env,
  					 module_functions__constructors__uint64,
  					 MODULE_FUNCTIONS__CONSTRUCTORS__UINT64,
  					 0);

  mmec_define_elisp_functions_from_table(env,
  					 module_functions__constructors__ldouble,
  					 MODULE_FUNCTIONS__CONSTRUCTORS__LDOUBLE,
  					 0);

  mmec_define_elisp_functions_from_table(env, module_functions__printers,           MODULE_FUNCTIONS__PRINTERS,           0);
  mmec_define_elisp_functions_from_table(env, module_functions__fit_predicates,     MODULE_FUNCTIONS__FIT_PREDICATES,     0);
  mmec_define_elisp_functions_from_table(env, module_functions__sint64_comparison,  MODULE_FUNCTIONS__SINT64_COMPARISON,  0);
  mmec_define_elisp_functions_from_table(env, module_functions__uint64_comparison,  MODULE_FUNCTIONS__UINT64_COMPARISON,  0);
  mmec_define_elisp_functions_from_table(env, module_functions__ldouble_comparison, MODULE_FUNCTIONS__LDOUBLE_COMPARISON, 0);

  mmec_define_elisp_functions_from_table(env,
  					 module_functions__sint64_uint64_comparison,
  					 MODULE_FUNCTIONS__SINT64_UINT64_COMPARISON,
  					 0);

  mmec_define_elisp_functions_from_table(env,
  					 module_functions__uint64_sint64_comparison,
  					 MODULE_FUNCTIONS__UINT64_SINT64_COMPARISON,
  					 0);

  mmec_define_elisp_functions_from_table(env, module_functions__sign_inspection, MODULE_FUNCTIONS__SIGN_INSPECTION, 0);
}

/* end of file */
