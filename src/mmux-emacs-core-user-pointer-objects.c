/*
  Part of: MMUX Emacs Core
  Contents: definitions of user-pointer objects
  Date: Feb  1, 2020

  Abstract

	This module implements user-pointer objects.

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


/** --------------------------------------------------------------------
 ** Constructors for number objects whose internal representation is a built-in "integer".
 ** ----------------------------------------------------------------- */

/* This is for signed numbers. */
#undef  MMUX_EMACS_CORE_DEFINE_MAKER_1
#define MMUX_EMACS_CORE_DEFINE_MAKER_1(STEM, CAPSTEM, ARGSTEM)		\
  emacs_value								\
  Fmmux_emacs_core_make_ ## STEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(nargs == 1);							\
    mmux_emacs_core_type_ ## ARGSTEM ## _t val =			\
      mmux_emacs_core_get_ ## ARGSTEM(env, args[0]);			\
									\
    if ((MMUX_EMACS_CORE_ ## CAPSTEM ## _MIN <= val) &&			\
	(val <= MMUX_EMACS_CORE_ ## CAPSTEM ## _MAX)) {			\
      return mmux_emacs_core_make_integer(env, (mmux_emacs_core_type_ ## STEM ## _t)val); \
    } else {								\
      return mmux_emacs_core_error_value_out_of_range(env);		\
    }									\
  }

/* This is for unsigned integers. */
#undef  MMUX_EMACS_CORE_DEFINE_MAKER_2
#define MMUX_EMACS_CORE_DEFINE_MAKER_2(STEM, CAPSTEM, ARGSTEM)		\
  emacs_value								\
  Fmmux_emacs_core_make_ ## STEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(nargs == 1);							\
    mmux_emacs_core_type_ ## ARGSTEM ## _t val =			\
      mmux_emacs_core_get_ ## ARGSTEM(env, args[0]);			\
									\
    if (val <= MMUX_EMACS_CORE_ ## CAPSTEM ## _MAX) {			\
      return mmux_emacs_core_make_integer(env, (mmux_emacs_core_type_ ## STEM ## _t)val); \
    } else {								\
      return mmux_emacs_core_error_value_out_of_range(env);		\
    }									\
  }

/* This is for signed integer numbers. */
#undef  MMUX_EMACS_CORE_DEFINE_MAKER_11
#define MMUX_EMACS_CORE_DEFINE_MAKER_11(STEM, CAPSTEM)		\
  MMUX_EMACS_CORE_DEFINE_MAKER_1(STEM, CAPSTEM, sint64)

/* This is for unsigned integer numbers. */
#undef  MMUX_EMACS_CORE_DEFINE_MAKER_21
#define MMUX_EMACS_CORE_DEFINE_MAKER_21(STEM, CAPSTEM)		\
  MMUX_EMACS_CORE_DEFINE_MAKER_2(STEM, CAPSTEM, uint64)

MMUX_EMACS_CORE_DEFINE_MAKER_11(char,	CHAR)
MMUX_EMACS_CORE_DEFINE_MAKER_11(schar,	SCHAR)
MMUX_EMACS_CORE_DEFINE_MAKER_21(uchar,	UCHAR)
MMUX_EMACS_CORE_DEFINE_MAKER_11(sshrt,	SSHRT)
MMUX_EMACS_CORE_DEFINE_MAKER_21(ushrt,	USHRT)
MMUX_EMACS_CORE_DEFINE_MAKER_11(sint8,	SINT8)
MMUX_EMACS_CORE_DEFINE_MAKER_21(uint8,	UINT8)
MMUX_EMACS_CORE_DEFINE_MAKER_11(sint16,	SINT16)
MMUX_EMACS_CORE_DEFINE_MAKER_21(uint16,	UINT16)


/** --------------------------------------------------------------------
 ** User-pointer objects: exact integers not represantable by an intmax_t.
 ** ----------------------------------------------------------------- */

#undef  MMUX_EMACS_CORE_DEFINE_ALLOCATOR_AND_FINALIZER
#define  MMUX_EMACS_CORE_DEFINE_ALLOCATOR_AND_FINALIZER(STEM)		\
  static void								\
  mmux_emacs_core_ ## STEM ## _finalizer (void * _obj)			\
  {									\
    MMUX_EMACS_CORE_PC(mmux_emacs_core_ ## STEM ##_t, obj, _obj);	\
    free(obj);								\
  }									\
									\
  emacs_value								\
  mmux_emacs_core_make_ ## STEM (emacs_env *env, mmux_emacs_core_type_ ## STEM ## _t val) \
  {									\
    mmux_emacs_core_ ## STEM ##_t *obj;					\
									\
    errno = 0;								\
    obj   = (mmux_emacs_core_ ## STEM ##_t *)malloc(sizeof(mmux_emacs_core_ ## STEM ##_t)); \
    if (obj) {								\
      obj->val	= val;							\
									\
      return mmux_emacs_core_make_user_ptr(env, mmux_emacs_core_ ## STEM ##_finalizer, obj); \
    } else {								\
      return mmux_emacs_core_error_memory_allocation(env);		\
    }									\
  }									\

/* This is for signed numbers. */
#undef  MMUX_EMACS_CORE_DEFINE_MAKER_3
#define MMUX_EMACS_CORE_DEFINE_MAKER_3(STEM, CAPSTEM, ARGSTEM)		\
  MMUX_EMACS_CORE_DEFINE_ALLOCATOR_AND_FINALIZER(STEM)			\
    emacs_value								\
    Fmmux_emacs_core_make_ ## STEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(nargs == 1);							\
    mmux_emacs_core_type_ ## ARGSTEM ## _t val =			\
      mmux_emacs_core_get_ ## ARGSTEM(env, args[0]);			\
									\
    if ((MMUX_EMACS_CORE_ ## CAPSTEM ## _MIN <= val) &&			\
	(val <= MMUX_EMACS_CORE_ ## CAPSTEM ## _MAX)) {			\
      return mmux_emacs_core_make_ ## STEM(env, (mmux_emacs_core_type_ ## STEM ## _t)val); \
    } else {								\
      return mmux_emacs_core_error_value_out_of_range(env);		\
    }									\
  }

/* This is for unsigned numbers. */
#undef  MMUX_EMACS_CORE_DEFINE_MAKER_4
#define MMUX_EMACS_CORE_DEFINE_MAKER_4(STEM, CAPSTEM, ARGSTEM)		\
  MMUX_EMACS_CORE_DEFINE_ALLOCATOR_AND_FINALIZER(STEM)			\
    emacs_value								\
    Fmmux_emacs_core_make_ ## STEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(nargs == 1);							\
    mmux_emacs_core_type_ ## ARGSTEM ## _t val =			\
      mmux_emacs_core_get_ ## ARGSTEM(env, args[0]);			\
									\
    if (val <= MMUX_EMACS_CORE_ ## CAPSTEM ## _MAX) {			\
      return mmux_emacs_core_make_ ## STEM(env, (mmux_emacs_core_type_ ## STEM ## _t)val); \
    } else {								\
      return mmux_emacs_core_error_value_out_of_range(env);		\
    }									\
  }

/* This is for signed integer numbers. */
#undef  MMUX_EMACS_CORE_DEFINE_MAKER_31
#define MMUX_EMACS_CORE_DEFINE_MAKER_31(STEM, CAPSTEM)		\
  MMUX_EMACS_CORE_DEFINE_MAKER_3(STEM, CAPSTEM, sint64)

/* This is for unsigned integer numbers. */
#undef  MMUX_EMACS_CORE_DEFINE_MAKER_41
#define MMUX_EMACS_CORE_DEFINE_MAKER_41(STEM, CAPSTEM)		\
  MMUX_EMACS_CORE_DEFINE_MAKER_4(STEM, CAPSTEM, uint64)

/* This is for floating-point numbers. */
#undef  MMUX_EMACS_CORE_DEFINE_MAKER_32
#define MMUX_EMACS_CORE_DEFINE_MAKER_32(STEM, CAPSTEM)		\
  MMUX_EMACS_CORE_DEFINE_MAKER_3(STEM, CAPSTEM, ldouble)

MMUX_EMACS_CORE_DEFINE_MAKER_41(wchar,		WCHAR)
MMUX_EMACS_CORE_DEFINE_MAKER_41(uint,		UINT)
MMUX_EMACS_CORE_DEFINE_MAKER_31(sint,		SINT)
MMUX_EMACS_CORE_DEFINE_MAKER_41(ulong,		ULONG)
MMUX_EMACS_CORE_DEFINE_MAKER_31(slong,		SLONG)
MMUX_EMACS_CORE_DEFINE_MAKER_41(ullong,		ULLONG)
MMUX_EMACS_CORE_DEFINE_MAKER_31(sllong,		SLLONG)
MMUX_EMACS_CORE_DEFINE_MAKER_41(uint32,		UINT32)
MMUX_EMACS_CORE_DEFINE_MAKER_31(sint32,		SINT32)
MMUX_EMACS_CORE_DEFINE_MAKER_41(uint64,		UINT64)
MMUX_EMACS_CORE_DEFINE_MAKER_31(sint64,		SINT64)
MMUX_EMACS_CORE_DEFINE_MAKER_41(uintmax,	UINTMAX)
MMUX_EMACS_CORE_DEFINE_MAKER_31(sintmax,	SINTMAX)
MMUX_EMACS_CORE_DEFINE_MAKER_41(usize,		USIZE)
MMUX_EMACS_CORE_DEFINE_MAKER_31(ssize,		SSIZE)
MMUX_EMACS_CORE_DEFINE_MAKER_31(ptrdiff,	PTRDIFF)

MMUX_EMACS_CORE_DEFINE_MAKER_32(float,		FLOAT)
MMUX_EMACS_CORE_DEFINE_MAKER_32(ldouble,	LDOUBLE)


/** --------------------------------------------------------------------
 ** User-pointer objects: bytevectors.
 ** ----------------------------------------------------------------- */

static void
mmux_emacs_core_bytevector_finalizer (void * _obj)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_bytevector_t, obj, _obj);

  free(obj->ptr);
  free(obj);
}

static emacs_value
Fmmux_emacs_core_make_bytevector (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(nargs == 3);
  size_t	number_of_slots		= (size_t)mmux_emacs_core_extract_integer(env, args[0]);
  size_t	slot_size		= (size_t)mmux_emacs_core_extract_integer(env, args[1]);
  int		hold_signed_values	= (int)   mmux_emacs_core_extract_integer(env, args[2]);
  mmux_emacs_core_bytevector_t	* obj;

  errno = 0;
  obj   = (mmux_emacs_core_bytevector_t *)malloc(sizeof(mmux_emacs_core_bytevector_t));
  if (obj) {
    errno	= 0;
    obj->ptr	= (uint8_t *)calloc(number_of_slots, slot_size);
    if (obj->ptr) {
      obj->number_of_slots	= number_of_slots;
      obj->slot_size		= slot_size;
      obj->hold_signed_values	= hold_signed_values? 1 : 0;
      return mmux_emacs_core_make_user_ptr(env, mmux_emacs_core_bytevector_finalizer, obj);
    } else {
      return mmux_emacs_core_error_memory_allocation(env);
    }
  } else {
    return mmux_emacs_core_error_memory_allocation(env);
  }
}


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_FUNCTIONS	28
static mmux_emacs_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  {
    .name		= "mmux-core-c-make-bytevector",
    .implementation	= Fmmux_emacs_core_make_bytevector,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Build and return a new bytevector user pointer object."
  },

  /* Constructors  for  custom number  objects  whose  internal representation  is  a
     built-in "integer" object. */
  {
    .name		= "mmux-core-c-make-char",
    .implementation	= Fmmux_emacs_core_make_char,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-char'.",
  },
  {
    .name		= "mmux-core-c-make-schar",
    .implementation	= Fmmux_emacs_core_make_schar,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-schar'.",
  },
  {
    .name		= "mmux-core-c-make-uchar",
    .implementation	= Fmmux_emacs_core_make_uchar,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uchar'.",
  },
  {
    .name		= "mmux-core-c-make-sshrt",
    .implementation	= Fmmux_emacs_core_make_sshrt,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sshrt'.",
  },
  {
    .name		= "mmux-core-c-make-ushrt",
    .implementation	= Fmmux_emacs_core_make_ushrt,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-ushrt'.",
  },
  {
    .name		= "mmux-core-c-make-sint8",
    .implementation	= Fmmux_emacs_core_make_sint8,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sint8'.",
  },
  {
    .name		= "mmux-core-c-make-uint8",
    .implementation	= Fmmux_emacs_core_make_uint8,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uint8'.",
  },
  {
    .name		= "mmux-core-c-make-sint16",
    .implementation	= Fmmux_emacs_core_make_sint16,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sint16'.",
  },
  {
    .name		= "mmux-core-c-make-uint16",
    .implementation	= Fmmux_emacs_core_make_uint16,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uint16'.",
  },

  /* Constructors  for  custom number  objects  whose  internal representation  is  a
     user-pointer object. */
  {
    .name		= "mmux-core-c-make-uint",
    .implementation	= Fmmux_emacs_core_make_uint,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uint'.",
  },
  {
    .name		= "mmux-core-c-make-sint",
    .implementation	= Fmmux_emacs_core_make_sint,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sint'.",
  },
  {
    .name		= "mmux-core-c-make-ulong",
    .implementation	= Fmmux_emacs_core_make_ulong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-ulong'.",
  },
  {
    .name		= "mmux-core-c-make-slong",
    .implementation	= Fmmux_emacs_core_make_slong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-slong'.",
  },
  {
    .name		= "mmux-core-c-make-ullong",
    .implementation	= Fmmux_emacs_core_make_ullong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-ullong'.",
  },
  {
    .name		= "mmux-core-c-make-sllong",
    .implementation	= Fmmux_emacs_core_make_sllong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sllong'.",
  },
  {
    .name		= "mmux-core-c-make-uint32",
    .implementation	= Fmmux_emacs_core_make_uint32,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uint32'.",
  },
  {
    .name		= "mmux-core-c-make-sint32",
    .implementation	= Fmmux_emacs_core_make_sint32,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sint32'.",
  },
  {
    .name		= "mmux-core-c-make-uint64",
    .implementation	= Fmmux_emacs_core_make_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uint64'.",
  },
  {
    .name		= "mmux-core-c-make-sint64",
    .implementation	= Fmmux_emacs_core_make_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sint64'.",
  },
  {
    .name		= "mmux-core-c-make-uintmax",
    .implementation	= Fmmux_emacs_core_make_uintmax,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uintmax'.",
  },
  {
    .name		= "mmux-core-c-make-sintmax",
    .implementation	= Fmmux_emacs_core_make_sintmax,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sintmax'.",
  },
  {
    .name		= "mmux-core-c-make-usize",
    .implementation	= Fmmux_emacs_core_make_usize,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-usize'.",
  },
  {
    .name		= "mmux-core-c-make-ssize",
    .implementation	= Fmmux_emacs_core_make_ssize,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-ssize'.",
  },
  {
    .name		= "mmux-core-c-make-ptrdiff",
    .implementation	= Fmmux_emacs_core_make_ptrdiff,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-ptrdiff'.",
  },
  {
    .name		= "mmux-core-c-make-wchar",
    .implementation	= Fmmux_emacs_core_make_wchar,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-wchar'.",
  },
  {
    .name		= "mmux-core-c-make-float",
    .implementation	= Fmmux_emacs_core_make_float,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-float'.",
  },
  {
    .name		= "mmux-core-c-make-ldouble",
    .implementation	= Fmmux_emacs_core_make_ldouble,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-ldouble'.",
  },
};


/** --------------------------------------------------------------------
 ** Module initialisation.
 ** ----------------------------------------------------------------- */

void
mmux_emacs_core_user_ptr_objects_init (emacs_env * env)
{
  mmux_emacs_define_functions_from_table(env, module_functions_table, NUMBER_OF_MODULE_FUNCTIONS, 0);
}

/* end of file */
