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
 ** User-pointer objects: sint32 exact integers.
 ** ----------------------------------------------------------------- */

#undef  MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER
#define MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(STEM, CTYPE, ETYPE, ARG_GETTER_STEM)	\
  static void								\
  mmux_emacs_core_ ## STEM ## _finalizer (void * _obj)			\
  {									\
    MMUX_EMACS_CORE_PC(mmux_emacs_core_ ## STEM ##_t, obj, _obj);	\
    free(obj);								\
  }									\
									\
  emacs_value								\
  mmux_emacs_core_make_ ## STEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED) \
  {									\
    assert(nargs == 1);							\
    ETYPE val = mmux_emacs_core_get_ ## ARG_GETTER_STEM(env, args[0]);	\
    mmux_emacs_core_ ## STEM ##_t *obj;					\
									\
    errno = 0;								\
    obj   = (mmux_emacs_core_ ## STEM ##_t *)malloc(sizeof(mmux_emacs_core_ ## STEM ##_t)); \
    if (obj) {								\
      obj->val	= (CTYPE) val;						\
									\
      return mmux_emacs_core_make_user_ptr(env, mmux_emacs_core_ ## STEM ##_finalizer, obj); \
    } else {								\
      return mmux_emacs_core_error_memory_allocation(env);		\
    }									\
  }

MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(uint,	unsigned int,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(sint,        signed int,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(ulong,	unsigned long int,	intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(slong,         signed long int,	intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(ullong,	unsigned long long int,	intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(sllong,        signed long long int,	intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(uint32,	uint32_t,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(sint32,       int32_t,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(uint64,	uint64_t,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(sint64,	 int64_t,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(uintmax,	uintmax_t,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(sintmax,	 intmax_t,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(usize,	 size_t,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(ssize,	ssize_t,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(ptrdiff,	ptrdiff_t,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(wchar,	wchar_t,		intmax_t,     integer)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(float,	float,			double,	      float)
MMUX_EMACS_CORE_DEFINE_MAKER_AND_FINALIZER(long_double,	long double,		double,	      float)


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
  size_t	number_of_slots		= (size_t)mmux_emacs_core_get_integer(env, args[0]);
  size_t	slot_size		= (size_t)mmux_emacs_core_get_integer(env, args[1]);
  int		hold_signed_values	= (int)   mmux_emacs_core_get_integer(env, args[2]);
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

#define NUMBER_OF_MODULE_FUNCTIONS	19
static mmux_emacs_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  {
    .name		= "mmux-core-c-bytevector-make",
    .implementation	= Fmmux_emacs_core_make_bytevector,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Build and return a new bytevector user pointer object."
  },

  {
    .name		= "mmux-core-c-make-uint",
    .implementation	= mmux_emacs_core_make_uint,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uint'.",
  },
  {
    .name		= "mmux-core-c-make-sint",
    .implementation	= mmux_emacs_core_make_sint,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sint'.",
  },
  {
    .name		= "mmux-core-c-make-ulong",
    .implementation	= mmux_emacs_core_make_ulong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-ulong'.",
  },
  {
    .name		= "mmux-core-c-make-slong",
    .implementation	= mmux_emacs_core_make_slong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-slong'.",
  },
  {
    .name		= "mmux-core-c-make-ullong",
    .implementation	= mmux_emacs_core_make_ullong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-ullong'.",
  },
  {
    .name		= "mmux-core-c-make-sllong",
    .implementation	= mmux_emacs_core_make_sllong,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sllong'.",
  },
  {
    .name		= "mmux-core-c-make-uint32",
    .implementation	= mmux_emacs_core_make_uint32,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uint32'.",
  },
  {
    .name		= "mmux-core-c-make-sint32",
    .implementation	= mmux_emacs_core_make_sint32,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sint32'.",
  },
  {
    .name		= "mmux-core-c-make-uint64",
    .implementation	= mmux_emacs_core_make_uint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uint64'.",
  },
  {
    .name		= "mmux-core-c-make-sint64",
    .implementation	= mmux_emacs_core_make_sint64,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sint64'.",
  },
  {
    .name		= "mmux-core-c-make-uintmax",
    .implementation	= mmux_emacs_core_make_uintmax,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-uintmax'.",
  },
  {
    .name		= "mmux-core-c-make-sintmax",
    .implementation	= mmux_emacs_core_make_sintmax,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-sintmax'.",
  },
  {
    .name		= "mmux-core-c-make-usize",
    .implementation	= mmux_emacs_core_make_usize,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-usize'.",
  },
  {
    .name		= "mmux-core-c-make-ssize",
    .implementation	= mmux_emacs_core_make_ssize,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-ssize'.",
  },
  {
    .name		= "mmux-core-c-make-ptrdiff",
    .implementation	= mmux_emacs_core_make_ptrdiff,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-ptrdiff'.",
  },
  {
    .name		= "mmux-core-c-make-wchar",
    .implementation	= mmux_emacs_core_make_wchar,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-wchar'.",
  },
  {
    .name		= "mmux-core-c-make-float",
    .implementation	= mmux_emacs_core_make_float,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-float'.",
  },
  {
    .name		= "mmux-core-c-make-long-double",
    .implementation	= mmux_emacs_core_make_long_double,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "build and return a user-pointer object of type `cc-long-double'.",
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
