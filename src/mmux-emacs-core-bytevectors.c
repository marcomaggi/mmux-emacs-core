/*
  Part of: MMUX Emacs Core
  Contents: definitions of user pointer objects
  Date: Feb  1, 2020

  Abstract

	This module implements bytevector object operations.

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
#include <errno.h>


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: constructors and destructors.
 ** ----------------------------------------------------------------- */

mmec_intrep_bytevector_t *
mmec_new_intrep_bytevector (intmax_t number_of_slots, intmax_t slot_size, bool hold_signed_values)
{
  mmec_intrep_bytevector_t	* obj;

  errno = 0;
  obj   = (mmec_intrep_bytevector_t *)malloc(sizeof(mmec_intrep_bytevector_t));
  if (obj) {
    errno	= 0;
    obj->ptr	= calloc(number_of_slots, slot_size);
    if (obj->ptr) {
      obj->number_of_slots	= number_of_slots;
      obj->slot_size		= slot_size;
      obj->hold_signed_values	= hold_signed_values;
      return obj;
    } else {
      free(obj);
      return NULL;
    }
  } else {
    return NULL;
  }
}

void
mmec_delete_intrep_bytevector (mmec_intrep_bytevector_t * bv)
{
  /* let's check the pointer just to be sure. */
  if (bv->ptr) {
    free(bv->ptr);
  }
  free(bv);
}

#undef  MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR
#define MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(TYPESTEM, HOLD_SIGNED_VALUES) \
  mmec_intrep_bytevector_t *						\
  mmec_make_ ## TYPESTEM ## _bytevector (intmax_t number_of_slots)	\
  {									\
    return mmec_new_intrep_bytevector(number_of_slots, sizeof(mmec_clang_ ## TYPESTEM ## _t), HOLD_SIGNED_VALUES); \
  }

MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(char,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(schar,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(uchar,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(wchar,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(sshrt,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(ushrt,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(sint,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(uint,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(slong,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(ulong,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(sllong,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(ullong,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(sintmax,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(uintmax,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(ssize,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(usize,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(ptrdiff,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(sint8,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(uint8,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(sint16,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(uint16,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(sint32,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(uint32,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(sint64,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(uint64,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(float,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(double,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(ldouble,	false)


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: Emacs value constructors and getters.
 ** ----------------------------------------------------------------- */

static void
mmec_bytevector_finalizer (void * obj)
{
  MMEC_PC(mmec_intrep_bytevector_t, bv, obj);

  mmec_delete_intrep_bytevector(bv);
}

emacs_value
mmec_new_emacs_value_from_intrep_bytevector (emacs_env * env, mmec_intrep_bytevector_t * bv)
{
  return mmec_new_emacs_value_from_usrptr_object(env, mmec_bytevector_finalizer, bv);
}

mmec_intrep_bytevector_t *
mmec_get_intrep_bytevector_from_emacs_value (emacs_env * env, emacs_value arg)
{
  return ((mmec_intrep_bytevector_t *)mmec_get_usrptr_object_from_emacs_value(env, arg));
}

static emacs_value
Fmmec_make_bytevector (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert(nargs == 3);
  intmax_t	number_of_slots		= mmec_extract_elisp_integer_from_emacs_value(env, args[0]);
  intmax_t	slot_size		= mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  bool		hold_signed_values	= mmec_extract_boolean_from_emacs_value(env, args[2]);

  if ((number_of_slots < 0) || (slot_size < 0)) {
    return mmec_error_constructor(env);
  } else {
    mmec_intrep_bytevector_t	* bv = mmec_new_intrep_bytevector(number_of_slots, slot_size, hold_signed_values);

    if (bv) {
      return mmec_new_emacs_value_from_intrep_bytevector(env, bv);
    } else {
      return mmec_error_memory_allocation(env);
    }
  }
}


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: Emacs value getters and setters.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_ELISP_BYTEVECTOR_GETTER
#define MMEC_DEFINE_ELISP_BYTEVECTOR_GETTER(TYPESTEM)			\
  static emacs_value							\
  Fmmec_bytevector_ ## TYPESTEM ## _ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(2 == nargs);							\
    mmec_intrep_bytevector_t	*bv	= mmec_get_intrep_bytevector_from_emacs_value(env, args[0]); \
    intmax_t			idx	= mmec_extract_elisp_integer_from_emacs_value(env, args[1]); \
									\
    if (mmec_bytevector_valid_slot_index(bv, idx)) {			\
      return mmec_new_emacs_value_from_clang_ ## TYPESTEM (env, mmec_bytevector_ ## TYPESTEM ## _ref(bv, idx)); \
    } else {								\
      return mmec_error_bytevector_index_out_of_range(env);		\
    }									\
  }

#undef  MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER
#define MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER(TYPESTEM)			\
  static emacs_value							\
  Fmmec_bytevector_ ## TYPESTEM ## _set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    assert(3 == nargs);							\
    mmec_intrep_bytevector_t	*bv	= mmec_get_intrep_bytevector_from_emacs_value(env, args[0]); \
    intmax_t			idx	= mmec_extract_elisp_integer_from_emacs_value(env, args[1]); \
									\
    if (mmec_bytevector_valid_slot_index(bv, idx)) {			\
      mmec_clang_ ## TYPESTEM ## _t val	= mmec_extract_clang_ ## TYPESTEM ##_from_emacs_value(env, args[2]); \
      mmec_bytevector_ ## TYPESTEM ## _set(bv, idx, val);		\
      return mmec_new_emacs_value_nil(env);				\
    } else {								\
      return mmec_error_bytevector_index_out_of_range(env);		\
    }									\
  }

#undef  MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER
#define MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(TYPESTEM)		\
  MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER(TYPESTEM)				\
  MMEC_DEFINE_ELISP_BYTEVECTOR_GETTER(TYPESTEM)

MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(char)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(schar)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(uchar)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(wchar)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(sshrt)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(ushrt)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(sint)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(uint)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(slong)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(ulong)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(sllong)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(ullong)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(sintmax)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(uintmax)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(ssize)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(usize)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(ptrdiff)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(sint8)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(uint8)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(sint16)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(uint16)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(sint32)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(uint32)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(sint64)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(uint64)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(float)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(double)
MMEC_DEFINE_ELISP_BYTEVECTOR_SETTER_GETTER(ldouble)


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: operations.
 ** ----------------------------------------------------------------- */

/* static emacs_value */
/* Fmmec_subbytevector (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) */
/* { */
/*   assert((2 == nargs) || (3 == nargs)); */
/*   mmec_intrep_bytevector_t	*bv	= mmec_get_intrep_bytevector_from_emacs_value(env, args[0]); */
/*   intmax_t			idx	= mmec_extract_elisp_integer_from_emacs_value(env, args[1]); */

/*   if (mmec_bytevector_valid_slot_index(bv, idx)) { */
/*     return mmec_new_emacs_value_from_clang_ (env, mmec_bytevector_ ## TYPESTEM ## _ref(bv, idx)); */
/*   } else { */
/*     return mmec_error_bytevector_index_out_of_range(env); */
/*   } */
/* } */


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_FUNCTIONS	(1+28+28)
static mmec_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  /* Constructors. */
  {
    .name		= "mmec-c-make-bytevector",
    .implementation	= Fmmec_make_bytevector,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Build and return a new bytevector user pointer object."
  },

  /* Bytevector objects: getters. */
  {
    .name		= "mmec-c-bytevector-char-ref",
    .implementation	= Fmmec_bytevector_char_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `char' from a `mmec-char-bytevector' object and return it as `mmec-char'."
  },
  {
    .name		= "mmec-c-bytevector-schar-ref",
    .implementation	= Fmmec_bytevector_schar_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `signed char' from a `mmec-schar-bytevector' object and return it as `mmec-schar'."
  },
  {
    .name		= "mmec-c-bytevector-uchar-ref",
    .implementation	= Fmmec_bytevector_uchar_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `unsigned char' from a `mmec-uchar-bytevector' object and return it as `mmec-uchar'."
  },
  {
    .name		= "mmec-c-bytevector-wchar-ref",
    .implementation	= Fmmec_bytevector_wchar_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `wchar_t' from a `mmec-wchar-bytevector' object and return it as `mmec-wchar'."
  },

  {
    .name		= "mmec-c-bytevector-sshrt-ref",
    .implementation	= Fmmec_bytevector_sshrt_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `signed short int' from a `mmec-sshrt-bytevector' object and return it as `mmec-sshrt'."
  },
  {
    .name		= "mmec-c-bytevector-ushrt-ref",
    .implementation	= Fmmec_bytevector_ushrt_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `unsigned short int' from a `mmec-ushrt-bytevector' object and return it as `mmec-ushrt'."
  },
  {
    .name		= "mmec-c-bytevector-sint-ref",
    .implementation	= Fmmec_bytevector_sint_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `signed int' from a `mmec-sint-bytevector' object and return it as `mmec-sint'."
  },
  {
    .name		= "mmec-c-bytevector-uint-ref",
    .implementation	= Fmmec_bytevector_uint_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `unsigned int' from a `mmec-uint-bytevector' object and return it as `mmec-uint'."
  },
  {
    .name		= "mmec-c-bytevector-slong-ref",
    .implementation	= Fmmec_bytevector_slong_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `signed long int' from a `mmec-slong-bytevector' object and return it as `mmec-slong'."
  },
  {
    .name		= "mmec-c-bytevector-ulong-ref",
    .implementation	= Fmmec_bytevector_ulong_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `unsigned long int' from a `mmec-ulong-bytevector' object and return it as `mmec-ulong'."
  },
  {
    .name		= "mmec-c-bytevector-sllong-ref",
    .implementation	= Fmmec_bytevector_sllong_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `signed long long int' from a `mmec-sllong-bytevector' object and return it as `mmec-sllong'."
  },
  {
    .name		= "mmec-c-bytevector-ullong-ref",
    .implementation	= Fmmec_bytevector_ullong_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `unsigned long long int' from a `mmec-ullong-bytevector' object and return it as `mmec-ullong'."
  },
  {
    .name		= "mmec-c-bytevector-sintmax-ref",
    .implementation	= Fmmec_bytevector_sintmax_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `intmax_t' from a `mmec-sintmax-bytevector' object and return it as `mmec-sintmax'."
  },
  {
    .name		= "mmec-c-bytevector-uintmax-ref",
    .implementation	= Fmmec_bytevector_uintmax_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `uintmax_t' from a `mmec-uintmax-bytevector' object and return it as `mmec-uintmax'."
  },
  {
    .name		= "mmec-c-bytevector-ssize-ref",
    .implementation	= Fmmec_bytevector_ssize_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `ssize_t' from a `mmec-ssize-bytevector' object and return it as `mmec-ssize'."
  },
  {
    .name		= "mmec-c-bytevector-usize-ref",
    .implementation	= Fmmec_bytevector_usize_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `size_t' from a `mmec-usize-bytevector' object and return it as `mmec-usize'."
  },
  {
    .name		= "mmec-c-bytevector-ptrdiff-ref",
    .implementation	= Fmmec_bytevector_ptrdiff_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `ptrdiff_t' from a `mmec-ptrdiff-bytevector' object and return it as `mmec-ptrdiff'."
  },

  {
    .name		= "mmec-c-bytevector-sint8-ref",
    .implementation	= Fmmec_bytevector_sint8_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `int8_t' from a `mmec-sint8-bytevector' object and return it as `mmec-sint8'."
  },
  {
    .name		= "mmec-c-bytevector-uint8-ref",
    .implementation	= Fmmec_bytevector_uint8_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `uint8_t' from a `mmec-uint8-bytevector' object and return it as `mmec-uint8'."
  },
  {
    .name		= "mmec-c-bytevector-sint16-ref",
    .implementation	= Fmmec_bytevector_sint16_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `int16_t' from a `mmec-sint16-bytevector' object and return it as `mmec-sint16'."
  },
  {
    .name		= "mmec-c-bytevector-uint16-ref",
    .implementation	= Fmmec_bytevector_uint16_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `uint16_t' from a `mmec-uint16-bytevector' object and return it as `mmec-uint16'."
  },
  {
    .name		= "mmec-c-bytevector-sint32-ref",
    .implementation	= Fmmec_bytevector_sint32_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `int32_t' from a `mmec-sint32-bytevector' object and return it as `mmec-sint32'."
  },
  {
    .name		= "mmec-c-bytevector-uint32-ref",
    .implementation	= Fmmec_bytevector_uint32_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `uint32_t' from a `mmec-uint32-bytevector' object and return it as `mmec-uint32'."
  },
  {
    .name		= "mmec-c-bytevector-sint64-ref",
    .implementation	= Fmmec_bytevector_sint64_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `int64_t' from a `mmec-sint64-bytevector' object and return it as `mmec-sint64'."
  },
  {
    .name		= "mmec-c-bytevector-uint64-ref",
    .implementation	= Fmmec_bytevector_uint64_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `uint64_t' from a `mmec-uint64-bytevector' object and return it as `mmec-uint64'."
  },

  {
    .name		= "mmec-c-bytevector-float-ref",
    .implementation	= Fmmec_bytevector_float_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `float' from a `mmec-float-bytevector' object and return it as `mmec-float'."
  },
  {
    .name		= "mmec-c-bytevector-double-ref",
    .implementation	= Fmmec_bytevector_double_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `double' from a `mmec-double-bytevector' object and return it as `mmec-double'."
  },
  {
    .name		= "mmec-c-bytevector-ldouble-ref",
    .implementation	= Fmmec_bytevector_ldouble_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract a value of type `ldouble' from a `mmec-ldouble-bytevector' object and return it as `mmec-ldouble'."
  },

  /* Bytevector objects: setters. */
  {
    .name		= "mmec-c-bytevector-char-set",
    .implementation	= Fmmec_bytevector_char_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `char' from a number of type `mmec-char' and store it into the binary array in an object of type `mmec-char-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-schar-set",
    .implementation	= Fmmec_bytevector_schar_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `signed char' from a number of type `mmec-schar' and store it into the binary array in an object of type `mmec-schar-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-uchar-set",
    .implementation	= Fmmec_bytevector_uchar_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `unsigned char' from a number of type `mmec-uchar' and store it into the binary array in an object of type `mmec-uchar-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-wchar-set",
    .implementation	= Fmmec_bytevector_wchar_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `wchar_t' from a number of type `mmec-wchar' and store it into the binary array in an object of type `mmec-wchar-bytevector'."
  },

  {
    .name		= "mmec-c-bytevector-sshrt-set",
    .implementation	= Fmmec_bytevector_sshrt_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `signed short int' from a number of type `mmec-sshrt' and store it into the binary array in an object of type `mmec-sshrt-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-ushrt-set",
    .implementation	= Fmmec_bytevector_ushrt_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `unsigned short int' from a number of type `mmec-ushrt' and store it into the binary array in an object of type `mmec-ushrt-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-sint-set",
    .implementation	= Fmmec_bytevector_sint_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `signed int' from a number of type `mmec-sint' and store it into the binary array in an object of type `mmec-sint-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-uint-set",
    .implementation	= Fmmec_bytevector_uint_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `unsigned int' from a number of type `mmec-uint' and store it into the binary array in an object of type `mmec-uint-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-slong-set",
    .implementation	= Fmmec_bytevector_slong_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `signed long int' from a number of type `mmec-slong' and store it into the binary array in an object of type `mmec-slong-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-ulong-set",
    .implementation	= Fmmec_bytevector_ulong_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `unsigned long int' from a number of type `mmec-ulong' and store it into the binary array in an object of type `mmec-ulong-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-sllong-set",
    .implementation	= Fmmec_bytevector_sllong_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `signed long long int' from a number of type `mmec-sllong' and store it into the binary array in an object of type `mmec-sllong-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-ullong-set",
    .implementation	= Fmmec_bytevector_ullong_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `unsigned long long int' from a number of type `mmec-ullong' and store it into the binary array in an object of type `mmec-ullong-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-sintmax-set",
    .implementation	= Fmmec_bytevector_sintmax_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `intmax_t' from a number of type `mmec-sintmax' and store it into the binary array in an object of type `mmec-sintmax-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-uintmax-set",
    .implementation	= Fmmec_bytevector_uintmax_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `uintmax_t' from a number of type `mmec-uintmax' and store it into the binary array in an object of type `mmec-uintmax-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-ssize-set",
    .implementation	= Fmmec_bytevector_ssize_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `ssize_t' from a number of type `mmec-ssize' and store it into the binary array in an object of type `mmec-ssize-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-usize-set",
    .implementation	= Fmmec_bytevector_usize_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `size_t' from a number of type `mmec-usize' and store it into the binary array in an object of type `mmec-usize-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-ptrdiff-set",
    .implementation	= Fmmec_bytevector_ptrdiff_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `ptrdiff_t' from a number of type `mmec-ptrdiff' and store it into the binary array in an object of type `mmec-ptrdiff-bytevector'."
  },

  {
    .name		= "mmec-c-bytevector-sint8-set",
    .implementation	= Fmmec_bytevector_sint8_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `int8_t' from a number of type `mmec-sint8' and store it into the binary array in an object of type `mmec-sint8-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-uint8-set",
    .implementation	= Fmmec_bytevector_uint8_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `uint8_t' from a number of type `mmec-uint8' and store it into the binary array in an object of type `mmec-uint8-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-sint16-set",
    .implementation	= Fmmec_bytevector_sint16_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `int16_t' from a number of type `mmec-sint16' and store it into the binary array in an object of type `mmec-sint16-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-uint16-set",
    .implementation	= Fmmec_bytevector_uint16_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `uint16_t' from a number of type `mmec-uint16' and store it into the binary array in an object of type `mmec-uint16-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-sint32-set",
    .implementation	= Fmmec_bytevector_sint32_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `int32_t' from a number of type `mmec-sint32' and store it into the binary array in an object of type `mmec-sint32-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-uint32-set",
    .implementation	= Fmmec_bytevector_uint32_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `uint32_t' from a number of type `mmec-uint32' and store it into the binary array in an object of type `mmec-uint32-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-sint64-set",
    .implementation	= Fmmec_bytevector_sint64_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `int64_t' from a number of type `mmec-sint64' and store it into the binary array in an object of type `mmec-sint64-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-uint64-set",
    .implementation	= Fmmec_bytevector_uint64_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `uint64_t' from a number of type `mmec-uint64' and store it into the binary array in an object of type `mmec-uint64-bytevector'."
  },

  {
    .name		= "mmec-c-bytevector-float-set",
    .implementation	= Fmmec_bytevector_float_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `float' from a number of type `mmec-float' and store it into the binary array in an object of type `mmec-float-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-double-set",
    .implementation	= Fmmec_bytevector_double_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `double' from a number of type `mmec-double' and store it into the binary array in an object of type `mmec-double-bytevector'."
  },
  {
    .name		= "mmec-c-bytevector-ldouble-set",
    .implementation	= Fmmec_bytevector_ldouble_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Extract a value of type `long double' from a number of type `mmec-ldouble' and store it into the binary array in an object of type `mmec-ldouble-bytevector'."
  },
};


/** --------------------------------------------------------------------
 ** Module initialisation.
 ** ----------------------------------------------------------------- */

void
mmec_bytevector_objects_init (emacs_env * env)
{
  mmec_define_elisp_functions_from_table(env, module_functions_table, NUMBER_OF_MODULE_FUNCTIONS, 0);
}

/* end of file */
