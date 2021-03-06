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
#include <string.h>


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: constructors and destructors.
 ** ----------------------------------------------------------------- */

mmec_intrep_bytevector_t *
mmec_new_intrep_bytevector (emacs_env * env, intmax_t number_of_slots, intmax_t slot_size, bool hold_signed_values)
{
  mmec_intrep_bytevector_t	* bv = NULL;

  mmec_clear_environment_error(env);

  if (slot_size <= 0) {
    mmec_error_bytevector_constructor_invalid_slot_size(env);
    return NULL;
  } else if ((mmec_most_positive_fixnum() / slot_size) < number_of_slots) {
    /* The number  of bytes overflows the  maximum integer representable by  an Emacs
       Lisp value of type "integer". */
    if (1) {
      fprintf(stderr, "%s: slot_size=%ld, number_of_slots=%ld, maximum_fixnum=%ld\n", __func__,
	      slot_size, number_of_slots, mmec_most_positive_fixnum());
    }
    mmec_error_bytevector_constructor_size_too_big(env);
    return NULL;
  } else {
    errno = 0;
    bv = (mmec_intrep_bytevector_t *)malloc(sizeof(mmec_intrep_bytevector_t));
    if (bv) {
      if (number_of_slots) {
	errno	= 0;
	bv->ptr	= calloc(number_of_slots, slot_size);
	if (NULL == bv->ptr) {
	  goto error_allocating_memory;
	}
      } else {
	bv->ptr	= NULL;
      }
      bv->number_of_slots		= number_of_slots;
      bv->slot_size		= slot_size;
      bv->hold_signed_values	= hold_signed_values;
      return bv;
    } else {
      goto error_allocating_memory;
    }
    return bv;
  }

 error_allocating_memory: {
    if (bv) {
      if (bv->ptr) {
	free(bv->ptr);
      }
      free(bv);
    }
    mmec_error_memory_allocation(env);
    return NULL;
  }
}

void
mmec_delete_intrep_bytevector (mmec_intrep_bytevector_t * bv)
{
  if (bv->ptr) {
    free(bv->ptr);
  }
  free(bv);
}

#undef  MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR
#define MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(TYPESTEM, HOLD_SIGNED_VALUES) \
  mmec_intrep_bytevector_t *						\
  mmec_new_ ## TYPESTEM ## _intrep_bytevector (emacs_env * env, intmax_t number_of_slots) \
  {									\
    return mmec_new_intrep_bytevector(env, number_of_slots, sizeof(mmec_clang_ ## TYPESTEM ## _t), HOLD_SIGNED_VALUES); \
  }

MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(char,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(schar,	true)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(uchar,	false)
MMEC_DEFINE_BYTEVECTOR_CONSTRUCTOR(wchar,	true)
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
    mmec_intrep_bytevector_t	* bv = mmec_new_intrep_bytevector(env, number_of_slots, slot_size, hold_signed_values);

    if (mmec_funcall_returned_with_success(env)) {
      return mmec_new_emacs_value_from_intrep_bytevector(env, bv);
    } else {
      return mmec_new_emacs_value_nil(env);
    }
  }
}


/** --------------------------------------------------------------------
 ** Copy constructors.
 ** ----------------------------------------------------------------- */

mmec_intrep_bytevector_t *
mmec_copy_intrep_bytevector (emacs_env * env, mmec_intrep_bytevector_t const * const bv_original)
{
  mmec_intrep_bytevector_t *	bv_copy;

  mmec_clear_environment_error(env);

  bv_copy = mmec_new_intrep_bytevector(env, bv_original->number_of_slots, bv_original->slot_size, bv_original->hold_signed_values);
  if (mmec_funcall_returned_with_success(env)) {
    uint8_t	*src = (uint8_t *)(bv_original->ptr);
    uint8_t	*dst = (uint8_t *)(bv_copy->ptr);

    memcpy(dst, src, bv_original->number_of_slots * bv_original->slot_size);
    return bv_copy;
  } else {
    return NULL;
  }
}

static emacs_value
Fmmec_copy_bytevector (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert(nargs == 1);
  mmec_intrep_bytevector_t const * const bv_original = mmec_get_intrep_bytevector_from_emacs_value(env, args[0]);
  mmec_intrep_bytevector_t * bv_copy;

  bv_copy = mmec_copy_intrep_bytevector(env, bv_original);
  if (mmec_funcall_returned_with_success(env)) {
    return mmec_new_emacs_value_from_intrep_bytevector(env, bv_copy);
  } else {
    return mmec_new_emacs_value_nil(env);
  }
}


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: inspection.
 ** ----------------------------------------------------------------- */

bool
mmec_bytevector_valid_slot_index (mmec_intrep_bytevector_t const * const bv, intmax_t const idx)
{
  return ((0 <= idx) && (idx < bv->number_of_slots))? true : false;
}

bool
mmec_bytevector_valid_start_slot_index (mmec_intrep_bytevector_t const * const bv, intmax_t const idx)
{
  return ((0 <= idx) && (idx <= bv->number_of_slots))? true : false;
}

bool
mmec_bytevector_valid_past_slot_index (mmec_intrep_bytevector_t const * const bv, intmax_t const idx)
{
  return ((0 <= idx) && (idx <= bv->number_of_slots))? true : false;
}

bool
mmec_bytevector_valid_start_and_past_slot_index (mmec_intrep_bytevector_t const * const bv, intmax_t const start, intmax_t const past)
{
  return ((mmec_bytevector_valid_start_slot_index(bv, start) &&
	   mmec_bytevector_valid_past_slot_index(bv, past) &&
	   (start <= past))? true : false);
}

bool
mmec_intrep_bytevector_is_empty (mmec_intrep_bytevector_t const * const bv)
{
  return ((0 == bv->number_of_slots)? true : false);
}

bool
mmec_intrep_bytevector_is_not_empty (mmec_intrep_bytevector_t const * const bv)
{
  return ((0 != bv->number_of_slots)? true : false);
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
 ** Bytevector user-pointer objects: comparison.
 ** ----------------------------------------------------------------- */

static int
mmec_intrep_bytevector_check_comparison_spans (mmec_intrep_bytevector_t const * const src, intmax_t const src_start, intmax_t const src_past,
					       mmec_intrep_bytevector_t const * const dst, intmax_t const dst_start, intmax_t const dst_past)
{
  assert(src->slot_size          == dst->slot_size);
  assert(src->hold_signed_values == dst->hold_signed_values);
  intmax_t	src_span = src_past - src_start;
  intmax_t	dst_span = dst_past - dst_start;

  if (src_span == dst_span) {
    return 0;
  } else if (src_span > dst_span) {
    return +1;
  } else {
    assert(src_span < dst_span);
    return -1;
  }
}

static emacs_value
mmec_execute_bytevector_compare (emacs_env *env, ptrdiff_t nargs, emacs_value args[], mmec_intrep_bytevector_compare_fun_t * compare)
{
  assert(6 == nargs);
  mmec_intrep_bytevector_t	*bv1	= mmec_get_intrep_bytevector_from_emacs_value(env, args[0]);
  intmax_t			start1	= mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  intmax_t			past1	= mmec_extract_elisp_integer_from_emacs_value(env, args[2]);
  mmec_intrep_bytevector_t	*bv2	= mmec_get_intrep_bytevector_from_emacs_value(env, args[3]);
  intmax_t			start2	= mmec_extract_elisp_integer_from_emacs_value(env, args[4]);
  intmax_t			past2	= mmec_extract_elisp_integer_from_emacs_value(env, args[5]);

  if (mmec_bytevector_valid_start_and_past_slot_index(bv1, start1, past1) &&
      mmec_bytevector_valid_start_and_past_slot_index(bv2, start2, past2)) {
    return mmec_new_emacs_value_integer(env, compare(bv1, start1, past1, bv2, start2, past2));
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
mmec_execute_bytevector_comparison (emacs_env *env, ptrdiff_t nargs, emacs_value args[], mmec_intrep_bytevector_comparison_fun_t * comparison)
{
  assert(6 == nargs);
  mmec_intrep_bytevector_t	*bv1	= mmec_get_intrep_bytevector_from_emacs_value(env, args[0]);
  intmax_t			start1	= mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  intmax_t			past1	= mmec_extract_elisp_integer_from_emacs_value(env, args[2]);
  mmec_intrep_bytevector_t	*bv2	= mmec_get_intrep_bytevector_from_emacs_value(env, args[3]);
  intmax_t			start2	= mmec_extract_elisp_integer_from_emacs_value(env, args[4]);
  intmax_t			past2	= mmec_extract_elisp_integer_from_emacs_value(env, args[5]);

  if (mmec_bytevector_valid_start_and_past_slot_index(bv1, start1, past1) &&
      mmec_bytevector_valid_start_and_past_slot_index(bv2, start2, past2)) {
    return mmec_new_emacs_value_boolean(env, comparison(bv1, start1, past1, bv2, start2, past2));
  } else {
    if (0) {
      fprintf(stderr, "%s: here %ld, %ld, %ld, %ld, %ld, %ld\n", __func__,
	      bv1->number_of_slots, bv2->number_of_slots, start1, past1, start1, past1);
    }
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

#undef  MMEC_ELISP_BYTEVECTOR_COMPARE_FUNCTION
#define MMEC_ELISP_BYTEVECTOR_COMPARE_FUNCTION(TYPESTEM)		\
  emacs_value								\
  Fmmec_c_ ## TYPESTEM ## _intrep_bytevector_compare			\
  (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    return mmec_execute_bytevector_compare(env, nargs, args, mmec_ ## TYPESTEM ## _intrep_bytevector_compare); \
  }

#undef  MMEC_ELISP_BYTEVECTOR_COMPARISON_FUNCTION
#define MMEC_ELISP_BYTEVECTOR_COMPARISON_FUNCTION(TYPESTEM, FUNCSTEM)	\
  emacs_value								\
  Fmmec_c_ ## TYPESTEM ## _intrep_bytevector_ ## FUNCSTEM			\
  (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED) \
  {									\
    return mmec_execute_bytevector_comparison(env, nargs, args, mmec_ ## TYPESTEM ## _intrep_bytevector_ ## FUNCSTEM); \
  }

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_BYTEVECTOR_COMPARISON
#define MMEC_DEFINE_BYTEVECTOR_COMPARISON(TYPESTEM)			\
  int									\
  mmec_ ## TYPESTEM ## _intrep_bytevector_compare			\
  (mmec_intrep_bytevector_t const * const src, intmax_t const src_start, intmax_t const src_past, \
   mmec_intrep_bytevector_t const * const dst, intmax_t const dst_start, intmax_t const dst_past) \
  {									\
    int rv = mmec_intrep_bytevector_check_comparison_spans(src, src_start, src_past, \
							   dst, dst_start, dst_past); \
    if (0 == rv) {							\
      MMEC_PC(mmec_clang_ ## TYPESTEM ## _t, src_ptr, src->ptr);	\
      MMEC_PC(mmec_clang_ ## TYPESTEM ## _t, dst_ptr, dst->ptr);	\
									\
      for (intmax_t i=src_start, j=dst_start; i<src_past; ++i, ++j) {	\
	if (0) {							\
	  fprintf(stderr, "%s: src[%ld]=%ld, dst[%ld]=%ld\n", __func__,	\
		  i, (long)(src_ptr[i]),				\
		  j, (long)(dst_ptr[j]));				\
	}								\
	if (src_ptr[i] > dst_ptr[j]) {					\
	  return +1;							\
	} else if (src_ptr[i] < dst_ptr[j]) {				\
	  return -1;							\
	}								\
      }									\
      return 0;								\
    } else {								\
      return rv;							\
    }									\
  }									\
									\
  bool									\
  mmec_ ## TYPESTEM ## _intrep_bytevector_equal				\
  (mmec_intrep_bytevector_t const * const src, intmax_t const src_start, intmax_t const src_past, \
   mmec_intrep_bytevector_t const * const dst, intmax_t const dst_start, intmax_t const dst_past) \
  {									\
    return ((0 == mmec_ ## TYPESTEM ## _intrep_bytevector_compare(src, src_start, src_past, dst, dst_start, dst_past))? true : false); \
  }									\
									\
  bool									\
  mmec_ ## TYPESTEM ## _intrep_bytevector_less				\
  (mmec_intrep_bytevector_t const * const src, intmax_t const src_start, intmax_t const src_past, \
   mmec_intrep_bytevector_t const * const dst, intmax_t const dst_start, intmax_t const dst_past) \
  {									\
    return ((-1 == mmec_ ## TYPESTEM ## _intrep_bytevector_compare(src, src_start, src_past, dst, dst_start, dst_past))? true : false); \
  }									\
									\
  bool									\
  mmec_ ## TYPESTEM ## _intrep_bytevector_greater			\
  (mmec_intrep_bytevector_t const * const src, intmax_t const src_start, intmax_t const src_past, \
   mmec_intrep_bytevector_t const * const dst, intmax_t const dst_start, intmax_t const dst_past) \
  {									\
    return ((+1 == mmec_ ## TYPESTEM ## _intrep_bytevector_compare(src, src_start, src_past, dst, dst_start, dst_past))? true : false); \
  }									\
									\
  bool									\
  mmec_ ## TYPESTEM ## _intrep_bytevector_leq				\
  (mmec_intrep_bytevector_t const * const src, intmax_t const src_start, intmax_t const src_past, \
   mmec_intrep_bytevector_t const * const dst, intmax_t const dst_start, intmax_t const dst_past) \
  {									\
    return ((+1 > mmec_ ## TYPESTEM ## _intrep_bytevector_compare(src, src_start, src_past, dst, dst_start, dst_past))? true : false); \
  }									\
									\
  bool									\
  mmec_ ## TYPESTEM ## _intrep_bytevector_geq				\
  (mmec_intrep_bytevector_t const * const src, intmax_t const src_start, intmax_t const src_past, \
   mmec_intrep_bytevector_t const * const dst, intmax_t const dst_start, intmax_t const dst_past) \
  {									\
    return ((-1 < mmec_ ## TYPESTEM ## _intrep_bytevector_compare(src, src_start, src_past, dst, dst_start, dst_past))? true : false); \
  }									\
									\
  MMEC_ELISP_BYTEVECTOR_COMPARE_FUNCTION(TYPESTEM)			\
  MMEC_ELISP_BYTEVECTOR_COMPARISON_FUNCTION(TYPESTEM, equal)		\
  MMEC_ELISP_BYTEVECTOR_COMPARISON_FUNCTION(TYPESTEM, less)		\
  MMEC_ELISP_BYTEVECTOR_COMPARISON_FUNCTION(TYPESTEM, greater)		\
  MMEC_ELISP_BYTEVECTOR_COMPARISON_FUNCTION(TYPESTEM, leq)		\
  MMEC_ELISP_BYTEVECTOR_COMPARISON_FUNCTION(TYPESTEM, geq)

/* ------------------------------------------------------------------ */

MMEC_DEFINE_BYTEVECTOR_COMPARISON(char)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(schar)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(uchar)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(wchar)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(sshrt)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(ushrt)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(sint)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(uint)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(slong)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(ulong)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(sllong)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(ullong)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(sintmax)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(uintmax)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(ssize)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(usize)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(ptrdiff)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(sint8)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(uint8)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(sint16)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(uint16)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(sint32)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(uint32)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(sint64)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(uint64)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(float)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(double)
MMEC_DEFINE_BYTEVECTOR_COMPARISON(ldouble)


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: subsequence.
 ** ----------------------------------------------------------------- */

mmec_intrep_bytevector_t *
mmec_new_intrep_bytevector_subsequence (emacs_env * env, mmec_intrep_bytevector_t const * const src,
					intmax_t const start, intmax_t const past)
{
  if (! mmec_bytevector_valid_start_and_past_slot_index(src, start, past)) {
    mmec_error_bytevector_index_out_of_range(env);
    return NULL;
  } else {
    mmec_intrep_bytevector_t	* dst = mmec_new_intrep_bytevector(env, past - start, src->slot_size, src->hold_signed_values);

    if (mmec_funcall_returned_with_error(env)) {
      /* Error building the new bytevector internal representation. */
      return NULL;
    } else if (mmec_intrep_bytevector_is_empty(dst)) {
      /* The  selected span  in the  source  bytevector is  empty.  No  need to  copy
	 anything. */
      return dst;
    } else {
      intmax_t	number_of_bytes = (past - start) * src->slot_size;

      memcpy(dst->ptr,
	     ((int8_t *)src->ptr) + (start * src->slot_size),
	     number_of_bytes);
      return dst;
    }
  }
}

static emacs_value
Fmmec_c_subbytevector (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * elisp_func_data MMEC_UNUSED)
{
  assert((2 == nargs) || (3 == nargs));
  mmec_intrep_bytevector_t	*src	= mmec_get_intrep_bytevector_from_emacs_value(env, args[0]);
  intmax_t			start	= mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  intmax_t			past;
  mmec_intrep_bytevector_t	*dst;

  if (2 == nargs) {
    past = src->number_of_slots;
    dst  = mmec_new_intrep_bytevector_subsequence(env, src, start, past);
    if (mmec_funcall_returned_with_success(env)) {
      return mmec_new_emacs_value_from_intrep_bytevector(env, dst);
    } else {
      return mmec_new_emacs_value_nil(env);
    }
  } else {
    past = mmec_extract_elisp_integer_from_emacs_value(env, args[2]);
    dst  = mmec_new_intrep_bytevector_subsequence(env, src, start, past);
    if (mmec_funcall_returned_with_success(env)) {
      return mmec_new_emacs_value_from_intrep_bytevector(env, dst);
    } else {
      return mmec_new_emacs_value_nil(env);
    }
  }
}


/** --------------------------------------------------------------------
 ** Elisp functions table: entries for bytevector comparison.
 ** ----------------------------------------------------------------- */

/* There are 6 entries for each type stem. */
#undef  MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES
#define MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(TYPESTEM)	\
  {									\
    .name		= "mmec-c-" #TYPESTEM "-bytevector-compare",	\
      .implementation	= Fmmec_c_ ## TYPESTEM ## _intrep_bytevector_compare,	\
      .min_arity		= 6,					\
      .max_arity		= 6,					\
      .documentation	= "Compare two bytevectors.",			\
      },								\
  {									\
    .name		= "mmec-c-" #TYPESTEM "-bytevector-equal",	\
      .implementation	= Fmmec_c_ ## TYPESTEM ## _intrep_bytevector_equal,	\
      .min_arity		= 6,					\
      .max_arity		= 6,					\
      .documentation	= "Compare two bytevectors.",			\
      },								\
  {									\
    .name		= "mmec-c-" #TYPESTEM "-bytevector-less",	\
      .implementation	= Fmmec_c_ ## TYPESTEM ## _intrep_bytevector_less,	\
      .min_arity		= 6,					\
      .max_arity		= 6,					\
      .documentation	= "Compare two bytevectors.",			\
      },								\
  {									\
    .name		= "mmec-c-" #TYPESTEM "-bytevector-greater",	\
      .implementation	= Fmmec_c_ ## TYPESTEM ## _intrep_bytevector_greater,	\
      .min_arity		= 6,					\
      .max_arity		= 6,					\
      .documentation	= "Compare two bytevectors.",			\
      },								\
  {									\
    .name		= "mmec-c-" #TYPESTEM "-bytevector-leq",	\
      .implementation	= Fmmec_c_ ## TYPESTEM ## _intrep_bytevector_leq,	\
      .min_arity		= 6,					\
      .max_arity		= 6,					\
      .documentation	= "Compare two bytevectors.",			\
      },								\
  {									\
    .name		= "mmec-c-" #TYPESTEM "-bytevector-geq",	\
      .implementation	= Fmmec_c_ ## TYPESTEM ## _intrep_bytevector_geq,	\
      .min_arity		= 6,					\
      .max_arity		= 6,					\
      .documentation	= "Compare two bytevectors.",			\
      },

/* There are 28 type stems.  In total 28 * 6 = 168 entries. */
#undef  MMEC_INTREP_BYTEVECTOR_COMPARISON_TABLE_ENTRIES
#define MMEC_BYTEVECTOR_COMPARISON_TABLE_ENTRIES			\
      MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(char)		\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(schar)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(uchar)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(wchar)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(sshrt)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(ushrt)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(sint)		\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(uint)		\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(slong)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(ulong)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(sllong)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(ullong)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(sintmax)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(uintmax)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(ssize)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(usize)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(ptrdiff)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(sint8)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(uint8)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(sint16)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(uint16)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(sint32)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(uint32)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(sint64)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(uint64)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(float)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(double)	\
	MMEC_BYTEVECTOR_COMPARISON_TYPESTEM_TABLE_ENTRIES(ldouble)


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_FUNCTIONS	(1+28+28+1 + 168 + 1)
static mmec_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  /* Constructors. */
  {
    .name		= "mmec-c-make-bytevector",
    .implementation	= Fmmec_make_bytevector,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Build and return a new bytevector user-pointer object."
  },
  {
    .name		= "mmec-c-copy-bytevector",
    .implementation	= Fmmec_copy_bytevector,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Copy a user-pointer object of type `bytevector'."
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

  /* ------------------------------------------------------------------ */

  MMEC_BYTEVECTOR_COMPARISON_TABLE_ENTRIES

  /* ------------------------------------------------------------------ */
  /* Bytevector operations. */
  {
    .name		= "mmec-c-subbytevector",
    .implementation	= Fmmec_c_subbytevector,
    .min_arity		= 2,
    .max_arity		= 3,
    .documentation	= "Build and return a new user-pointer object of type `bytevector' representing a subsequence of another `bytevector'.",
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
