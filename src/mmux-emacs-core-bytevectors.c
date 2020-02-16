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
 ** User-pointer objects: bytevectors.
 ** ----------------------------------------------------------------- */

static void
mmec_bytevector_finalizer (void * _obj)
{
  MMEC_PC(mmec_intrep_bytevector_t, obj, _obj);

  free(obj->ptr);
  free(obj);
}

static emacs_value
Fmmec_make_bytevector (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(nargs == 3);
  size_t	number_of_slots		= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[0]);
  size_t	slot_size		= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  int		hold_signed_values	= (int)   mmec_extract_elisp_integer_from_emacs_value(env, args[2]);
  mmec_intrep_bytevector_t	* obj;

  errno = 0;
  obj   = (mmec_intrep_bytevector_t *)malloc(sizeof(mmec_intrep_bytevector_t));
  if (obj) {
    errno	= 0;
    obj->ptr	= (uint8_t *)calloc(number_of_slots, slot_size);
    if (obj->ptr) {
      obj->number_of_slots	= number_of_slots;
      obj->slot_size		= slot_size;
      obj->hold_signed_values	= hold_signed_values? 1 : 0;
      return mmec_new_emacs_value_from_usrptr_object(env, mmec_bytevector_finalizer, obj);
    } else {
      return mmec_error_memory_allocation(env);
    }
  } else {
    return mmec_error_memory_allocation(env);
  }
}


/** --------------------------------------------------------------------
 ** Bytevector objects: setters.
 ** ----------------------------------------------------------------- */

static emacs_value
Fmmec_bytevector_u8_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(2 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);

  if (idx <= bv->number_of_slots) {
    return mmec_new_emacs_value_from_clang_uint8(env, bv->ptr[idx]);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmec_bytevector_s8_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(2 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);

  if (idx <= bv->number_of_slots) {
    int8_t	*slots	= ((int8_t*)(bv->ptr));

    return mmec_new_emacs_value_from_clang_sint8(env, slots[idx]);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmec_bytevector_u16_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(2 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);

  if (idx <= bv->number_of_slots) {
    uint16_t	*slots	= ((uint16_t*)(bv->ptr));

    return mmec_new_emacs_value_from_clang_uint16(env, slots[idx]);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmec_bytevector_s16_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(2 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);

  if (idx <= bv->number_of_slots) {
    int16_t	*slots	= ((int16_t*)(bv->ptr));

    return mmec_new_emacs_value_from_clang_sint16(env, slots[idx]);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmec_bytevector_u32_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(2 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);

  if (idx <= bv->number_of_slots) {
    uint32_t	*slots	= ((uint32_t *)(bv->ptr));

    return mmec_new_emacs_value_from_clang_uint32(env, slots[idx]);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmec_bytevector_s32_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(2 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);

  if (idx <= bv->number_of_slots) {
    int32_t	*slots	= ((int32_t*)(bv->ptr));

    return mmec_new_emacs_value_from_clang_sint32(env, slots[idx]);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmec_bytevector_u64_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(2 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);

  if (idx <= bv->number_of_slots) {
    uint64_t	*slots	= ((uint64_t*)(bv->ptr));

    return mmec_new_emacs_value_from_clang_uint64(env, slots[idx]);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmec_bytevector_s64_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(2 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);

  if (idx <= bv->number_of_slots) {
    int64_t	*slots	= ((int64_t*)(bv->ptr));

    return mmec_new_emacs_value_from_clang_sint64(env, slots[idx]);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}


/** --------------------------------------------------------------------
 ** Bytevector objects: getters.
 ** ----------------------------------------------------------------- */

static emacs_value
Fmmec_bytevector_u8_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(3 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  uint8_t			val	= mmec_extract_clang_uint8_from_emacs_value(env, args[2]);

  if (idx <= bv->number_of_slots) {
    bv->ptr[idx] = val;
    return mmec_new_emacs_value_nil(env);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmec_bytevector_s8_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(3 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  int8_t			val	= mmec_extract_clang_sint8_from_emacs_value(env, args[2]);

  if (idx <= bv->number_of_slots) {
    int8_t	*slots	= ((int8_t*)(bv->ptr));

    slots[idx] = val;
    return mmec_new_emacs_value_nil(env);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmec_bytevector_u16_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(3 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  uint16_t			val	= mmec_extract_clang_uint16_from_emacs_value(env, args[2]);

  if (idx <= bv->number_of_slots) {
    uint16_t	*slots	= ((uint16_t*)(bv->ptr));

    slots[idx] = val;
    return mmec_new_emacs_value_nil(env);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmec_bytevector_s16_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(3 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  int16_t			val	= mmec_extract_clang_sint16_from_emacs_value(env, args[2]);

  if (idx <= bv->number_of_slots) {
    int16_t	*slots	= ((int16_t*)(bv->ptr));

    slots[idx] = val;
    return mmec_new_emacs_value_nil(env);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmec_bytevector_u32_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(3 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  uint32_t			val	= mmec_extract_clang_uint32_from_emacs_value(env, args[2]);

  if (idx <= bv->number_of_slots) {
    uint32_t	*slots	= ((uint32_t*)(bv->ptr));

    slots[idx] = val;
    return mmec_new_emacs_value_nil(env);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmec_bytevector_s32_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(3 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  int32_t			val	= mmec_extract_clang_sint32_from_emacs_value(env, args[2]);

  if (idx <= bv->number_of_slots) {
    int32_t	*slots	= ((int32_t*)(bv->ptr));

    slots[idx] = val;
    return mmec_new_emacs_value_nil(env);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmec_bytevector_u64_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(3 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  uint64_t			val	= mmec_extract_clang_uint64_from_emacs_value(env, args[2]);

  if (idx <= bv->number_of_slots) {
    uint64_t	*slots	= ((uint64_t*)(bv->ptr));

    slots[idx] = val;
    return mmec_new_emacs_value_nil(env);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmec_bytevector_s64_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED)
{
  assert(3 == nargs);
  mmec_intrep_bytevector_t	*bv	= mmec_extract_intrep_bytevector(env, args[0]);
  size_t			idx	= (size_t)mmec_extract_elisp_integer_from_emacs_value(env, args[1]);
  int64_t			val	= mmec_extract_clang_sint64_from_emacs_value(env, args[2]);

  if (idx <= bv->number_of_slots) {
    int64_t	*slots	= ((int64_t*)(bv->ptr));

    slots[idx] = val;
    return mmec_new_emacs_value_nil(env);
  } else {
    return mmec_error_bytevector_index_out_of_range(env);
  }
}


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_FUNCTIONS	17
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
    .name		= "mmec-c-bytevector-u8-ref",
    .implementation	= Fmmec_bytevector_u8_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `uint8_t' from a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-s8-ref",
    .implementation	= Fmmec_bytevector_s8_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `int8_t' from a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-u16-ref",
    .implementation	= Fmmec_bytevector_u16_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `uint16_t' from a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-s16-ref",
    .implementation	= Fmmec_bytevector_s16_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `int16_t' from a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-u32-ref",
    .implementation	= Fmmec_bytevector_u32_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `uint32_t' from a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-s32-ref",
    .implementation	= Fmmec_bytevector_s32_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `int32_t' from a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-u64-ref",
    .implementation	= Fmmec_bytevector_u64_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `uint64_t' from a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-s64-ref",
    .implementation	= Fmmec_bytevector_s64_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `int64_t' from a `mmec-bytevector' object."
  },

  /* Bytevector objects: setters. */
  {
    .name		= "mmec-c-bytevector-u8-set",
    .implementation	= Fmmec_bytevector_u8_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `uint8_t' into a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-s8-set",
    .implementation	= Fmmec_bytevector_s8_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `int8_t' into a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-u16-set",
    .implementation	= Fmmec_bytevector_u16_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `uint16_t' into a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-s16-set",
    .implementation	= Fmmec_bytevector_s16_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `int16_t' into a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-u32-set",
    .implementation	= Fmmec_bytevector_u32_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `uint32_t' into a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-s32-set",
    .implementation	= Fmmec_bytevector_s32_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `int32_t' into a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-u64-set",
    .implementation	= Fmmec_bytevector_u64_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `uint64_t' into a `mmec-bytevector' object."
  },
  {
    .name		= "mmec-c-bytevector-s64-set",
    .implementation	= Fmmec_bytevector_s64_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `int64_t' into a `mmec-bytevector' object."
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
