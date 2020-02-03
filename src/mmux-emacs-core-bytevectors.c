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


/** --------------------------------------------------------------------
 ** Bytevector objects: setters.
 ** ----------------------------------------------------------------- */

static emacs_value
Fmmux_emacs_core_bytevector_u8_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(2 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);

  if (idx <= bv->number_of_slots) {
    return mmux_emacs_core_make_intmax(env, bv->ptr[idx]);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmux_emacs_core_bytevector_s8_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(2 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);

  if (idx <= bv->number_of_slots) {
    int8_t	*slots	= ((int8_t*)(bv->ptr));

    return mmux_emacs_core_make_intmax(env, slots[idx]);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmux_emacs_core_bytevector_u16_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(2 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);

  if (idx <= bv->number_of_slots) {
    uint16_t	*slots	= ((uint16_t*)(bv->ptr));

    return mmux_emacs_core_make_intmax(env, slots[idx]);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmux_emacs_core_bytevector_s16_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(2 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);

  if (idx <= bv->number_of_slots) {
    int16_t	*slots	= ((int16_t*)(bv->ptr));

    return mmux_emacs_core_make_intmax(env, slots[idx]);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmux_emacs_core_bytevector_u32_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(2 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);

  if (idx <= bv->number_of_slots) {
    uint32_t	*slots	= ((uint32_t*)(bv->ptr));

    return mmux_emacs_core_make_intmax(env, slots[idx]);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmux_emacs_core_bytevector_s32_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(2 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);

  if (idx <= bv->number_of_slots) {
    int32_t	*slots	= ((int32_t*)(bv->ptr));

    return mmux_emacs_core_make_intmax(env, slots[idx]);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmux_emacs_core_bytevector_u64_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(2 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);

  if (idx <= bv->number_of_slots) {
    uint64_t	*slots	= ((uint64_t*)(bv->ptr));

    return mmux_emacs_core_make_intmax(env, slots[idx]);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmux_emacs_core_bytevector_s64_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(2 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);

  if (idx <= bv->number_of_slots) {
    int64_t	*slots	= ((int64_t*)(bv->ptr));

    return mmux_emacs_core_make_intmax(env, slots[idx]);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}


/** --------------------------------------------------------------------
 ** Bytevector objects: getters.
 ** ----------------------------------------------------------------- */

static emacs_value
Fmmux_emacs_core_bytevector_u8_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(3 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);
  uint8_t			val	= mmux_emacs_core_get_uint8(env, args[2]);

  if (idx <= bv->number_of_slots) {
    bv->ptr[idx] = val;
    return mmux_emacs_core_make_nil(env);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmux_emacs_core_bytevector_s8_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(3 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);
  int8_t			val	= mmux_emacs_core_get_int8(env, args[2]);

  if (idx <= bv->number_of_slots) {
    int8_t	*slots	= ((int8_t*)(bv->ptr));

    slots[idx] = val;
    return mmux_emacs_core_make_nil(env);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmux_emacs_core_bytevector_u16_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(3 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);
  uint16_t			val	= mmux_emacs_core_get_uint16(env, args[2]);

  if (idx <= bv->number_of_slots) {
    uint16_t	*slots	= ((uint16_t*)(bv->ptr));

    slots[idx] = val;
    return mmux_emacs_core_make_nil(env);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmux_emacs_core_bytevector_s16_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(3 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);
  int16_t			val	= mmux_emacs_core_get_int16(env, args[2]);

  if (idx <= bv->number_of_slots) {
    int16_t	*slots	= ((int16_t*)(bv->ptr));

    slots[idx] = val;
    return mmux_emacs_core_make_nil(env);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmux_emacs_core_bytevector_u32_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(3 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);
  uint32_t			val	= mmux_emacs_core_get_uint32(env, args[2]);

  if (idx <= bv->number_of_slots) {
    uint16_t	*slots	= ((uint16_t*)(bv->ptr));

    slots[idx] = val;
    return mmux_emacs_core_make_nil(env);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmux_emacs_core_bytevector_s32_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(3 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);
  int32_t			val	= mmux_emacs_core_get_int32(env, args[2]);

  if (idx <= bv->number_of_slots) {
    int32_t	*slots	= ((int32_t*)(bv->ptr));

    slots[idx] = val;
    return mmux_emacs_core_make_nil(env);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmux_emacs_core_bytevector_u64_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(3 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);
  uint64_t			val	= mmux_emacs_core_get_uint64(env, args[2]);

  if (idx <= bv->number_of_slots) {
    uint64_t	*slots	= ((uint64_t*)(bv->ptr));

    slots[idx] = val;
    return mmux_emacs_core_make_nil(env);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}

static emacs_value
Fmmux_emacs_core_bytevector_s64_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(3 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);
  int64_t			val	= mmux_emacs_core_get_int64(env, args[2]);

  if (idx <= bv->number_of_slots) {
    int64_t	*slots	= ((int64_t*)(bv->ptr));

    slots[idx] = val;
    return mmux_emacs_core_make_nil(env);
  } else {
    return mmux_emacs_core_error_bytevector_index_out_of_range(env);
  }
}


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_FUNCTIONS	16
static mmux_emacs_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  /* Bytevector objects: getters.. */
  {
    .name		= "mmux-core-c-bytevector-u8-ref",
    .implementation	= Fmmux_emacs_core_bytevector_u8_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `uint8_t' from a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-s8-ref",
    .implementation	= Fmmux_emacs_core_bytevector_s8_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `int8_t' from a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-u16-ref",
    .implementation	= Fmmux_emacs_core_bytevector_u16_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `uint16_t' from a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-s16-ref",
    .implementation	= Fmmux_emacs_core_bytevector_s16_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `int16_t' from a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-u32-ref",
    .implementation	= Fmmux_emacs_core_bytevector_u32_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `uint32_t' from a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-s32-ref",
    .implementation	= Fmmux_emacs_core_bytevector_s32_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `int32_t' from a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-u64-ref",
    .implementation	= Fmmux_emacs_core_bytevector_u64_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `uint64_t' from a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-s64-ref",
    .implementation	= Fmmux_emacs_core_bytevector_s64_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an `int64_t' from a `cc-bytevector' object."
  },

  /* Bytevector objects: setters. */
  {
    .name		= "mmux-core-c-bytevector-u8-set!",
    .implementation	= Fmmux_emacs_core_bytevector_u8_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `uint8_t' into a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-s8-set!",
    .implementation	= Fmmux_emacs_core_bytevector_s8_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `int8_t' into a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-u16-set!",
    .implementation	= Fmmux_emacs_core_bytevector_u16_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `uint16_t' into a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-s16-set!",
    .implementation	= Fmmux_emacs_core_bytevector_s16_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `int16_t' into a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-u32-set!",
    .implementation	= Fmmux_emacs_core_bytevector_u32_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `uint32_t' into a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-s32-set!",
    .implementation	= Fmmux_emacs_core_bytevector_s32_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `int32_t' into a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-u64-set!",
    .implementation	= Fmmux_emacs_core_bytevector_u64_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `uint64_t' into a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-s64-set!",
    .implementation	= Fmmux_emacs_core_bytevector_s64_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an `int64_t' into a `cc-bytevector' object."
  },
};


/** --------------------------------------------------------------------
 ** Module initialisation.
 ** ----------------------------------------------------------------- */

void
mmux_emacs_core_bytevectors_init (emacs_env * env)
{
  mmux_emacs_define_functions_from_table(env, module_functions_table, NUMBER_OF_MODULE_FUNCTIONS, 0);
}

/* end of file */
