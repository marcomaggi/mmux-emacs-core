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
 ** Inspection.
 ** ----------------------------------------------------------------- */

static emacs_value
Fmmux_emacs_core_bytevector_length (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(1 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);

  return mmux_emacs_core_make_intmax(env, bv->len);
}


/** --------------------------------------------------------------------
 ** Setters and getters.
 ** ----------------------------------------------------------------- */

static emacs_value
Fmmux_emacs_core_bytevector_u8_ref (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(2 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);

  return mmux_emacs_core_make_intmax(env, bv->ptr[idx]);
}

static emacs_value
Fmmux_emacs_core_bytevector_u8_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  assert(3 == nargs);
  mmux_emacs_core_bytevector_t	*bv	= mmux_emacs_core_get_bytevector(env, args[0]);
  size_t			idx	= mmux_emacs_core_get_size(env, args[1]);
  uint8_t			octet	= mmux_emacs_core_get_uint8(env, args[2]);

  bv->ptr[idx] = octet;
  return mmux_emacs_core_make_nil(env);
}


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_FUNCTIONS	3
static mmux_emacs_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  /* Inspection. */
  {
    .name		= "mmux-core-c-bytevector-length",
    .implementation	= Fmmux_emacs_core_bytevector_length,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Return an exact integer representing the length of a `cc-bytevector' object."
  },

  /* Getters and setters. */
  {
    .name		= "mmux-core-c-bytevector-u8-ref",
    .implementation	= Fmmux_emacs_core_bytevector_u8_ref,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Extract an uint8 from a `cc-bytevector' object."
  },
  {
    .name		= "mmux-core-c-bytevector-u8-set!",
    .implementation	= Fmmux_emacs_core_bytevector_u8_set,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Set an uint8 into a `cc-bytevector' object."
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

