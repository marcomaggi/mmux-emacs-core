/*
  Part of: MMUX Emacs Core
  Contents: definitions of user pointer objects
  Date: Feb  1, 2020

  Abstract

	This module implements user pointer objects.

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
 ** Bytevector object functions.
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
  size_t	number_of_slots		= mmux_emacs_core_get_size(env, args[0]);
  size_t	slot_size		= mmux_emacs_core_get_size(env, args[1]);
  int		hold_signed_values	= mmux_emacs_core_get_sint(env, args[2]);
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

#define NUMBER_OF_MODULE_FUNCTIONS	1
static mmux_emacs_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  {
    .name		= "mmux-core-c-bytevector-make",
    .implementation	= Fmmux_emacs_core_make_bytevector,
    .min_arity		= 3,
    .max_arity		= 3,
    .documentation	= "Build and return a new bytevector user pointer object."
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
