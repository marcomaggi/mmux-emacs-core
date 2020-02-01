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
  mmux_emacs_core_bytevector_t	* obj = (mmux_emacs_core_bytevector_t *)_obj;

  free(obj->ptr);
  free(obj);
}

static emacs_value
Fmmux_emacs_core_bytevector_cmake (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMUX_EMACS_CORE_UNUSED)
{
  size_t			len = mmux_emacs_core_get_size(env, args[0]);
  mmux_emacs_core_bytevector_t	* obj;

  errno = 0;
  obj   = malloc(sizeof(mmux_emacs_core_bytevector_t));
  if (obj) {
    errno	= 0;
    obj->ptr	= malloc(len * sizeof(uint8_t));
    if (obj->ptr) {
      obj->len	= len;
      return mmux_emacs_core_make_user_ptr(env, mmux_emacs_core_bytevector_finalizer, obj);
    } else
      goto memory_allocation_error;
  } else {
    goto memory_allocation_error;
  }

  return env->intern(env, "nil");

 memory_allocation_error:
  {
    char const		* errmsg = strerror(errno);
    emacs_value		Serrmsg = mmux_emacs_core_make_string(env, errmsg, strlen(errmsg));

    /* Signal an error,  then immediately return.  In the "elisp"  Info file: see the
       node "Standard Errors" for a list of  the standard error symbols; see the node
       "Error Symbols"  for methods to define  error symbols.  (Marco Maggi;  Jan 14,
       2020) */
    env->non_local_exit_signal(env, env->intern(env, "mmux-core-no-memory-error"), Serrmsg);
    return env->intern(env, "nil");
  }
}


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_FUNCTIONS	1
static mmux_emacs_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  {
    .name		= "mmux-core-c-bytevector-make",
    .implementation	= Fmmux_emacs_core_bytevector_cmake,
    .min_arity		= 1,
    .max_arity		= 1,
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
