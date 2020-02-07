/*
  Part of: MMUX Emacs Core
  Contents: helper functions to signal errors
  Date: Feb  3, 2020

  Abstract

	This module implements helper functions to signal errors.

	To signal  an error: we call  "env->non_local_exit_signal()" then immediately
	return.  In the "elisp" Info file: see  the node "Standard Errors" for a list
	of the  standard error symbols; see  the node "Error Symbols"  for methods to
	define error symbols.  (Marco Maggi; Jan 14, 2020)

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
 ** System errors.
 ** ----------------------------------------------------------------- */

emacs_value
mmux_emacs_core_error_memory_allocation (emacs_env * env)
{
  char const		* errmsg = strerror(errno);
  emacs_value		Serrmsg = mmux_emacs_core_make_string(env, errmsg, strlen(errmsg));

  env->non_local_exit_signal(env, env->intern(env, "mmux-core-no-memory-error"), Serrmsg);
  return env->intern(env, "nil");
}


/** --------------------------------------------------------------------
 ** Range errors.
 ** ----------------------------------------------------------------- */

emacs_value
mmux_emacs_core_error_bytevector_index_out_of_range (emacs_env * env)
{
  static char const	* errmsg	= "attempt to access a bytevector obect with an index out of range";
  emacs_value		Serrmsg		= mmux_emacs_core_make_string(env, errmsg, strlen(errmsg));

  env->non_local_exit_signal(env, env->intern(env, "mmux-core-bytevector-index-out-of-range"), Serrmsg);
  return env->intern(env, "nil");
}

emacs_value
mmux_emacs_core_error_signed_unsigned_integer_comparison (emacs_env * env)
{
  static char const	* errmsg	= "cannot compare a signed integer with an unsigned integer";
  emacs_value		Serrmsg		= mmux_emacs_core_make_string(env, errmsg, strlen(errmsg));

  env->non_local_exit_signal(env, env->intern(env, "mmux-core-signed/unsigned-integer-comparison"), Serrmsg);
  return env->intern(env, "nil");
}

/* end of file */
