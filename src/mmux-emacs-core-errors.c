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
 ** Non-local exits.
 ** ----------------------------------------------------------------- */

bool
mmec_funcall_returned_with_success (emacs_env * env)
{
  return ((emacs_funcall_exit_return == env->non_local_exit_check(env))? true : false);
}

bool
mmec_funcall_returned_with_error (emacs_env * env)
{
  return ((emacs_funcall_exit_return != env->non_local_exit_check(env))? true : false);
}

bool
mmec_funcall_returned_with_signal (emacs_env * env)
{
  return ((emacs_funcall_exit_signal == env->non_local_exit_check(env))? true : false);
}

bool
mmec_funcall_returned_with_throw (emacs_env * env)
{
  return ((emacs_funcall_exit_throw == env->non_local_exit_check(env))? true : false);
}

void
mmec_clear_environment_error (emacs_env * env)
{
  env->non_local_exit_clear(env);
}


/** --------------------------------------------------------------------
 ** Base errors.
 ** ----------------------------------------------------------------- */

MMEC_DEFINE_ERROR_SIGNALLER(mmec, base, "mmec-error", "Error while executing a MMUX Emacs Core operation.")


/** --------------------------------------------------------------------
 ** Features.
 ** ----------------------------------------------------------------- */

MMEC_DEFINE_ERROR_SIGNALLER(mmec, unsupported_feature,
			    "mmec-error-unsupported-feature",
			    "The requested feature is not implemented.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, unimplemented_c_language_function,
			    "mmec-error-unimplemented-c-language-function",
			    "The requested C language function is not implemented by the underlying platform.")


/** --------------------------------------------------------------------
 ** Constructor errors.
 ** ----------------------------------------------------------------- */

MMEC_DEFINE_ERROR_SIGNALLER(mmec, constructor,
			    "mmec-error-constructor",
			    "An error occurred while constructing an object.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, memory_alloction,
			    "mmec-error-no-memory",
			    "Not enough memory available to allocate an object.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, instantiating_abstract_type,
			    "mmec-error-instantiating-abstract-type",
			    "An attempt was performed to instantiate an abstract data type.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, unsupported_init_type,
			    "mmec-error-unsupported-init-type",
			    "An argument given to an object constructor has an unsupported type.")

/* ------------------------------------------------------------------ */

MMEC_DEFINE_ERROR_SIGNALLER(mmec, bytevector_constructor,
			    "mmec-error-bytevector-constructor",
			    "An error occurred while building a bytevector object.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, bytevector_constructor_invalid_number_of_slots,
			    "mmec-error-bytevector-constructor-invalid-number-of-slots",
			    "An invalid number of slots was given as argument to a bytevector constructor.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, bytevector_constructor_invalid_slot_size,
			    "mmec-error-bytevector-constructor-invalid-slot-size",
			    "An invalid slot size was given as argument to a bytevector constructor.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, bytevector_constructor_size_too_big,
			    "mmec-error-bytevector-constructor-size-too-big",
			    "The bytevector size resulting from the given slot size and number of slots is too big.")


/** --------------------------------------------------------------------
 ** Range errors.
 ** ----------------------------------------------------------------- */

MMEC_DEFINE_ERROR_SIGNALLER(mmec, value_out_of_range,
			    "mmec-error-value-out-of-range",
			    "A numeric object is out of range.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, index_out_of_range,
			    "mmec-error-index-out-of-range",
			    "Attempt to access the internal represenation of an object with an index out of range.")

/* ------------------------------------------------------------------ */

MMEC_DEFINE_ERROR_SIGNALLER(mmec, error_bytevector_is_empty,
			    "mmec-error-bytevector-is-empty",
			    "Used to signal that a bytevector operand is empty.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, bytevector_index_out_of_range,
			    "mmec-error-bytevector-index-out-of-range",
			    "Attempt to access a bytevector obect with an index out of range.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, bytevector_span_start_out_of_range,
			    "mmec-error-bytevector-span-start-out-of-range",
			    "The start index selected as slots span beginning in a bytevector is out of range.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, bytevector_span_past_out_of_range,
			    "mmec-error-bytevector-span-past-out-of-range",
			    "The past index selected as slots span ending in a bytevector is out of range.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, bytevector_invalid_span_limits,
			    "mmec-error-bytevector-invalid-span-limits",
			    "The start and past indexes selected as slots span limits in a bytevector are invalid.")


/** --------------------------------------------------------------------
 ** Operations errors.
 ** ----------------------------------------------------------------- */

MMEC_DEFINE_ERROR_SIGNALLER(mmec, signed_unsigned_integer_comparison,
			    "mmec-error-signed/unsigned-integer-comparison",
			    "Cannot compare a signed integer with an unsigned integer.")


/** --------------------------------------------------------------------
 ** Mathematics errors.
 ** ----------------------------------------------------------------- */

MMEC_DEFINE_ERROR_SIGNALLER(mmec, mathematics,
			    "mmec-error-mathematics",
			    "Error while performing a mathematics operation.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, mathematics_overflow,
			    "mmec-error-mathematics-overflow",
			    "Overflow while performing a mathematics operation.")

MMEC_DEFINE_ERROR_SIGNALLER(mmec, mathematics_underflow,
			    "mmec-error-mathematics-underflow",
			    "Underflow while performing a mathematics operation.")

/* end of file */
