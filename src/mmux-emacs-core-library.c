/*
  Part of: MMUX Emacs Core
  Contents: library functions
  Date: Feb  1, 2020

  Abstract

	This module implements library initialisation and version numbers inspection.

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

#include <string.h>
#include <stdint.h>
#include "mmux-emacs-core-internals.h"


/** --------------------------------------------------------------------
 ** Global and local variables.
 ** ----------------------------------------------------------------- */

/* This is required by GNU Emacs' API. */
int  plugin_is_GPL_compatible;

static intmax_t	most_positive_fixnum;
static intmax_t	most_negative_fixnum;


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

char const *
mmec_version_string (void)
{
  return mmux_emacs_core_VERSION_INTERFACE_STRING;
}
int
mmec_version_interface_current (void)
{
  return mmux_emacs_core_VERSION_INTERFACE_CURRENT;
}
int
mmec_version_interface_revision (void)
{
  return mmux_emacs_core_VERSION_INTERFACE_REVISION;
}
int
mmec_version_interface_age (void)
{
  return mmux_emacs_core_VERSION_INTERFACE_AGE;
}

/* ------------------------------------------------------------------ */

static emacs_value
Fmmec_version_string (MMEC_ELISP_FUNCTION_UNUSED_ARGS)
{
  char const *	str = mmec_version_string();

  return mmec_new_emacs_value_string(env, str, strlen(str));
}
static emacs_value
Fmmec_version_interface_current (MMEC_ELISP_FUNCTION_UNUSED_ARGS)
{
  int	N = mmec_version_interface_current();

  return mmec_new_emacs_value_integer(env, (intmax_t)N);
}
static emacs_value
Fmmec_version_interface_revision (MMEC_ELISP_FUNCTION_UNUSED_ARGS)
{
  int	N = mmec_version_interface_revision();

  return mmec_new_emacs_value_integer(env, (intmax_t)N);
}
static emacs_value
Fmmec_version_interface_age (MMEC_ELISP_FUNCTION_UNUSED_ARGS)
{
  int	N = mmec_version_interface_age();

  return mmec_new_emacs_value_integer(env, (intmax_t)N);
}


/** --------------------------------------------------------------------
 ** Special functions.
 ** ----------------------------------------------------------------- */

intmax_t
mmec_most_positive_fixnum (void)
{
  return most_positive_fixnum;
}

intmax_t
mmec_most_negative_fixnum (void)
{
  return most_negative_fixnum;
}


/** --------------------------------------------------------------------
 ** Table of elisp functions.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_FUNCTIONS	4
static mmec_module_function_t const module_functions_table[NUMBER_OF_MODULE_FUNCTIONS] = {
  {
    .name		= "mmec-version-string",
    .implementation	= Fmmec_version_string,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return the version string."
  },
  {
    .name		= "mmec-version-interface-current",
    .implementation	= Fmmec_version_interface_current,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return the interface version current number."
  },
  {
    .name		= "mmec-version-interface-revision",
    .implementation	= Fmmec_version_interface_revision,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return the interface version revision number."
  },
  {
    .name		= "mmec-version-interface-age",
    .implementation	= Fmmec_version_interface_age,
    .min_arity		= 0,
    .max_arity		= 0,
    .documentation	= "Return the interface version age number."
  }
};


/** --------------------------------------------------------------------
 ** Module initialisation.
 ** ----------------------------------------------------------------- */

void
mmec_define_elisp_functions_from_table (emacs_env * env, mmec_module_function_t const * module_functions,
					int number_of_module_functions, int verbose)
{
  emacs_value	Qdefalias = env->intern(env, "defalias");

  for (int i=0; i<number_of_module_functions; ++i) {
    emacs_value	args[2]	= {
      env->intern(env, module_functions[i].name),
      env->make_function(env,
			 module_functions[i].min_arity,
			 module_functions[i].max_arity,
			 module_functions[i].implementation,
			 module_functions[i].documentation,
			 NULL)
    };
    if (verbose) {
      fprintf(stderr, "%s: defining function for C %s\n", __func__, module_functions[i].name);
    }
    env->funcall(env, Qdefalias, 2, args);
  }
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  if (ert->size < (ptrdiff_t)sizeof(*ert)) {
    return 1;
  } else {
    emacs_env	*env = ert->get_environment(ert);

    if (env->size < (ptrdiff_t)sizeof(*env)) {
      return 2;
    } else {
      mmec_define_elisp_functions_from_table(env, module_functions_table, NUMBER_OF_MODULE_FUNCTIONS, 0);
      mmec_number_objects_init(env);
      mmec_number_constants_init(env);
      mmec_bytevector_objects_init(env);
      mmec_mathematics_init(env);

      {
	emacs_value	Qsymbol_value		= env->intern(env, "symbol-value");
	emacs_value	Qmost_positive_fixnum	= env->intern(env, "most-positive-fixnum");
	emacs_value	Qmost_negative_fixnum	= env->intern(env, "most-negative-fixnum");

	{
	  emacs_value	args[1] = { Qmost_positive_fixnum };
	  emacs_value	val	= env->funcall(env, Qsymbol_value, 1, args);

	  most_positive_fixnum = mmec_extract_elisp_integer_from_emacs_value(env, val);
	}
	{
	  emacs_value	args[1] = { Qmost_negative_fixnum };
	  emacs_value	val	= env->funcall(env, Qsymbol_value, 1, args);

	  most_negative_fixnum = mmec_extract_elisp_integer_from_emacs_value(env, val);
	}
      }

      return 0;
    }
  }
}

/* end of file */
