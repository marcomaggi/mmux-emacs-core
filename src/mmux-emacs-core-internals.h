/*
  Part of: MMUX Emacs modules
  Contents: private header file, generic definitions
  Date: Feb  1, 2020

  Abstract

	This header file is for internal definitions.  It must be included by all the
	source files in  this package.  This file  is not specific for  a package: it
	contains  generic definitions  used by  all the  packages in  the MMUX  Emacs
	project.

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

#ifndef MMUX_EMACS_INTERNALS_H
#define MMUX_EMACS_INTERNALS_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <mmux-emacs-core.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>


/** --------------------------------------------------------------------
 ** Constants.
 ** ----------------------------------------------------------------- */

/* This is required by GNU Emacs' API. */
mmux_emacs_core_decl int  plugin_is_GPL_compatible;


/** --------------------------------------------------------------------
 ** Function prototypes.
 ** ----------------------------------------------------------------- */

mmux_emacs_core_decl int emacs_module_init (struct emacs_runtime *ert)
  __attribute__((__nonnull__(1)));

mmux_emacs_core_private_decl void mmux_emacs_core_user_ptr_objects_init (emacs_env * env)
  __attribute__((__nonnull__(1)));

mmux_emacs_core_private_decl void mmux_emacs_core_bytevectors_init (emacs_env * env)
  __attribute__((__nonnull__(1)));

/* ------------------------------------------------------------------ */

mmux_emacs_core_private_decl emacs_value mmux_emacs_core_error_memory_allocation (emacs_env * env)
  __attribute__((__nonnull__(1)));

mmux_emacs_core_private_decl emacs_value mmux_emacs_core_error_bytevector_index_out_of_range (emacs_env * env)
  __attribute__((__nonnull__(1)));


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* MMUX_EMACS_INTERNALS_H */

/* end of file */
