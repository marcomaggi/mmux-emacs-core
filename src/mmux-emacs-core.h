/*
  Part of: MMUX Emacs Core
  Contents: public header file
  Date: Feb  1, 2020

  Abstract

	This is the public  header file of the library, defining  the public API.  It
	must be included in all the code that uses the library.

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

#ifndef MMUX_EMACS_CORE_H
#define MMUX_EMACS_CORE_H 1


/** --------------------------------------------------------------------
 ** Preliminary definitions.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
extern "C" {
#endif

/* The macro MMUX_EMACS_CORE_UNUSED indicates that a  function, function argument or variable may
   potentially be unused. Usage examples:

   static int unused_function (char arg) MMUX_EMACS_CORE_UNUSED;
   int foo (char unused_argument MMUX_EMACS_CORE_UNUSED);
   int unused_variable MMUX_EMACS_CORE_UNUSED;
*/
#ifdef __GNUC__
#  define MMUX_EMACS_CORE_UNUSED		__attribute__((__unused__))
#else
#  define MMUX_EMACS_CORE_UNUSED		/* empty */
#endif

#ifndef __GNUC__
#  define __attribute__(...)	/* empty */
#endif

/* I found the following chunk on the Net.  (Marco Maggi; Sun Feb 26, 2012) */
#if defined _WIN32 || defined __CYGWIN__
#  ifdef BUILDING_DLL
#    ifdef __GNUC__
#      define mmux_emacs_core_decl		__attribute__((__dllexport__)) extern
#    else
#      define mmux_emacs_core_decl		__declspec(dllexport) extern
#    endif
#  else
#    ifdef __GNUC__
#      define mmux_emacs_core_decl		__attribute__((__dllimport__)) extern
#    else
#      define mmux_emacs_core_decl		__declspec(dllimport) extern
#    endif
#  endif
#  define mmux_emacs_core_private_decl	extern
#else
#  if __GNUC__ >= 4
#    define mmux_emacs_core_decl		__attribute__((__visibility__("default"))) extern
#    define mmux_emacs_core_private_decl	__attribute__((__visibility__("hidden")))  extern
#  else
#    define mmux_emacs_core_decl		extern
#    define mmux_emacs_core_private_decl	extern
#  endif
#endif


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <emacs-module.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>


/** --------------------------------------------------------------------
 ** Constants and preprocessor macros.
 ** ----------------------------------------------------------------- */

#define MMUX_EMACS_CORE_PC(POINTER_TYPE, POINTER_NAME, EXPRESSION)	\
  POINTER_TYPE * POINTER_NAME = (POINTER_TYPE *) (EXPRESSION)


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

mmux_emacs_core_decl char const *	mmux_emacs_core_version_string			(void);
mmux_emacs_core_decl int		mmux_emacs_core_version_interface_current	(void);
mmux_emacs_core_decl int		mmux_emacs_core_version_interface_revision	(void);
mmux_emacs_core_decl int		mmux_emacs_core_version_interface_age		(void);


/** --------------------------------------------------------------------
 ** Preprocessor macros.
 ** ----------------------------------------------------------------- */

#undef  MMUX_EMACS_IFACE_FUNCTION_UNUSED_ARGS
#define MMUX_EMACS_IFACE_FUNCTION_UNUSED_ARGS \
  emacs_env *env, ptrdiff_t nargs MMUX_EMACS_CORE_UNUSED, emacs_value args[] MMUX_EMACS_CORE_UNUSED, void *data MMUX_EMACS_CORE_UNUSED


/** --------------------------------------------------------------------
 ** MMUX Emacs generic type definitions.
 ** ----------------------------------------------------------------- */

typedef emacs_value mmux_emacs_function_implementation_t (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);
typedef void mmux_emacs_finalizer_func_t (void *);
typedef mmux_emacs_finalizer_func_t *	mmux_emacs_finalizer_t;

typedef struct mmux_emacs_module_function_t	mmux_emacs_module_function_t;

struct mmux_emacs_module_function_t {
  char const				* name;
  mmux_emacs_function_implementation_t	* implementation;
  ptrdiff_t				min_arity;
  ptrdiff_t				max_arity;
  char const				* documentation;
};

/* ------------------------------------------------------------------ */

typedef signed   int		mmux_sint_t;
typedef unsigned int		mmux_uint_t;
typedef signed   long int	mmux_slint_t;
typedef unsigned long int	mmux_ulint_t;


/** --------------------------------------------------------------------
 ** Functions prototypes.
 ** ----------------------------------------------------------------- */

mmux_emacs_core_decl void
mmux_emacs_define_functions_from_table (emacs_env * env, mmux_emacs_module_function_t const * module_functions,
					int number_of_module_functions, int verbose)
  __attribute__((__nonnull__(1,2)));


/** --------------------------------------------------------------------
 ** MMUX Emacs generic inline functions: argument getters.
 ** ----------------------------------------------------------------- */

static inline void *
mmux_emacs_core_get_user_ptr (emacs_env * env, emacs_value arg)
{
  return env->get_user_ptr(env, arg);
}

static inline intmax_t
mmux_emacs_core_get_intmax (emacs_env * env, emacs_value arg)
{
  return env->extract_integer(env, arg);
}

static inline double
mmux_emacs_core_get_double (emacs_env * env, emacs_value arg)
{
  return env->extract_float(env, arg);
}

static inline mmux_ulint_t
mmux_emacs_core_get_ulint (emacs_env * env, emacs_value arg)
{
  return ((mmux_ulint_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline mmux_slint_t
mmux_emacs_core_get_slint (emacs_env * env, emacs_value arg)
{
  return ((mmux_slint_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline mmux_ulint_t
mmux_emacs_core_get_uint (emacs_env * env, emacs_value arg)
{
  return ((mmux_uint_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline mmux_sint_t
mmux_emacs_core_get_sint (emacs_env * env, emacs_value arg)
{
  return ((mmux_sint_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline size_t
mmux_emacs_core_get_size (emacs_env * env, emacs_value arg)
{
  return ((size_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline ssize_t
mmux_emacs_core_get_ssize (emacs_env * env, emacs_value arg)
{
  return ((ssize_t)(mmux_emacs_core_get_intmax(env, arg)));
}

/* ------------------------------------------------------------------ */

static inline uint8_t
mmux_emacs_core_get_uint8 (emacs_env * env, emacs_value arg)
{
  return ((uint8_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline uint16_t
mmux_emacs_core_get_uint16 (emacs_env * env, emacs_value arg)
{
  return ((uint16_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline uint32_t
mmux_emacs_core_get_uint32 (emacs_env * env, emacs_value arg)
{
  return ((uint32_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline uint64_t
mmux_emacs_core_get_uint64 (emacs_env * env, emacs_value arg)
{
  return ((uint64_t)(mmux_emacs_core_get_intmax(env, arg)));
}

/* ------------------------------------------------------------------ */

static inline int8_t
mmux_emacs_core_get_int8 (emacs_env * env, emacs_value arg)
{
  return ((int8_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline int16_t
mmux_emacs_core_get_int16 (emacs_env * env, emacs_value arg)
{
  return ((int16_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline int32_t
mmux_emacs_core_get_int32 (emacs_env * env, emacs_value arg)
{
  return ((int32_t)(mmux_emacs_core_get_intmax(env, arg)));
}

static inline int64_t
mmux_emacs_core_get_int64 (emacs_env * env, emacs_value arg)
{
  return ((int64_t)(mmux_emacs_core_get_intmax(env, arg)));
}


/** --------------------------------------------------------------------
 ** MMUX Emacs generic inline functions: object makers.
 ** ----------------------------------------------------------------- */

static inline emacs_value
mmux_emacs_core_make_user_ptr (emacs_env * env, mmux_emacs_finalizer_t finalizer, void * ptr)
{
  return env->make_user_ptr(env, finalizer, ptr);
}

static inline emacs_value
mmux_emacs_core_make_nil (emacs_env * env)
{
  return env->intern(env, "nil");
}

static inline emacs_value
mmux_emacs_core_make_true (emacs_env * env)
{
  return env->intern(env, "t");
}

static inline emacs_value
mmux_emacs_core_make_boolean (emacs_env * env, int val)
{
  return ((val)? mmux_emacs_core_make_true(env) : mmux_emacs_core_make_nil(env));
}

static inline emacs_value
mmux_emacs_core_make_intmax (emacs_env * env, intmax_t arg)
{
  return env->make_integer(env, arg);
}

static inline emacs_value
mmux_emacs_core_make_double (emacs_env * env, double arg)
{
  return env->make_float(env, arg);
}

static inline emacs_value
mmux_emacs_core_make_uint (emacs_env * env, mmux_uint_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_sint (emacs_env * env, mmux_sint_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_ulint (emacs_env * env, mmux_ulint_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_slint (emacs_env * env, mmux_slint_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_string (emacs_env * env, char const * strptr, size_t strlen)
{
  return env->make_string(env, strptr, strlen);
}

/* ------------------------------------------------------------------ */

static inline emacs_value
mmux_emacs_core_make_uint8 (emacs_env * env, uint8_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_uint16 (emacs_env * env, uint16_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_uint32 (emacs_env * env, uint32_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_uint64 (emacs_env * env, uint64_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

/* ------------------------------------------------------------------ */

static inline emacs_value
mmux_emacs_core_make_int8 (emacs_env * env, int8_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_int16 (emacs_env * env, int16_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_int32 (emacs_env * env, int32_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_int64 (emacs_env * env, int64_t arg)
{
  return mmux_emacs_core_make_intmax(env, (intmax_t)arg);
}


/** --------------------------------------------------------------------
 ** Bytevector user pointer objects.
 ** ----------------------------------------------------------------- */

typedef struct mmux_emacs_core_bytevector_t	mmux_emacs_core_bytevector_t;

struct mmux_emacs_core_bytevector_t {
  size_t	number_of_slots;
  size_t	slot_size;
  int		hold_signed_values;
  uint8_t	* ptr;
};

static inline mmux_emacs_core_bytevector_t *
mmux_emacs_core_get_bytevector (emacs_env * env, emacs_value arg)
{
  return ((mmux_emacs_core_bytevector_t *)mmux_emacs_core_get_user_ptr(env, arg));
}


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* MMUX_EMACS_CORE_H */

/* end of file */
