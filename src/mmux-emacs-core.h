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

#ifndef MMEC_H
#define MMEC_H 1


/** --------------------------------------------------------------------
 ** Preliminary definitions.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
extern "C" {
#endif

/* The macro MMEC_UNUSED indicates that a  function, function argument or variable may
   potentially be unused. Usage examples:

   static int unused_function (char arg) MMEC_UNUSED;
   int foo (char unused_argument MMEC_UNUSED);
   int unused_variable MMEC_UNUSED;
*/
#ifdef __GNUC__
#  define MMEC_UNUSED		__attribute__((__unused__))
#else
#  define MMEC_UNUSED		/* empty */
#endif

#ifndef __GNUC__
#  define __attribute__(...)	/* empty */
#endif

/* I found the following chunk on the Net.  (Marco Maggi; Sun Feb 26, 2012) */
#if defined _WIN32 || defined __CYGWIN__
#  ifdef BUILDING_DLL
#    ifdef __GNUC__
#      define mmec_decl		__attribute__((__dllexport__)) extern
#    else
#      define mmec_decl		__declspec(dllexport) extern
#    endif
#  else
#    ifdef __GNUC__
#      define mmec_decl		__attribute__((__dllimport__)) extern
#    else
#      define mmec_decl		__declspec(dllimport) extern
#    endif
#  endif
#  define mmec_private_decl	extern
#else
#  if __GNUC__ >= 4
#    define mmec_decl		__attribute__((__visibility__("default"))) extern
#    define mmec_private_decl	__attribute__((__visibility__("hidden")))  extern
#  else
#    define mmec_decl		extern
#    define mmec_private_decl	extern
#  endif
#endif


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <emacs-module.h>
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <unistd.h>
#include <mmux-emacs-core-constants.h>


/** --------------------------------------------------------------------
 ** Constants.
 ** ----------------------------------------------------------------- */


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

mmec_decl char const *	mmec_version_string		(void);
mmec_decl int		mmec_version_interface_current	(void);
mmec_decl int		mmec_version_interface_revision	(void);
mmec_decl int		mmec_version_interface_age	(void);


/** --------------------------------------------------------------------
 ** Preprocessor macros.
 ** ----------------------------------------------------------------- */

#undef  MMEC_ELISP_FUNCTION_UNUSED_ARGS
#define MMEC_ELISP_FUNCTION_UNUSED_ARGS \
  emacs_env *env, ptrdiff_t nargs MMEC_UNUSED, emacs_value args[] MMEC_UNUSED, void *data MMEC_UNUSED

#define MMEC_PC(POINTER_TYPE, POINTER_NAME, EXPRESSION)	\
  POINTER_TYPE * POINTER_NAME = (POINTER_TYPE *) (EXPRESSION)

#define MMEC_CAST(TYPE, NAME, EXPRESSION)	\
  TYPE NAME = (TYPE) (EXPRESSION)

/* Usage examples:
 *
 * MMEC_DEFINE_ERROR_SIGNALLER(mmec, memory_alloction,
 *                             "mmec-error-no-memory-error",
 *                             strerror(errno))
 *
 * MMEC_DEFINE_ERROR_SIGNALLER(mmec, instantiating_abstract_type,
 *                             "mmec-error-instantiating-abstract-type",
 *                             "An attempt was performed to instantiate an abstract data type.")
 *
 * Expand into a function definition.  The function signals an error and returns nil.
 */
#undef  MMEC_DEFINE_ERROR_SIGNALLER
#define MMEC_DEFINE_ERROR_SIGNALLER(PREFIX, NAME, SYMBOL, MESSAGE)	\
  emacs_value								\
  PREFIX ## _error_ ## NAME (emacs_env * env)				\
  {									\
    char const		* errmsg = MESSAGE;				\
    emacs_value		Serrmsg = mmec_new_emacs_value_string(env, errmsg, strlen(errmsg)); \
									\
    env->non_local_exit_signal(env, env->intern(env, SYMBOL), Serrmsg); \
    return env->intern(env, "nil");					\
  }


/** --------------------------------------------------------------------
 ** Error signaling functions.
 ** ----------------------------------------------------------------- */

typedef emacs_value mmec_error_signaller_fun_t (emacs_env * env);

#undef  MMEC_SIGNALLER_PROTOTYPE
#define MMEC_SIGNALLER_PROTOTYPE(PREFIX, FUNCNAME)	\
  mmec_decl mmec_error_signaller_fun_t PREFIX ## _error_ ## FUNCNAME __attribute__((__nonnull__(1)))

MMEC_SIGNALLER_PROTOTYPE(mmec, base);

/* ------------------------------------------------------------------ */

MMEC_SIGNALLER_PROTOTYPE(mmec, constructor);
MMEC_SIGNALLER_PROTOTYPE(mmec, memory_allocation);
MMEC_SIGNALLER_PROTOTYPE(mmec, instantiating_abstract_type);
MMEC_SIGNALLER_PROTOTYPE(mmec, unsupported_init_type);

/* ------------------------------------------------------------------ */

MMEC_SIGNALLER_PROTOTYPE(mmec, value_out_of_range);
MMEC_SIGNALLER_PROTOTYPE(mmec, index_out_of_range);
MMEC_SIGNALLER_PROTOTYPE(mmec, bytevector_index_out_of_range);

/* ------------------------------------------------------------------ */

MMEC_SIGNALLER_PROTOTYPE(mmec, signed_unsigned_integer_comparison);


/** --------------------------------------------------------------------
 ** MMUX Emacs generic type definitions.
 ** ----------------------------------------------------------------- */

/* These definitions are useful when composing the output of C language macros. */
typedef char			mmec_clang_char_t;
typedef signed char		mmec_clang_schar_t;
typedef unsigned char		mmec_clang_uchar_t;
typedef wchar_t			mmec_clang_wchar_t;
typedef signed   short int	mmec_clang_sshrt_t;
typedef unsigned short int	mmec_clang_ushrt_t;
typedef signed   int		mmec_clang_sint_t;
typedef unsigned int		mmec_clang_uint_t;
typedef signed   long int	mmec_clang_slong_t;
typedef unsigned long int	mmec_clang_ulong_t;
typedef signed   long long int	mmec_clang_sllong_t;
typedef unsigned long long int	mmec_clang_ullong_t;
typedef ssize_t			mmec_clang_ssize_t;
typedef size_t			mmec_clang_usize_t;
typedef intmax_t		mmec_clang_sintmax_t;
typedef uintmax_t		mmec_clang_uintmax_t;
typedef ptrdiff_t		mmec_clang_ptrdiff_t;
typedef int8_t			mmec_clang_sint8_t;
typedef uint8_t			mmec_clang_uint8_t;
typedef int16_t			mmec_clang_sint16_t;
typedef uint16_t		mmec_clang_uint16_t;
typedef int32_t			mmec_clang_sint32_t;
typedef uint32_t		mmec_clang_uint32_t;
typedef int64_t			mmec_clang_sint64_t;
typedef uint64_t		mmec_clang_uint64_t;
typedef float			mmec_clang_float_t;
typedef double			mmec_clang_double_t;
typedef long double		mmec_clang_ldouble_t;


/** --------------------------------------------------------------------
 ** Elisp functions definition.
 ** ----------------------------------------------------------------- */

typedef emacs_value mmec_elisp_function_implementation_t (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);
typedef struct mmec_module_function_t	mmec_module_function_t;

struct mmec_module_function_t {
  char const				* name;
  mmec_elisp_function_implementation_t	* implementation;
  ptrdiff_t				min_arity;
  ptrdiff_t				max_arity;
  char const				* documentation;
};

mmec_decl void
mmec_define_elisp_functions_from_table (emacs_env * env, mmec_module_function_t const * module_functions,
					int number_of_module_functions, int verbose)
  __attribute__((__nonnull__(1,2)));


/** --------------------------------------------------------------------
 ** User pointer objects definition.
 ** ----------------------------------------------------------------- */

typedef void mmec_usrptr_object_finalizer_func_t (void *);
typedef mmec_usrptr_object_finalizer_func_t *	mmec_usrptr_object_finalizer_t;


/** --------------------------------------------------------------------
 ** Handling Emacs built-in values.
 ** ----------------------------------------------------------------- */

static inline void *
mmec_get_usrptr_object_from_emacs_value (emacs_env * env, emacs_value arg)
{
  return env->get_user_ptr(env, arg);
}

static inline intmax_t
mmec_extract_elisp_integer_from_emacs_value (emacs_env * env, emacs_value arg)
{
  return env->extract_integer(env, arg);
}

static inline double
mmec_extract_elisp_float_from_emacs_value (emacs_env * env, emacs_value arg)
{
  return env->extract_float(env, arg);
}

/* ------------------------------------------------------------------ */

static inline emacs_value
mmec_new_emacs_value_from_usrptr_object (emacs_env * env, mmec_usrptr_object_finalizer_t finalizer, void * ptr)
{
  return env->make_user_ptr(env, finalizer, ptr);
}

static inline emacs_value
mmec_new_emacs_value_nil (emacs_env * env)
{
  return env->intern(env, "nil");
}

static inline emacs_value
mmec_new_emacs_value_true (emacs_env * env)
{
  return env->intern(env, "t");
}

static inline emacs_value
mmec_new_emacs_value_boolean (emacs_env * env, int val)
{
  return ((val)? mmec_new_emacs_value_true(env) : mmec_new_emacs_value_nil(env));
}

static inline emacs_value
mmec_new_emacs_value_integer (emacs_env * env, intmax_t val)
{
  return env->make_integer(env, val);
}

static inline emacs_value
mmec_new_emacs_value_float (emacs_env * env, double val)
{
  return env->make_float(env, val);
}

static inline emacs_value
mmec_new_emacs_value_string (emacs_env * env, char const * ptr, size_t len)
{
  return env->make_string(env, ptr, len);
}


/** --------------------------------------------------------------------
 ** Number objects having "integer" as internal representation.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP
#define MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP(STEM)			\
  typedef intmax_t		mmec_intrep_ ## STEM ## _t;		\
									\
  static inline mmec_intrep_ ## STEM ## _t				\
  mmec_extract_intrep_ ## STEM ## _from_emacs_value (emacs_env * env, emacs_value arg) \
  {									\
    return (mmec_intrep_ ## STEM ## _t)mmec_extract_elisp_integer_from_emacs_value(env, arg); \
  }									\
									\
  static inline mmec_clang_ ## STEM ## _t				\
  mmec_extract_clang_ ## STEM ## _from_intrep_ ## STEM (mmec_intrep_ ## STEM ## _t val) \
  {									\
    return (mmec_clang_ ## STEM ## _t)val;				\
  }									\
									\
  static inline mmec_clang_ ## STEM ## _t				\
  mmec_extract_clang_ ## STEM ## _from_emacs_value (emacs_env * env, emacs_value arg) \
  {									\
    return mmec_extract_clang_ ## STEM ## _from_intrep_ ## STEM(mmec_extract_intrep_ ## STEM ## _from_emacs_value(env, arg)); \
  }									\
									\
  static inline mmec_intrep_ ## STEM ## _t				\
  mmec_new_intrep_ ## STEM ## _from_clang_ ## STEM (mmec_clang_ ## STEM ## _t val) \
  {									\
    return (mmec_intrep_ ## STEM ## _t)val;				\
  }									\
									\
  static inline emacs_value						\
  mmec_new_emacs_value_from_intrep_ ## STEM (emacs_env * env, mmec_intrep_ ## STEM ## _t val) \
  {									\
    return mmec_new_emacs_value_integer(env, (intmax_t)val);		\
  }									\
									\
  static inline emacs_value						\
  mmec_new_emacs_value_from_clang_ ## STEM (emacs_env * env, mmec_clang_ ## STEM ## _t val) \
  {									\
    return mmec_new_emacs_value_from_intrep_ ## STEM(env, mmec_new_intrep_ ## STEM ## _from_clang_ ## STEM(val)); \
  }

/* ------------------------------------------------------------------ */

MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP(char)
MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP(schar)
MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP(uchar)
MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP(sshrt)
MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP(ushrt)
MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP(sint8)
MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP(uint8)
MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP(sint16)
MMEC_DEFINE_WRAPPER_TYPE_INTEGER_REP(uint16)


/** --------------------------------------------------------------------
 ** Number objects having "float" as internal representation.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_WRAPPER_TYPE_FLOAT_REP
#define MMEC_DEFINE_WRAPPER_TYPE_FLOAT_REP(STEM)			\
  typedef double		mmec_intrep_ ## STEM ## _t;		\
									\
  static inline mmec_intrep_ ## STEM ## _t				\
  mmec_extract_intrep_ ## STEM ## _from_emacs_value (emacs_env * env, emacs_value arg) \
  {									\
    return (mmec_intrep_ ## STEM ## _t)mmec_extract_elisp_float_from_emacs_value(env, arg); \
  }									\
									\
  static inline mmec_clang_ ## STEM ## _t				\
  mmec_extract_clang_ ## STEM ## _from_intrep_ ## STEM (mmec_intrep_ ## STEM ## _t val) \
  {									\
    return (mmec_clang_ ## STEM ## _t)val;				\
  }									\
									\
  static inline mmec_clang_ ## STEM ## _t				\
  mmec_extract_clang_ ## STEM ## _from_emacs_value (emacs_env * env, emacs_value arg) \
  {									\
    return mmec_extract_clang_ ## STEM ## _from_intrep_ ## STEM(mmec_extract_intrep_ ## STEM ## _from_emacs_value(env, arg)); \
  }									\
									\
  static inline mmec_intrep_ ## STEM ## _t				\
  mmec_new_intrep_ ## STEM ## _from_clang_ ## STEM (mmec_clang_ ## STEM ## _t val) \
  {									\
    return (mmec_intrep_ ## STEM ## _t)val;				\
  }									\
									\
  static inline emacs_value						\
  mmec_new_emacs_value_from_intrep_ ## STEM (emacs_env * env, mmec_intrep_ ## STEM ## _t val) \
  {									\
    return mmec_new_emacs_value_integer(env, (intmax_t)val);		\
  }									\
									\
  static inline emacs_value						\
  mmec_new_emacs_value_from_clang_ ## STEM (emacs_env * env, mmec_clang_ ## STEM ## _t val) \
  {									\
    return mmec_new_emacs_value_from_intrep_ ## STEM(env, mmec_new_intrep_ ## STEM ## _from_clang_ ## STEM(val)); \
  }

/* ------------------------------------------------------------------ */

MMEC_DEFINE_WRAPPER_TYPE_FLOAT_REP(double)


/** --------------------------------------------------------------------
 ** Number objects having user-pointer objects as internal representation.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP
#define MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(BSTEM)			\
  typedef struct mmec_intrep_ ## BSTEM ## _stru_t	mmec_intrep_ ## BSTEM ## _stru_t; \
  typedef struct mmec_intrep_ ## BSTEM ## _stru_t *	mmec_intrep_ ## BSTEM ## _t; \
									\
  struct mmec_intrep_ ## BSTEM ## _stru_t {				\
    mmec_clang_ ## BSTEM ## _t val;					\
  };									\
									\
  mmec_decl mmec_intrep_ ## BSTEM ## _t mmec_new_intrep_ ## BSTEM ## _from_clang_ ## BSTEM (mmec_clang_ ## BSTEM ## _t val); \
  mmec_decl emacs_value mmec_new_emacs_value_from_intrep_ ## BSTEM (emacs_env * env, mmec_intrep_ ## BSTEM ## _t irep); \
  mmec_decl emacs_value mmec_new_emacs_value_from_clang_ ## BSTEM (emacs_env * env, mmec_clang_ ## BSTEM ## _t val); \
  mmec_decl mmec_intrep_ ## BSTEM ## _t mmec_get_intrep_ ## BSTEM ## _from_emacs_value (emacs_env * env, emacs_value arg); \
  mmec_decl mmec_clang_ ## BSTEM ## _t mmec_extract_clang_ ## BSTEM ## _from_intrep_ ## BSTEM (mmec_intrep_ ## BSTEM ## _t irep); \
  mmec_decl mmec_clang_ ## BSTEM ## _t mmec_extract_clang_ ## BSTEM ## _from_emacs_value (emacs_env * env, emacs_value arg);

/* ------------------------------------------------------------------ */

MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(wchar)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(sint)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(uint)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(slong)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(ulong)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(sllong)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(ullong)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(sintmax)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(uintmax)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(ssize)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(usize)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(ptrdiff)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(sint32)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(uint32)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(sint64)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(uint64)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(float)
MMEC_DEFINE_WRAPPER_TYPE_USRPTR_REP(ldouble)


/** --------------------------------------------------------------------
 ** User-pointer objects: bytevectors.
 ** ----------------------------------------------------------------- */

typedef struct mmec_intrep_bytevector_t	mmec_intrep_bytevector_t;

struct mmec_intrep_bytevector_t {
  size_t	number_of_slots;
  size_t	slot_size;
  int		hold_signed_values;
  uint8_t	* ptr;
};

static inline mmec_intrep_bytevector_t *
mmec_extract_intrep_bytevector (emacs_env * env, emacs_value arg)
{
  return ((mmec_intrep_bytevector_t *)mmec_get_usrptr_object_from_emacs_value(env, arg));
}


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* MMEC_H */

/* end of file */
