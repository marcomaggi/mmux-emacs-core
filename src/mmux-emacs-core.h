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

#define MMUX_EMACS_CORE_PC(POINTER_TYPE, POINTER_NAME, EXPRESSION)	\
  POINTER_TYPE * POINTER_NAME = (POINTER_TYPE *) (EXPRESSION)

/* Usage examples:
 *
 * MMUX_EMACS_CORE_DEFINE_ERROR_SIGNALER(memory_alloction,
 *                                       "mmec-error-no-memory-error",
 *                                       strerror(errno))
 *
 * MMUX_EMACS_CORE_DEFINE_ERROR_SIGNALER(instantiating_abstract_type,
 *                                       "mmec-error-instantiating-abstract-type",
 *                                       "An attempt was performed to instantiate an abstract data type.")
 *
 * Expand into a function definition.  The function signals an error and returns nil.
 */
#undef  MMUX_EMACS_CORE_DEFINE_ERROR_SIGNALER
#define MMUX_EMACS_CORE_DEFINE_ERROR_SIGNALER(NAME, SYMBOL, MESSAGE)	\
  emacs_value								\
  mmux_emacs_core_error_ ## NAME (emacs_env * env)			\
  {									\
    char const		* errmsg = MESSAGE;				\
    emacs_value		Serrmsg = mmux_emacs_core_make_string(env, errmsg, strlen(errmsg)); \
									\
    env->non_local_exit_signal(env, env->intern(env, SYMBOL), Serrmsg); \
    return env->intern(env, "nil");					\
  }


/** --------------------------------------------------------------------
 ** Error signaling functions.
 ** ----------------------------------------------------------------- */

mmux_emacs_core_decl emacs_value mmux_emacs_core_base (emacs_env * env)
  __attribute__((__nonnull__(1)));

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_error_constructor (emacs_env * env)
  __attribute__((__nonnull__(1)));

mmux_emacs_core_decl emacs_value mmux_emacs_core_error_memory_allocation (emacs_env * env)
  __attribute__((__nonnull__(1)));

mmux_emacs_core_decl emacs_value mmux_emacs_core_error_instantiating_abstract_type (emacs_env * env)
  __attribute__((__nonnull__(1)));

mmux_emacs_core_decl emacs_value mmux_emacs_core_error_unsupported_init_type (emacs_env * env)
  __attribute__((__nonnull__(1)));

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_error_value_out_of_range (emacs_env * env)
  __attribute__((__nonnull__(1)));

mmux_emacs_core_decl emacs_value mmux_emacs_core_error_index_out_of_range (emacs_env * env)
  __attribute__((__nonnull__(1)));

mmux_emacs_core_decl emacs_value mmux_emacs_core_error_bytevector_index_out_of_range (emacs_env * env)
  __attribute__((__nonnull__(1)));

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_error_signed_unsigned_integer_comparison (emacs_env * env)
  __attribute__((__nonnull__(1)));


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

/* These definitions are useful when composing the output of C language macros. */
typedef char			mmux_emacs_core_type_char_t;
typedef signed char		mmux_emacs_core_type_schar_t;
typedef unsigned char		mmux_emacs_core_type_uchar_t;
typedef signed   short int	mmux_emacs_core_type_sshrt_t;
typedef unsigned short int	mmux_emacs_core_type_ushrt_t;
typedef signed   int		mmux_emacs_core_type_sint_t;
typedef unsigned int		mmux_emacs_core_type_uint_t;
typedef signed   long int	mmux_emacs_core_type_slong_t;
typedef unsigned long int	mmux_emacs_core_type_ulong_t;
typedef signed   long long int	mmux_emacs_core_type_sllong_t;
typedef unsigned long long int	mmux_emacs_core_type_ullong_t;
typedef ssize_t			mmux_emacs_core_type_ssize_t;
typedef size_t			mmux_emacs_core_type_usize_t;
typedef intmax_t		mmux_emacs_core_type_sintmax_t;
typedef uintmax_t		mmux_emacs_core_type_uintmax_t;
typedef ptrdiff_t		mmux_emacs_core_type_ptrdiff_t;
typedef int8_t			mmux_emacs_core_type_sint8_t;
typedef uint8_t			mmux_emacs_core_type_uint8_t;
typedef int16_t			mmux_emacs_core_type_sint16_t;
typedef uint16_t		mmux_emacs_core_type_uint16_t;
typedef int32_t			mmux_emacs_core_type_sint32_t;
typedef uint32_t		mmux_emacs_core_type_uint32_t;
typedef int64_t			mmux_emacs_core_type_sint64_t;
typedef uint64_t		mmux_emacs_core_type_uint64_t;
typedef float			mmux_emacs_core_type_float_t;
typedef double			mmux_emacs_core_type_double_t;
typedef long double		mmux_emacs_core_type_long_double_t;

/* ------------------------------------------------------------------ */

typedef emacs_value mmux_emacs_core_type_maker_fun_t (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
						      void * data MMUX_EMACS_CORE_UNUSED);


/** --------------------------------------------------------------------
 ** Functions prototypes.
 ** ----------------------------------------------------------------- */

mmux_emacs_core_decl void
mmux_emacs_define_functions_from_table (emacs_env * env, mmux_emacs_module_function_t const * module_functions,
					int number_of_module_functions, int verbose)
  __attribute__((__nonnull__(1,2)));


/** --------------------------------------------------------------------
 ** MMUX Emacs generic inline functions: argument getters for Emacs built-int objects.
 ** ----------------------------------------------------------------- */

static inline void *
mmux_emacs_core_get_user_ptr (emacs_env * env, emacs_value arg)
{
  return env->get_user_ptr(env, arg);
}

static inline intmax_t
mmux_emacs_core_extract_integer (emacs_env * env, emacs_value arg)
{
  return env->extract_integer(env, arg);
}

static inline double
mmux_emacs_core_extract_double (emacs_env * env, emacs_value arg)
{
  return env->extract_float(env, arg);
}


/** --------------------------------------------------------------------
 ** MMUX Emacs generic inline functions: argument getters for custom objects.
 ** ----------------------------------------------------------------- */

static inline char
mmux_emacs_core_get_char (emacs_env * env, emacs_value arg)
{
  return ((char)(mmux_emacs_core_extract_integer(env, arg)));
}

static inline signed char
mmux_emacs_core_get_schar (emacs_env * env, emacs_value arg)
{
  return ((signed char)(mmux_emacs_core_extract_integer(env, arg)));
}

static inline unsigned char
mmux_emacs_core_get_uchar (emacs_env * env, emacs_value arg)
{
  return ((unsigned char)(mmux_emacs_core_extract_integer(env, arg)));
}

/* ------------------------------------------------------------------ */

static inline unsigned short int
mmux_emacs_core_get_ushrt (emacs_env * env, emacs_value arg)
{
  return ((unsigned short int)(mmux_emacs_core_extract_integer(env, arg)));
}

static inline signed short int
mmux_emacs_core_get_sshrt (emacs_env * env, emacs_value arg)
{
  return ((signed short int)(mmux_emacs_core_extract_integer(env, arg)));
}

/* ------------------------------------------------------------------ */

static inline uint8_t
mmux_emacs_core_get_uint8 (emacs_env * env, emacs_value arg)
{
  return ((uint8_t)(mmux_emacs_core_extract_integer(env, arg)));
}

static inline int8_t
mmux_emacs_core_get_sint8 (emacs_env * env, emacs_value arg)
{
  return ((int8_t)(mmux_emacs_core_extract_integer(env, arg)));
}

/* ------------------------------------------------------------------ */

static inline uint16_t
mmux_emacs_core_get_uint16 (emacs_env * env, emacs_value arg)
{
  return ((uint16_t)(mmux_emacs_core_extract_integer(env, arg)));
}

static inline int16_t
mmux_emacs_core_get_sint16 (emacs_env * env, emacs_value arg)
{
  return ((int16_t)(mmux_emacs_core_extract_integer(env, arg)));
}


/** --------------------------------------------------------------------
 ** MMUX Emacs generic inline functions: object makers for Emacs built-in objects.
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
mmux_emacs_core_make_integer (emacs_env * env, intmax_t arg)
{
  return env->make_integer(env, arg);
}

static inline emacs_value
mmux_emacs_core_make_double (emacs_env * env, double arg)
{
  return env->make_float(env, arg);
}

static inline emacs_value
mmux_emacs_core_make_string (emacs_env * env, char const * strptr, size_t strlen)
{
  return env->make_string(env, strptr, strlen);
}


/** --------------------------------------------------------------------
 ** MMUX Emacs generic inline functions: object makers for custom objects.
 ** ----------------------------------------------------------------- */

static inline emacs_value
mmux_emacs_core_make_char (emacs_env * env, char arg)
{
  return mmux_emacs_core_make_integer(env, (intmax_t)arg);
}

/* ------------------------------------------------------------------ */

static inline emacs_value
mmux_emacs_core_make_schar (emacs_env * env, signed char arg)
{
  return mmux_emacs_core_make_integer(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_uchar (emacs_env * env, unsigned char arg)
{
  return mmux_emacs_core_make_integer(env, (intmax_t)arg);
}

/* ------------------------------------------------------------------ */

static inline emacs_value
mmux_emacs_core_make_ushrt (emacs_env * env, unsigned short int arg)
{
  return mmux_emacs_core_make_integer(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_sshrt (emacs_env * env, signed short int arg)
{
  return mmux_emacs_core_make_integer(env, (intmax_t)arg);
}

/* ------------------------------------------------------------------ */

static inline emacs_value
mmux_emacs_core_make_uint8 (emacs_env * env, uint8_t arg)
{
  return mmux_emacs_core_make_integer(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_sint8 (emacs_env * env, int8_t arg)
{
  return mmux_emacs_core_make_integer(env, (intmax_t)arg);
}

/* ------------------------------------------------------------------ */

static inline emacs_value
mmux_emacs_core_make_uint16 (emacs_env * env, uint16_t arg)
{
  return mmux_emacs_core_make_integer(env, (intmax_t)arg);
}

static inline emacs_value
mmux_emacs_core_make_sint16 (emacs_env * env, int16_t arg)
{
  return mmux_emacs_core_make_integer(env, (intmax_t)arg);
}


/** --------------------------------------------------------------------
 ** User-pointer objects: exact integer objects, floating-point objects.
 ** ----------------------------------------------------------------- */

/* At the  time of this  writing: GNU Emacs version  26+ implements as  exact integer
   numbers as fixnum objects with "intmax_t" internal representation; but not all the
   range representable  by an "intmax_t" is  used, because some bits  are occupied by
   the type flags.

   We trust  such type to  correctly represent the C  language integers up  to 32-bit
   internal representations; for all the other types we need a user-pointer object to
   make available the whole type range.

   (Marco Maggi; Feb  4, 2020)
*/

typedef struct mmux_emacs_core_wchar_t		mmux_emacs_core_wchar_t;

typedef struct mmux_emacs_core_uint32_t		mmux_emacs_core_uint32_t;
typedef struct mmux_emacs_core_sint32_t		mmux_emacs_core_sint32_t;

typedef struct mmux_emacs_core_uint64_t		mmux_emacs_core_uint64_t;
typedef struct mmux_emacs_core_sint64_t		mmux_emacs_core_sint64_t;

typedef struct mmux_emacs_core_sint_t		mmux_emacs_core_sint_t;
typedef struct mmux_emacs_core_uint_t		mmux_emacs_core_uint_t;

typedef struct mmux_emacs_core_slong_t		mmux_emacs_core_slong_t;
typedef struct mmux_emacs_core_ulong_t		mmux_emacs_core_ulong_t;

typedef struct mmux_emacs_core_sllong_t		mmux_emacs_core_sllong_t;
typedef struct mmux_emacs_core_ullong_t		mmux_emacs_core_ullong_t;

typedef struct mmux_emacs_core_ssize_t		mmux_emacs_core_ssize_t;
typedef struct mmux_emacs_core_usize_t		mmux_emacs_core_usize_t;

typedef struct mmux_emacs_core_sintmax_t	mmux_emacs_core_sintmax_t;
typedef struct mmux_emacs_core_uintmax_t	mmux_emacs_core_uintmax_t;

typedef struct mmux_emacs_core_ptrdiff_t	mmux_emacs_core_ptrdiff_t;

typedef struct mmux_emacs_core_float_t		mmux_emacs_core_float_t;
typedef struct mmux_emacs_core_double_t		mmux_emacs_core_double_t;
typedef struct mmux_emacs_core_long_double_t	mmux_emacs_core_long_double_t;

/* ------------------------------------------------------------------ */

struct mmux_emacs_core_wchar_t		{ wchar_t			val; };

struct mmux_emacs_core_sint32_t		{ int32_t			val; };
struct mmux_emacs_core_uint32_t		{ uint32_t			val; };

struct mmux_emacs_core_sint64_t		{ int64_t			val; };
struct mmux_emacs_core_uint64_t		{ uint64_t			val; };

struct mmux_emacs_core_sint_t		{ int				val; };
struct mmux_emacs_core_uint_t		{ unsigned int			val; };

struct mmux_emacs_core_slong_t		{ signed long int		val; };
struct mmux_emacs_core_ulong_t		{ unsigned long int		val; };

struct mmux_emacs_core_sllong_t		{ signed long long int		val; };
struct mmux_emacs_core_ullong_t		{ unsigned long long int	val; };

struct mmux_emacs_core_ssize_t		{ ssize_t			val; };
struct mmux_emacs_core_usize_t		{ size_t			val; };

struct mmux_emacs_core_sintmax_t	{ intmax_t			val; };
struct mmux_emacs_core_uintmax_t	{ uintmax_t			val; };

struct mmux_emacs_core_ptrdiff_t	{ ptrdiff_t			val; };

struct mmux_emacs_core_float_t		{ float				val; };
struct mmux_emacs_core_double_t		{ double			val; };
struct mmux_emacs_core_long_double_t	{ long double			val; };

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_sint32 (emacs_env * env,  int32_t val);
mmux_emacs_core_decl emacs_value mmux_emacs_core_make_uint32 (emacs_env * env, uint32_t val);

static inline int32_t
mmux_emacs_core_get_sint32 (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_sint32_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}
static inline uint32_t
mmux_emacs_core_get_uint32 (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_uint32_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_sint64 (emacs_env * env,  int64_t val);
mmux_emacs_core_decl emacs_value mmux_emacs_core_make_uint64 (emacs_env * env, uint64_t val);

static inline int64_t
mmux_emacs_core_get_sint64 (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_sint64_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  if (0) { fprintf(stderr, "%s: %p %ld\n", __func__, (void*)arg, (long)(obj->val)); }
  return obj->val;
}
static inline uint64_t
mmux_emacs_core_get_uint64 (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_uint64_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_sint (emacs_env * env, signed   int val);
mmux_emacs_core_decl emacs_value mmux_emacs_core_make_uint (emacs_env * env, unsigned int val);

static inline int
mmux_emacs_core_get_sint (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_sint_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}
static inline unsigned int
mmux_emacs_core_get_uint (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_uint_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_slong (emacs_env * env, signed   long int val);
mmux_emacs_core_decl emacs_value mmux_emacs_core_make_ulong (emacs_env * env, unsigned long int val);

static inline long int
mmux_emacs_core_get_slong (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_slong_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}
static inline unsigned long int
mmux_emacs_core_get_ulong (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_ulong_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_sllong (emacs_env * env, signed   long long int val);
mmux_emacs_core_decl emacs_value mmux_emacs_core_make_ullong (emacs_env * env, unsigned long long int val);

static inline long long int
mmux_emacs_core_get_sllong (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_sllong_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}
static inline unsigned long long int
mmux_emacs_core_get_ullong (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_ullong_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_ssize (emacs_env * env, ssize_t val);
mmux_emacs_core_decl emacs_value mmux_emacs_core_make_usize (emacs_env * env,  size_t val);

static inline ssize_t
mmux_emacs_core_get_ssize (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_ssize_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}
static inline size_t
mmux_emacs_core_get_usize (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_usize_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_sintmax (emacs_env * env,  intmax_t val);
mmux_emacs_core_decl emacs_value mmux_emacs_core_make_uintmax (emacs_env * env, uintmax_t val);

static inline intmax_t
mmux_emacs_core_get_sintmax (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_sintmax_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}
static inline uintmax_t
mmux_emacs_core_get_uintmax (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_uintmax_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_ptrdiff (emacs_env * env, ptrdiff_t val);

static inline ptrdiff_t
mmux_emacs_core_get_ptrdiff (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_ptrdiff_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_wchar (emacs_env * env, wchar_t val);

static inline wchar_t
mmux_emacs_core_get_wchar (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_wchar_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_float (emacs_env * env, float val);

static inline float
mmux_emacs_core_get_float (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_float_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_double (emacs_env * env, double val);

static inline double
mmux_emacs_core_get_double (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_double_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}

/* ------------------------------------------------------------------ */

mmux_emacs_core_decl emacs_value mmux_emacs_core_make_long_double (emacs_env * env, long double val);

static inline long double
mmux_emacs_core_get_long_double (emacs_env * env, emacs_value arg)
{
  MMUX_EMACS_CORE_PC(mmux_emacs_core_long_double_t, obj, mmux_emacs_core_get_user_ptr(env, arg));

  return obj->val;
}


/** --------------------------------------------------------------------
 ** User-pointer objects: bytevectors.
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
