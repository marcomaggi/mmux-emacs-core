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
  emacs_env *env, ptrdiff_t nargs MMEC_UNUSED, emacs_value args[] MMEC_UNUSED, void *elisp_func_data MMEC_UNUSED

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

#undef  MMEC_ERROR_SIGNALLER_PROTOTYPE
#define MMEC_ERROR_SIGNALLER_PROTOTYPE(PREFIX, FUNCNAME)	\
  mmec_decl mmec_error_signaller_fun_t PREFIX ## _error_ ## FUNCNAME __attribute__((__nonnull__(1)))

MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, base);

/* ------------------------------------------------------------------ */

MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, constructor);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, memory_allocation);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, instantiating_abstract_type);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, unsupported_init_type);

MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, bytevector_constructor);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, bytevector_constructor_invalid_number_of_slots);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, bytevector_constructor_invalid_slot_size);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, bytevector_constructor_size_too_big);

/* ------------------------------------------------------------------ */

MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, value_out_of_range);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, index_out_of_range);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, bytevector_index_out_of_range);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, error_bytevector_is_empty);

/* ------------------------------------------------------------------ */

MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, signed_unsigned_integer_comparison);

/* ------------------------------------------------------------------ */

MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, mathematics);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, mathematics_overflow);
MMEC_ERROR_SIGNALLER_PROTOTYPE(mmec, mathematics_underflow);


/** --------------------------------------------------------------------
 ** Error detection functions.
 ** ----------------------------------------------------------------- */

typedef bool mmec_funcall_error_detection_fun_t (emacs_env * env);

mmec_decl mmec_funcall_error_detection_fun_t mmec_funcall_returned_with_success
  __attribute__((__nonnull__(1)));

mmec_decl mmec_funcall_error_detection_fun_t mmec_funcall_returned_with_error
  __attribute__((__nonnull__(1)));

mmec_decl mmec_funcall_error_detection_fun_t mmec_funcall_returned_with_signal
  __attribute__((__nonnull__(1)));

mmec_decl mmec_funcall_error_detection_fun_t mmec_funcall_returned_with_throw
  __attribute__((__nonnull__(1)));

mmec_decl void mmec_clear_environment_error (emacs_env * env)
  __attribute__((__nonnull__(1)));


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

static inline bool
mmec_extract_boolean_from_emacs_value (emacs_env * env, emacs_value arg)
{
  return (env->eq(env, arg, env->intern(env, "nil")))? false : true;
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
    return mmec_new_emacs_value_integer(env, val);			\
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
    return mmec_new_emacs_value_float(env, val);			\
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
 ** Bytevector user-pointer objects: type definitions.
 ** ----------------------------------------------------------------- */

typedef struct mmec_intrep_bytevector_t	mmec_intrep_bytevector_t;

struct mmec_intrep_bytevector_t {
  /* Non-negative number representing the number of slots in the bytevector. */
  intmax_t	number_of_slots;
  /* Strictly positive number representing the slot size. */
  intmax_t	slot_size;
  /* True  if  the numbers  stored  in  the slots  are  meant  to be  interpreted  as
     signed. */
  bool		hold_signed_values;
  /* Pointer to the data area.  Set to NULL if the number of slots is zero. */
  void		* ptr;
};

mmec_decl emacs_value mmec_new_emacs_value_from_intrep_bytevector (emacs_env * env, mmec_intrep_bytevector_t * bv)
  __attribute__((__nonnull__(1,2)));

mmec_decl mmec_intrep_bytevector_t * mmec_get_intrep_bytevector_from_emacs_value (emacs_env * env, emacs_value arg)
  __attribute__((__nonnull__(1)));


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: constructors and destructors.
 ** ----------------------------------------------------------------- */

mmec_decl mmec_intrep_bytevector_t * mmec_new_intrep_bytevector (emacs_env * env, intmax_t number_of_slots,
								 intmax_t slot_size, bool hold_signed_values);
mmec_decl void mmec_delete_intrep_bytevector (mmec_intrep_bytevector_t * bv)
  __attribute__((__nonnull__(1)));


typedef mmec_intrep_bytevector_t * mmec_new_intrep_bytevector_fun_t (emacs_env * env, intmax_t number_of_slots);

mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_char_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_schar_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_uchar_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_wchar_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_sshrt_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_ushrt_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_sint_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_uint_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_slong_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_ulong_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_sllong_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_ullong_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_sintmax_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_uintmax_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_ssize_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_usize_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_ptrdiff_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_sint8_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_uint8_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_sint16_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_uint16_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_sint32_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_uint32_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_sint64_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_uint64_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_float_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_double_intrep_bytevector;
mmec_decl mmec_new_intrep_bytevector_fun_t mmec_new_ldouble_intrep_bytevector;


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: inspection.
 ** ----------------------------------------------------------------- */

mmec_decl bool mmec_bytevector_valid_slot_index (mmec_intrep_bytevector_t const * const bv, intmax_t const idx)
  __attribute__((__nonnull__(1)));

mmec_decl bool mmec_bytevector_valid_past_slot_index (mmec_intrep_bytevector_t const * const bv, intmax_t const idx)
  __attribute__((__nonnull__(1)));

mmec_decl bool mmec_bytevector_valid_start_and_past_slot_index (mmec_intrep_bytevector_t const * bv,
								intmax_t start, intmax_t past)
  __attribute__((__nonnull__(1)));

mmec_decl bool mmec_intrep_bytevector_is_empty (mmec_intrep_bytevector_t const * const bv)
  __attribute__((__nonnull__(1)));

mmec_decl bool mmec_intrep_bytevector_is_not_empty (mmec_intrep_bytevector_t const * const bv)
  __attribute__((__nonnull__(1)));


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: getters and setters.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_BYTEVECTOR_GETTER
#define MMEC_DEFINE_BYTEVECTOR_GETTER(TYPESTEM)				\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_bytevector_ ## TYPESTEM ## _ref (mmec_intrep_bytevector_t const * const bv, intmax_t idx) \
  {									\
    MMEC_PC(mmec_clang_ ## TYPESTEM ## _t, data, bv->ptr);		\
    return data[idx];							\
  }

#undef  MMEC_DEFINE_BYTEVECTOR_SETTER
#define MMEC_DEFINE_BYTEVECTOR_SETTER(TYPESTEM)				\
  static inline void							\
  mmec_bytevector_ ## TYPESTEM ## _set (mmec_intrep_bytevector_t *bv, intmax_t idx, mmec_clang_ ## TYPESTEM ## _t val) \
  {									\
    MMEC_PC(mmec_clang_ ## TYPESTEM ## _t, data, bv->ptr);		\
    data[idx] = val;							\
  }

#undef  MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER
#define MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(TYPESTEM)			\
  MMEC_DEFINE_BYTEVECTOR_SETTER(TYPESTEM)				\
  MMEC_DEFINE_BYTEVECTOR_GETTER(TYPESTEM)

MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(char)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(schar)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(uchar)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(wchar)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(sshrt)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(ushrt)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(sint)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(uint)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(slong)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(ulong)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(sllong)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(ullong)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(sintmax)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(uintmax)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(ssize)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(usize)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(ptrdiff)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(sint8)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(uint8)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(sint16)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(uint16)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(sint32)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(uint32)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(sint64)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(uint64)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(float)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(double)
MMEC_DEFINE_BYTEVECTOR_SETTER_GETTER(ldouble)


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: comparison.
 ** ----------------------------------------------------------------- */

typedef int mmec_intrep_bytevector_compare_fun_t \
    (mmec_intrep_bytevector_t const * const src, intmax_t const src_start, intmax_t const src_past,
     mmec_intrep_bytevector_t const * const dst, intmax_t const dst_start, intmax_t const dst_past);

typedef bool mmec_intrep_bytevector_comparison_fun_t \
    (mmec_intrep_bytevector_t const * const src, intmax_t const src_start, intmax_t const src_past,
     mmec_intrep_bytevector_t const * const dst, intmax_t const dst_start, intmax_t const dst_past);

#undef  MMEC_DECLARE_BYTEVECTOR_COMPARISON
#define MMEC_DECLARE_BYTEVECTOR_COMPARISON(TYPESTEM)			\
  mmec_decl mmec_intrep_bytevector_compare_fun_t mmec_ ## TYPESTEM ## _intrep_bytevector_compare    __attribute__((__nonnull__(1,4))); \
  mmec_decl mmec_intrep_bytevector_comparison_fun_t mmec_ ## TYPESTEM ## _intrep_bytevector_equal   __attribute__((__nonnull__(1,4))); \
  mmec_decl mmec_intrep_bytevector_comparison_fun_t mmec_ ## TYPESTEM ## _intrep_bytevector_less    __attribute__((__nonnull__(1,4))); \
  mmec_decl mmec_intrep_bytevector_comparison_fun_t mmec_ ## TYPESTEM ## _intrep_bytevector_greater __attribute__((__nonnull__(1,4))); \
  mmec_decl mmec_intrep_bytevector_comparison_fun_t mmec_ ## TYPESTEM ## _intrep_bytevector_leq     __attribute__((__nonnull__(1,4))); \
  mmec_decl mmec_intrep_bytevector_comparison_fun_t mmec_ ## TYPESTEM ## _intrep_bytevector_geq     __attribute__((__nonnull__(1,4)));

MMEC_DECLARE_BYTEVECTOR_COMPARISON(char)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(schar)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(uchar)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(wchar)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(sshrt)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(ushrt)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(sint)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(uint)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(slong)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(ulong)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(sllong)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(ullong)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(sintmax)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(uintmax)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(ssize)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(usize)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(ptrdiff)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(sint8)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(uint8)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(sint16)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(uint16)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(sint32)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(uint32)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(sint64)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(uint64)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(float)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(double)
MMEC_DECLARE_BYTEVECTOR_COMPARISON(ldouble)


/** --------------------------------------------------------------------
 ** Bytevector user-pointer objects: operations.
 ** ----------------------------------------------------------------- */

mmec_decl mmec_intrep_bytevector_t * mmec_new_intrep_bytevector_subsequence (emacs_env * env, mmec_intrep_bytevector_t const * src,
									     intmax_t start, intmax_t past)
  __attribute__((__nonnull__(2)));


/** --------------------------------------------------------------------
 ** Miscellaneous functions.
 ** ----------------------------------------------------------------- */

mmec_decl intmax_t mmec_most_positive_fixnum (void);
mmec_decl intmax_t mmec_most_negative_fixnum (void);


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* MMEC_H */

/* end of file */
