/*
  Part of: MMUX Emacs Core
  Contents: mathematics functions
  Date: Mar  2, 2020

  Abstract

	This  module defines  the mathematical  functions applied  to the  C language
	numeric types.

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
#include <math.h>
#include <float.h>


/** --------------------------------------------------------------------
 ** Helper functions.
 ** ----------------------------------------------------------------- */

static inline double
square (double X)
{
  return X * X;
}
static inline float
squaref (float X)
{
  return X * X;
}
static inline long double
squarel (long double X)
{
  return X * X;
}

/* ------------------------------------------------------------------ */

static inline double
cube (double X)
{
  return X * X * X;
}

static inline float
cubef (float X)
{
  return X * X * X;
}

static inline long double
cubel (long double X)
{
  return X * X * X;
}

/* ------------------------------------------------------------------ */

static inline float
exp10f (float X)
{
  return expf(X * logf(10.0));
}
static inline double
exp10 (double X)
{
  return exp(X * log(10.0));
}
static inline long double
exp10l (long double X)
{
  return expl(X * logl(10.0));
}

/* ------------------------------------------------------------------ */

static inline double
root (double X, double Y)
{
  return pow(X, 1.0/Y);
}
static inline float
rootf (float X, float Y)
{
  return pow(X, ((float)1.0)/Y);
}
static inline long double
rootl (long double X, long double Y)
{
  return pow(X, ((long double)1.0)/Y);
}


/** --------------------------------------------------------------------
 ** Macros.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_FUNC1
#define MMEC_DEFINE_FUNC1(FUNCSTEM, TYPESTEM, SUFFIX)			\
  static emacs_value							\
  Fmmec_c_ ## TYPESTEM ## _ ## FUNCSTEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED) \
  {									\
    assert(1 == nargs);							\
    mmec_clang_ ## TYPESTEM ## _t	op = mmec_extract_clang_ ## TYPESTEM ## _from_emacs_value(env, args[0]); \
    mmec_clang_ ## TYPESTEM ## _t	rv = FUNCSTEM ## SUFFIX(op);	\
									\
    return mmec_new_emacs_value_from_clang_ ## TYPESTEM(env, rv);	\
  }

#undef  MMEC_DEFINE_FUNC2
#define MMEC_DEFINE_FUNC2(FUNCSTEM, TYPESTEM, SUFFIX)			\
  static emacs_value							\
  Fmmec_c_ ## TYPESTEM ## _ ## FUNCSTEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED) \
  {									\
    assert(2 == nargs);							\
    mmec_clang_ ## TYPESTEM ## _t	op1 = mmec_extract_clang_ ## TYPESTEM ## _from_emacs_value(env, args[0]); \
    mmec_clang_ ## TYPESTEM ## _t	op2 = mmec_extract_clang_ ## TYPESTEM ## _from_emacs_value(env, args[0]); \
    mmec_clang_ ## TYPESTEM ## _t	rv = FUNCSTEM ## SUFFIX(op1, op2); \
									\
    return mmec_new_emacs_value_from_clang_ ## TYPESTEM(env, rv);	\
  }


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS
#define MMEC_DEFINE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS(TYPESTEM, SUFFIX)	\
  MMEC_DEFINE_FUNC1(square	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(cube	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC2(pow	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(sqrt	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(cbrt	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC2(root	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC2(hypot	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(expm1	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(log1p	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(exp	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(exp2	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(exp10	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(log	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(log2	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(log10	,TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(logb	,TYPESTEM, SUFFIX)

MMEC_DEFINE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS(float,   f)
MMEC_DEFINE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS(double,   )
MMEC_DEFINE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS(ldouble, l)


/** --------------------------------------------------------------------
 ** Trigonometric functions.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_TRIGONOMETRIC_FUNCTIONS
#define MMEC_DEFINE_TRIGONOMETRIC_FUNCTIONS(TYPESTEM, SUFFIX)	\
  MMEC_DEFINE_FUNC1(sin,	TYPESTEM, SUFFIX)	\
    MMEC_DEFINE_FUNC1(cos,	TYPESTEM, SUFFIX)	\
    MMEC_DEFINE_FUNC1(tan,	TYPESTEM, SUFFIX)	\
    MMEC_DEFINE_FUNC1(asin,	TYPESTEM, SUFFIX)	\
    MMEC_DEFINE_FUNC1(acos,	TYPESTEM, SUFFIX)	\
    MMEC_DEFINE_FUNC1(atan,	TYPESTEM, SUFFIX)	\
    MMEC_DEFINE_FUNC2(atan2,	TYPESTEM, SUFFIX)

MMEC_DEFINE_TRIGONOMETRIC_FUNCTIONS(float,   f)
MMEC_DEFINE_TRIGONOMETRIC_FUNCTIONS(double,   )
MMEC_DEFINE_TRIGONOMETRIC_FUNCTIONS(ldouble, l)


/** --------------------------------------------------------------------
 ** Hyperbolic functions.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_HYPERBOLIC_FUNCTIONS
#define MMEC_DEFINE_HYPERBOLIC_FUNCTIONS(TYPESTEM, SUFFIX)	\
  MMEC_DEFINE_FUNC1(sinh,	TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(cosh,	TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(tanh,	TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(asinh,	TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(acosh,	TYPESTEM, SUFFIX)		\
    MMEC_DEFINE_FUNC1(atanh,	TYPESTEM, SUFFIX)

MMEC_DEFINE_HYPERBOLIC_FUNCTIONS(float,   f)
MMEC_DEFINE_HYPERBOLIC_FUNCTIONS(double,   )
MMEC_DEFINE_HYPERBOLIC_FUNCTIONS(ldouble, l)


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS	(17 * 3)
static mmec_module_function_t const
  module_exponentiation_and_logarithms_functions_table[NUMBER_OF_MODULE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS] = {
  /* Exponentiation  and  Logarithms  functions   for  MMEC  numbers  whose  internal
     representation is an Emacs built-in value of type `float'. */
  {
    .name		= "mmec-c-double-square",
    .implementation	= Fmmec_c_double_square,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the square of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-cube",
    .implementation	= Fmmec_c_double_cube,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the cube of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-pow",
    .implementation	= Fmmec_c_double_pow,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute X raised to the power of Y, where the operands are number values of type `float'.",
  },
  {
    .name		= "mmec-c-double-square",
    .implementation	= Fmmec_c_double_square,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute X raised to the power of Y, where the operands are number values of type `float'.",
  },
  {
    .name		= "mmec-c-double-sqrt",
    .implementation	= Fmmec_c_double_sqrt,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute the square root of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-cbrt",
    .implementation	= Fmmec_c_double_cbrt,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the cubic root of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-root",
    .implementation	= Fmmec_c_double_root,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute the Yth root of X, where the operands are number values of type `float'.",
  },
  {
    .name		= "mmec-c-double-hypot",
    .implementation	= Fmmec_c_double_hypot,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute sqrt(X*X + Y*Y) where the operands are number values of type `float'.",
  },
  {
    .name		= "mmec-c-double-expm1",
    .implementation	= Fmmec_c_double_expm1,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the exp(X - 1) where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-log1p",
    .implementation	= Fmmec_c_double_log1p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the log(1 + X) where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-exp",
    .implementation	= Fmmec_c_double_exp,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the exp(X) where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-exp2",
    .implementation	= Fmmec_c_double_exp2,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 2 raised to the power of X where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-exp10",
    .implementation	= Fmmec_c_double_exp10,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 10 raised to the power of X where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-log",
    .implementation	= Fmmec_c_double_log,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the natural logarithm of X where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-log2",
    .implementation	= Fmmec_c_double_log2,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the base 2 logarithm of X where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-log10",
    .implementation	= Fmmec_c_double_log10,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the base 10 logarithm of X where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-logb",
    .implementation	= Fmmec_c_double_logb,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Extract the exponent of X and return it, where the operand is a number value of type `float'.",
  },

  /* Trigonometric functions  for MMEC  numbers whose  internal representation  is an
     Emacs user-pointer object of type `float'. */
  {
    .name		= "mmec-c-float-square",
    .implementation	= Fmmec_c_float_square,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the square of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-cube",
    .implementation	= Fmmec_c_float_cube,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the cube of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-pow",
    .implementation	= Fmmec_c_float_pow,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute X raised to the power of Y, where the operands are user-pointer objects of type `float'.",
  },
  {
    .name		= "mmec-c-float-square",
    .implementation	= Fmmec_c_float_square,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute X raised to the power of Y, where the operands are user-pointer objects of type `float'.",
  },
  {
    .name		= "mmec-c-float-sqrt",
    .implementation	= Fmmec_c_float_sqrt,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute the square root of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-cbrt",
    .implementation	= Fmmec_c_float_cbrt,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the cubic root of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-root",
    .implementation	= Fmmec_c_float_root,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute the Yth root of X, where the operands are user-pointer objects of type `float'.",
  },
  {
    .name		= "mmec-c-float-hypot",
    .implementation	= Fmmec_c_float_hypot,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute sqrt(X*X + Y*Y) where the operands are user-pointer objects of type `float'.",
  },
  {
    .name		= "mmec-c-float-expm1",
    .implementation	= Fmmec_c_float_expm1,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the exp(X - 1) where the operand is a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-log1p",
    .implementation	= Fmmec_c_float_log1p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the log(1 + X) where the operand is a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-exp",
    .implementation	= Fmmec_c_float_exp,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the exp(X) where the operand is a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-exp2",
    .implementation	= Fmmec_c_float_exp2,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 2 raised to the power of X where the operand is a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-exp10",
    .implementation	= Fmmec_c_float_exp10,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 10 raised to the power of X where the operand is a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-log",
    .implementation	= Fmmec_c_float_log,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the natural logarithm of X where the operand is a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-log2",
    .implementation	= Fmmec_c_float_log2,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the base 2 logarithm of X where the operand is a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-log10",
    .implementation	= Fmmec_c_float_log10,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the base 10 logarithm of X where the operand is a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-logb",
    .implementation	= Fmmec_c_float_logb,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Extract the exponent of X and return it, where the operand is a user-pointer object of type `float'.",
  },

  /* Trigonometric functions  for MMEC  numbers whose  internal representation  is an
     Emacs user-pointer object of type `ldouble'. */
  {
    .name		= "mmec-c-ldouble-square",
    .implementation	= Fmmec_c_ldouble_square,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the square of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-cube",
    .implementation	= Fmmec_c_ldouble_cube,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the cube of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-pow",
    .implementation	= Fmmec_c_ldouble_pow,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute X raised to the power of Y, where the operands are user-pointer objects of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-square",
    .implementation	= Fmmec_c_ldouble_square,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute X raised to the power of Y, where the operands are user-pointer objects of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-sqrt",
    .implementation	= Fmmec_c_ldouble_sqrt,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute the square root of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-cbrt",
    .implementation	= Fmmec_c_ldouble_cbrt,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the cubic root of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-root",
    .implementation	= Fmmec_c_ldouble_root,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute the Yth root of X, where the operands are user-pointer objects of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-hypot",
    .implementation	= Fmmec_c_ldouble_hypot,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute sqrt(X*X + Y*Y) where the operands are user-pointer objects of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-expm1",
    .implementation	= Fmmec_c_ldouble_expm1,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the exp(X - 1) where the operand is a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-log1p",
    .implementation	= Fmmec_c_ldouble_log1p,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the log(1 + X) where the operand is a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-exp",
    .implementation	= Fmmec_c_ldouble_exp,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the exp(X) where the operand is a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-exp2",
    .implementation	= Fmmec_c_ldouble_exp2,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 2 raised to the power of X where the operand is a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-exp10",
    .implementation	= Fmmec_c_ldouble_exp10,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 10 raised to the power of X where the operand is a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-log",
    .implementation	= Fmmec_c_ldouble_log,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the natural logarithm of X where the operand is a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-log2",
    .implementation	= Fmmec_c_ldouble_log2,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the base 2 logarithm of X where the operand is a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-log10",
    .implementation	= Fmmec_c_ldouble_log10,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the base 10 logarithm of X where the operand is a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-logb",
    .implementation	= Fmmec_c_ldouble_logb,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Extract the exponent of X and return it, where the operand is a user-pointer object of type `ldouble'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_TRIGONOMETRIC_FUNCTIONS	(7 * 3)
static mmec_module_function_t const module_trigonometric_functions_table[NUMBER_OF_MODULE_TRIGONOMETRIC_FUNCTIONS] = {
  /* Trigonometric functions  for MMEC  numbers whose  internal representation  is an
     Emacs built-in value of type `float'. */
  {
    .name		= "mmec-c-double-sin",
    .implementation	= Fmmec_c_double_sin,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric sine of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-cos",
    .implementation	= Fmmec_c_double_cos,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric cosine of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-tan",
    .implementation	= Fmmec_c_double_tan,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric tangent of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-asin",
    .implementation	= Fmmec_c_double_asin,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric arc sine of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-acos",
    .implementation	= Fmmec_c_double_acos,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric arc cosine of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-atan",
    .implementation	= Fmmec_c_double_atan,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric arc tangent of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-atan2",
    .implementation	= Fmmec_c_double_atan2,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute the trigonometric arc tangent of the division between two number values of type `float'.",
  },

  /* Trigonometric functions  for MMEC  numbers whose  internal representation  is an
     Emacs user-pointer object of type `float'. */
  {
    .name		= "mmec-c-float-sin",
    .implementation	= Fmmec_c_float_sin,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric sine of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-cos",
    .implementation	= Fmmec_c_float_cos,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric cosine of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-tan",
    .implementation	= Fmmec_c_float_tan,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric tangent of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-asin",
    .implementation	= Fmmec_c_float_asin,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric arc sine of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-acos",
    .implementation	= Fmmec_c_float_acos,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric arc cosine of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-atan",
    .implementation	= Fmmec_c_float_atan,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric arc tangent of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-atan2",
    .implementation	= Fmmec_c_float_atan2,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute the trigonometric arc tangent of the division between two user-pointer object of type `float'.",
  },

  /* Trigonometric functions  for MMEC  numbers whose  internal representation  is an
     Emacs user-pointer object of type `ldouble'. */
  {
    .name		= "mmec-c-ldouble-sin",
    .implementation	= Fmmec_c_ldouble_sin,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric sine of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-cos",
    .implementation	= Fmmec_c_ldouble_cos,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric cosine of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-tan",
    .implementation	= Fmmec_c_ldouble_tan,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric tangent of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-asin",
    .implementation	= Fmmec_c_ldouble_asin,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric arc sine of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-acos",
    .implementation	= Fmmec_c_ldouble_acos,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric arc cosine of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-atan",
    .implementation	= Fmmec_c_ldouble_atan,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the trigonometric arc tangent of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-atan2",
    .implementation	= Fmmec_c_ldouble_atan2,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute the trigonometric arc tangent of the division between two user-pointer object of type `ldouble'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_HYPERBOLIC_FUNCTIONS	(6 * 3)
static mmec_module_function_t const module_hyperbolic_functions_table[NUMBER_OF_MODULE_HYPERBOLIC_FUNCTIONS] = {
  /* Hyperbolic functions  for MMEC  numbers whose  internal representation  is an
     Emacs built-in value of type `float'. */
  {
    .name		= "mmec-c-double-sinh",
    .implementation	= Fmmec_c_double_sinh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic sine of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-cosh",
    .implementation	= Fmmec_c_double_cosh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic cosine of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-tanh",
    .implementation	= Fmmec_c_double_tanh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic tangent of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-asinh",
    .implementation	= Fmmec_c_double_asinh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic inverse sine of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-acosh",
    .implementation	= Fmmec_c_double_acosh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic inverse cosine of a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-atanh",
    .implementation	= Fmmec_c_double_atanh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic inverse tangent of a number value of type `float'.",
  },

  /* Hyperbolic functions  for MMEC  numbers whose  internal representation  is an
     Emacs user-pointer object of type `float'. */
  {
    .name		= "mmec-c-float-sinh",
    .implementation	= Fmmec_c_float_sinh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic sine of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-cosh",
    .implementation	= Fmmec_c_float_cosh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic cosine of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-tanh",
    .implementation	= Fmmec_c_float_tanh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic tangent of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-asinh",
    .implementation	= Fmmec_c_float_asinh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic inverse sine of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-acosh",
    .implementation	= Fmmec_c_float_acosh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic inverse cosine of a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-atanh",
    .implementation	= Fmmec_c_float_atanh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic inverse tangent of a user-pointer object of type `float'.",
  },

  /* Hyperbolic functions  for MMEC  numbers whose  internal representation  is an
     Emacs user-pointer object of type `ldouble'. */
  {
    .name		= "mmec-c-ldouble-sinh",
    .implementation	= Fmmec_c_ldouble_sinh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic sine of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-cosh",
    .implementation	= Fmmec_c_ldouble_cosh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic cosine of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-tanh",
    .implementation	= Fmmec_c_ldouble_tanh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic tangent of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-asinh",
    .implementation	= Fmmec_c_ldouble_asinh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic inverse sine of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-acosh",
    .implementation	= Fmmec_c_ldouble_acosh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic inverse cosine of a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-atanh",
    .implementation	= Fmmec_c_ldouble_atanh,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute the hyperbolic inverse tangent of a user-pointer object of type `ldouble'.",
  },
};


/** --------------------------------------------------------------------
 ** Module initialisation.
 ** ----------------------------------------------------------------- */

void
mmec_mathematics_init (emacs_env * env)
{
  mmec_define_elisp_functions_from_table(env, module_exponentiation_and_logarithms_functions_table,
					 NUMBER_OF_MODULE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS, 0);
  mmec_define_elisp_functions_from_table(env, module_trigonometric_functions_table,
					 NUMBER_OF_MODULE_TRIGONOMETRIC_FUNCTIONS, 0);
  mmec_define_elisp_functions_from_table(env, module_hyperbolic_functions_table,
					 NUMBER_OF_MODULE_HYPERBOLIC_FUNCTIONS, 0);
}

/* end of file */
