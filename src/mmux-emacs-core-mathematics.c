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
 ** Helper functions: arithmetics.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_CLANG_ADD
#define MMEC_DEFINE_CLANG_ADD(TYPESTEM)					\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _add (mmec_clang_ ## TYPESTEM ## _t X,	\
				   mmec_clang_ ## TYPESTEM ## _t Y)	\
  {									\
    return X + Y;							\
  }

MMEC_DEFINE_CLANG_ADD(sint64)
MMEC_DEFINE_CLANG_ADD(uint64)
MMEC_DEFINE_CLANG_ADD(float)
MMEC_DEFINE_CLANG_ADD(double)
MMEC_DEFINE_CLANG_ADD(ldouble)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_SUB
#define MMEC_DEFINE_CLANG_SUB(TYPESTEM)					\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _sub (mmec_clang_ ## TYPESTEM ## _t X,	\
				   mmec_clang_ ## TYPESTEM ## _t Y)	\
  {									\
    return X - Y;							\
  }

MMEC_DEFINE_CLANG_SUB(sint64)
MMEC_DEFINE_CLANG_SUB(uint64)
MMEC_DEFINE_CLANG_SUB(float)
MMEC_DEFINE_CLANG_SUB(double)
MMEC_DEFINE_CLANG_SUB(ldouble)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_MUL
#define MMEC_DEFINE_CLANG_MUL(TYPESTEM)					\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _mul (mmec_clang_ ## TYPESTEM ## _t X,	\
				   mmec_clang_ ## TYPESTEM ## _t Y)	\
  {									\
    return X * Y;							\
  }

MMEC_DEFINE_CLANG_MUL(sint64)
MMEC_DEFINE_CLANG_MUL(uint64)
MMEC_DEFINE_CLANG_MUL(float)
MMEC_DEFINE_CLANG_MUL(double)
MMEC_DEFINE_CLANG_MUL(ldouble)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_DIV
#define MMEC_DEFINE_CLANG_DIV(TYPESTEM)					\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _div (mmec_clang_ ## TYPESTEM ## _t X,	\
				   mmec_clang_ ## TYPESTEM ## _t Y)	\
  {									\
    return X + Y;							\
  }

MMEC_DEFINE_CLANG_DIV(sint64)
MMEC_DEFINE_CLANG_DIV(uint64)
MMEC_DEFINE_CLANG_DIV(float)
MMEC_DEFINE_CLANG_DIV(double)
MMEC_DEFINE_CLANG_DIV(ldouble)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_NEG
#define MMEC_DEFINE_CLANG_NEG(TYPESTEM)					\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _neg (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return - X;								\
  }

MMEC_DEFINE_CLANG_NEG(sint64)
MMEC_DEFINE_CLANG_NEG(uint64)
MMEC_DEFINE_CLANG_NEG(float)
MMEC_DEFINE_CLANG_NEG(double)
MMEC_DEFINE_CLANG_NEG(ldouble)

/* ------------------------------------------------------------------ */

static inline mmec_clang_sint64_t
mmec_clang_sint64_inv (mmec_clang_sint64_t X MMEC_UNUSED)
{
  return 0;
}

static inline mmec_clang_uint64_t
mmec_clang_uint64_inv (mmec_clang_uint64_t X MMEC_UNUSED)
{
  return 0;
}

#undef  MMEC_DEFINE_CLANG_INV
#define MMEC_DEFINE_CLANG_INV(TYPESTEM)					\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _inv (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return ((mmec_clang_ ## TYPESTEM ## _t)1.0) / X;			\
  }

MMEC_DEFINE_CLANG_INV(float)
MMEC_DEFINE_CLANG_INV(double)
MMEC_DEFINE_CLANG_INV(ldouble)

/* ------------------------------------------------------------------ */

static inline mmec_clang_sint64_t
mmec_clang_sint64_mod (mmec_clang_sint64_t X, mmec_clang_sint64_t Y)
{
  mmec_clang_sint64_t  rv = X % Y;

  if ((Y < 0)? (rv > 0) : (rv < 0)) {
    rv += Y;
  }
  return rv;
}

static inline mmec_clang_uint64_t
mmec_clang_uint64_mod (mmec_clang_uint64_t X, mmec_clang_uint64_t Y)
{
  return X % Y;
}

#undef  MMEC_DEFINE_CLANG_MOD
#define MMEC_DEFINE_CLANG_MOD(TYPESTEM, SUFFIX)				\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _mod (mmec_clang_ ## TYPESTEM ## _t X,	\
				   mmec_clang_ ## TYPESTEM ## _t Y)	\
  {									\
    return fmod ## SUFFIX(X, Y);					\
  }

MMEC_DEFINE_CLANG_MOD(float,   f)
MMEC_DEFINE_CLANG_MOD(double,   )
MMEC_DEFINE_CLANG_MOD(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_PERCENT
#define MMEC_DEFINE_CLANG_PERCENT(TYPESTEM)				\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _percent (mmec_clang_ ## TYPESTEM ## _t X,	\
				       mmec_clang_ ## TYPESTEM ## _t Y)	\
  {									\
    return X % Y;							\
  }

MMEC_DEFINE_CLANG_PERCENT(sint64)
MMEC_DEFINE_CLANG_PERCENT(uint64)


/** --------------------------------------------------------------------
 ** Helper functions: parts.
 ** ----------------------------------------------------------------- */

static inline mmec_clang_sint64_t
mmec_clang_sint64_abs (mmec_clang_sint64_t X)
{
  return ((X < 0)? (- X) : X);
}

static inline mmec_clang_uint64_t
mmec_clang_uint64_abs (mmec_clang_uint64_t X)
{
  return X;
}

#undef  MMEC_DEFINE_CLANG_ABS
#define MMEC_DEFINE_CLANG_ABS(TYPESTEM, SUFFIX)				\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _abs (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return fabs ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_ABS(float,   f)
MMEC_DEFINE_CLANG_ABS(double,   )
MMEC_DEFINE_CLANG_ABS(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_CEIL
#define MMEC_DEFINE_CLANG_CEIL(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _ceil (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return ceil ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_CEIL(float,   f)
MMEC_DEFINE_CLANG_CEIL(double,   )
MMEC_DEFINE_CLANG_CEIL(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_FLOOR
#define MMEC_DEFINE_CLANG_FLOOR(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _floor (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return floor ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_FLOOR(float,   f)
MMEC_DEFINE_CLANG_FLOOR(double,   )
MMEC_DEFINE_CLANG_FLOOR(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_TRUNC
#define MMEC_DEFINE_CLANG_TRUNC(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _trunc (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return trunc ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_TRUNC(float,   f)
MMEC_DEFINE_CLANG_TRUNC(double,   )
MMEC_DEFINE_CLANG_TRUNC(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_RINT
#define MMEC_DEFINE_CLANG_RINT(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _rint (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return rint ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_RINT(float,   f)
MMEC_DEFINE_CLANG_RINT(double,   )
MMEC_DEFINE_CLANG_RINT(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_ROUND
#define MMEC_DEFINE_CLANG_ROUND(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _round (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return round ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_ROUND(float,   f)
MMEC_DEFINE_CLANG_ROUND(double,   )
MMEC_DEFINE_CLANG_ROUND(ldouble, l)


/** --------------------------------------------------------------------
 ** Helper functions: exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_CLANG_SQUARE
#define MMEC_DEFINE_CLANG_SQUARE(TYPESTEM)				\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _square (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return X * X;							\
  }

MMEC_DEFINE_CLANG_SQUARE(float)
MMEC_DEFINE_CLANG_SQUARE(double)
MMEC_DEFINE_CLANG_SQUARE(ldouble)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_CUBE
#define MMEC_DEFINE_CLANG_CUBE(TYPESTEM)				\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _cube (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return X * X * X;							\
  }

MMEC_DEFINE_CLANG_CUBE(float)
MMEC_DEFINE_CLANG_CUBE(double)
MMEC_DEFINE_CLANG_CUBE(ldouble)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_SQRT
#define MMEC_DEFINE_CLANG_SQRT(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _sqrt (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return sqrt ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_SQRT(float,   f)
MMEC_DEFINE_CLANG_SQRT(double,   )
MMEC_DEFINE_CLANG_SQRT(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_CBRT
#define MMEC_DEFINE_CLANG_CBRT(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _cbrt (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return cbrt ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_CBRT(float,   f)
MMEC_DEFINE_CLANG_CBRT(double,   )
MMEC_DEFINE_CLANG_CBRT(ldouble, l)

/* ------------------------------------------------------------------ */

static inline float
mmec_clang_float_root (float X, float Y)
{
  return powf(X, ((float)1.0)/Y);
}
static inline double
mmec_clang_double_root (double X, double Y)
{
  return pow(X, 1.0/Y);
}
static inline long double
mmec_clang_ldouble_root (long double X, long double Y)
{
  return powl(X, ((long double)1.0)/Y);
}

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_HYPOT
#define MMEC_DEFINE_CLANG_HYPOT(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _hypot (mmec_clang_ ## TYPESTEM ## _t X,	\
				     mmec_clang_ ## TYPESTEM ## _t Y)	\
  {									\
    return hypot ## SUFFIX(X, Y);					\
  }

MMEC_DEFINE_CLANG_HYPOT(float,   f)
MMEC_DEFINE_CLANG_HYPOT(double,   )
MMEC_DEFINE_CLANG_HYPOT(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_POW
#define MMEC_DEFINE_CLANG_POW(TYPESTEM, SUFFIX)				\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _pow (mmec_clang_ ## TYPESTEM ## _t X,	\
				   mmec_clang_ ## TYPESTEM ## _t Y)	\
  {									\
    return pow ## SUFFIX(X, Y);						\
  }

MMEC_DEFINE_CLANG_POW(float,   f)
MMEC_DEFINE_CLANG_POW(double,   )
MMEC_DEFINE_CLANG_POW(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_EXPM1
#define MMEC_DEFINE_CLANG_EXPM1(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _expm1 (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return expm1 ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_EXPM1(float,   f)
MMEC_DEFINE_CLANG_EXPM1(double,   )
MMEC_DEFINE_CLANG_EXPM1(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_LOG1P
#define MMEC_DEFINE_CLANG_LOG1P(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _log1p (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return log1p ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_LOG1P(float,   f)
MMEC_DEFINE_CLANG_LOG1P(double,   )
MMEC_DEFINE_CLANG_LOG1P(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_EXP
#define MMEC_DEFINE_CLANG_EXP(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _exp (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return exp ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_EXP(float,   f)
MMEC_DEFINE_CLANG_EXP(double,   )
MMEC_DEFINE_CLANG_EXP(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_EXP2
#define MMEC_DEFINE_CLANG_EXP2(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _exp2 (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return exp2 ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_EXP2(float,   f)
MMEC_DEFINE_CLANG_EXP2(double,   )
MMEC_DEFINE_CLANG_EXP2(ldouble, l)

/* ------------------------------------------------------------------ */

static inline float
mmec_clang_float_exp10 (float X)
{
  return expf(X * logf(10.0));
}
static inline double
mmec_clang_double_exp10 (double X)
{
  return exp(X * log(10.0));
}
static inline long double
mmec_clang_ldouble_exp10 (long double X)
{
  return expl(X * logl(10.0));
}

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_LOG
#define MMEC_DEFINE_CLANG_LOG(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _log (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return log ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_LOG(float,   f)
MMEC_DEFINE_CLANG_LOG(double,   )
MMEC_DEFINE_CLANG_LOG(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_LOG2
#define MMEC_DEFINE_CLANG_LOG2(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _log2 (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return log2 ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_LOG2(float,   f)
MMEC_DEFINE_CLANG_LOG2(double,   )
MMEC_DEFINE_CLANG_LOG2(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_LOG10
#define MMEC_DEFINE_CLANG_LOG10(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _log10 (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return log10 ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_LOG10(float,   f)
MMEC_DEFINE_CLANG_LOG10(double,   )
MMEC_DEFINE_CLANG_LOG10(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_LOGB
#define MMEC_DEFINE_CLANG_LOGB(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _logb (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return logb ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_LOGB(float,   f)
MMEC_DEFINE_CLANG_LOGB(double,   )
MMEC_DEFINE_CLANG_LOGB(ldouble, l)


/** --------------------------------------------------------------------
 ** Helper functions: trigonometric functions.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_CLANG_SIN
#define MMEC_DEFINE_CLANG_SIN(TYPESTEM, SUFFIX)				\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _sin (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return sin ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_SIN(float,   f)
MMEC_DEFINE_CLANG_SIN(double,   )
MMEC_DEFINE_CLANG_SIN(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_COS
#define MMEC_DEFINE_CLANG_COS(TYPESTEM, SUFFIX)				\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _cos (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return cos ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_COS(float,   f)
MMEC_DEFINE_CLANG_COS(double,   )
MMEC_DEFINE_CLANG_COS(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_TAN
#define MMEC_DEFINE_CLANG_TAN(TYPESTEM, SUFFIX)				\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _tan (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return tan ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_TAN(float,   f)
MMEC_DEFINE_CLANG_TAN(double,   )
MMEC_DEFINE_CLANG_TAN(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_ASIN
#define MMEC_DEFINE_CLANG_ASIN(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _asin (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return asin ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_ASIN(float,   f)
MMEC_DEFINE_CLANG_ASIN(double,   )
MMEC_DEFINE_CLANG_ASIN(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_ACOS
#define MMEC_DEFINE_CLANG_ACOS(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _acos (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return acos ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_ACOS(float,   f)
MMEC_DEFINE_CLANG_ACOS(double,   )
MMEC_DEFINE_CLANG_ACOS(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_ATAN
#define MMEC_DEFINE_CLANG_ATAN(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _atan (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return atan ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_ATAN(float,   f)
MMEC_DEFINE_CLANG_ATAN(double,   )
MMEC_DEFINE_CLANG_ATAN(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_ATAN2
#define MMEC_DEFINE_CLANG_ATAN2(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _atan2 (mmec_clang_ ## TYPESTEM ## _t X,	\
				     mmec_clang_ ## TYPESTEM ## _t Y)	\
  {									\
    return atan2 ## SUFFIX(X, Y);					\
  }

MMEC_DEFINE_CLANG_ATAN2(float,   f)
MMEC_DEFINE_CLANG_ATAN2(double,   )
MMEC_DEFINE_CLANG_ATAN2(ldouble, l)


/** --------------------------------------------------------------------
 ** Helper functions: hyperbolic functions.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_CLANG_SINH
#define MMEC_DEFINE_CLANG_SINH(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _sinh (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return sinh ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_SINH(float,   f)
MMEC_DEFINE_CLANG_SINH(double,   )
MMEC_DEFINE_CLANG_SINH(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_COSH
#define MMEC_DEFINE_CLANG_COSH(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _cosh (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return cosh ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_COSH(float,   f)
MMEC_DEFINE_CLANG_COSH(double,   )
MMEC_DEFINE_CLANG_COSH(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_TANH
#define MMEC_DEFINE_CLANG_TANH(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _tanh (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return tanh ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_TANH(float,   f)
MMEC_DEFINE_CLANG_TANH(double,   )
MMEC_DEFINE_CLANG_TANH(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_ASINH
#define MMEC_DEFINE_CLANG_ASINH(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _asinh (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return asinh ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_ASINH(float,   f)
MMEC_DEFINE_CLANG_ASINH(double,   )
MMEC_DEFINE_CLANG_ASINH(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_ACOSH
#define MMEC_DEFINE_CLANG_ACOSH(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _acosh (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return acosh ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_ACOSH(float,   f)
MMEC_DEFINE_CLANG_ACOSH(double,   )
MMEC_DEFINE_CLANG_ACOSH(ldouble, l)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_CLANG_ATANH
#define MMEC_DEFINE_CLANG_ATANH(TYPESTEM, SUFFIX)			\
  static inline mmec_clang_ ## TYPESTEM ## _t				\
  mmec_clang_ ## TYPESTEM ## _atanh (mmec_clang_ ## TYPESTEM ## _t X)	\
  {									\
    return atanh ## SUFFIX(X);						\
  }

MMEC_DEFINE_CLANG_ATANH(float,   f)
MMEC_DEFINE_CLANG_ATANH(double,   )
MMEC_DEFINE_CLANG_ATANH(ldouble, l)


/** --------------------------------------------------------------------
 ** Macros.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_FUNC1
#define MMEC_DEFINE_FUNC1(FUNCSTEM, TYPESTEM)				\
  static emacs_value							\
  Fmmec_c_ ## TYPESTEM ## _ ## FUNCSTEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED) \
  {									\
    assert(1 == nargs);							\
    mmec_clang_ ## TYPESTEM ## _t	op = mmec_extract_clang_ ## TYPESTEM ## _from_emacs_value(env, args[0]); \
    mmec_clang_ ## TYPESTEM ## _t	rv = mmec_clang_ ## TYPESTEM ## _ ## FUNCSTEM (op); \
									\
    return mmec_new_emacs_value_from_clang_ ## TYPESTEM(env, rv);	\
  }

#undef  MMEC_DEFINE_FUNC2
#define MMEC_DEFINE_FUNC2(FUNCSTEM, TYPESTEM)				\
  static emacs_value							\
  Fmmec_c_ ## TYPESTEM ## _ ## FUNCSTEM (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void * data MMEC_UNUSED) \
  {									\
    assert(2 == nargs);							\
    mmec_clang_ ## TYPESTEM ## _t	op1 = mmec_extract_clang_ ## TYPESTEM ## _from_emacs_value(env, args[0]); \
    mmec_clang_ ## TYPESTEM ## _t	op2 = mmec_extract_clang_ ## TYPESTEM ## _from_emacs_value(env, args[0]); \
    mmec_clang_ ## TYPESTEM ## _t	rv = mmec_clang_ ## TYPESTEM ## _ ## FUNCSTEM(op1, op2); \
									\
    return mmec_new_emacs_value_from_clang_ ## TYPESTEM(env, rv);	\
  }


/** --------------------------------------------------------------------
 ** Arithmetics.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_ARITHMETICS_FUNCTIONS
#define MMEC_DEFINE_ARITHMETICS_FUNCTIONS(TYPESTEM)	\
  MMEC_DEFINE_FUNC2(add,	TYPESTEM)	\
    MMEC_DEFINE_FUNC2(sub,	TYPESTEM)	\
    MMEC_DEFINE_FUNC2(mul,	TYPESTEM)	\
    MMEC_DEFINE_FUNC2(div,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(neg,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(inv,	TYPESTEM)	\
    MMEC_DEFINE_FUNC2(mod,	TYPESTEM)

MMEC_DEFINE_ARITHMETICS_FUNCTIONS(sint64)
MMEC_DEFINE_ARITHMETICS_FUNCTIONS(uint64)
MMEC_DEFINE_ARITHMETICS_FUNCTIONS(float)
MMEC_DEFINE_ARITHMETICS_FUNCTIONS(double)
MMEC_DEFINE_ARITHMETICS_FUNCTIONS(ldouble)

#undef  MMEC_DEFINE_ARITHMETICS_FUNCTIONS
#define MMEC_DEFINE_ARITHMETICS_FUNCTIONS(TYPESTEM)	\
  MMEC_DEFINE_FUNC2(percent,	TYPESTEM)

MMEC_DEFINE_ARITHMETICS_FUNCTIONS(sint64)
MMEC_DEFINE_ARITHMETICS_FUNCTIONS(uint64)


/** --------------------------------------------------------------------
 ** Parts.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_PARTS_FUNCTIONS
#define MMEC_DEFINE_PARTS_FUNCTIONS(TYPESTEM)	\
  MMEC_DEFINE_FUNC1(abs,	TYPESTEM)

MMEC_DEFINE_PARTS_FUNCTIONS(sint64)
MMEC_DEFINE_PARTS_FUNCTIONS(uint64)
MMEC_DEFINE_PARTS_FUNCTIONS(float)
MMEC_DEFINE_PARTS_FUNCTIONS(double)
MMEC_DEFINE_PARTS_FUNCTIONS(ldouble)

/* ------------------------------------------------------------------ */

#undef  MMEC_DEFINE_PARTS_FUNCTIONS
#define MMEC_DEFINE_PARTS_FUNCTIONS(TYPESTEM)	\
  MMEC_DEFINE_FUNC1(ceil,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(floor,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(trunc,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(rint,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(round,	TYPESTEM)

MMEC_DEFINE_PARTS_FUNCTIONS(float)
MMEC_DEFINE_PARTS_FUNCTIONS(double)
MMEC_DEFINE_PARTS_FUNCTIONS(ldouble)


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS
#define MMEC_DEFINE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS(TYPESTEM) \
  MMEC_DEFINE_FUNC1(square	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(cube	,TYPESTEM)		\
    MMEC_DEFINE_FUNC2(pow	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(sqrt	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(cbrt	,TYPESTEM)		\
    MMEC_DEFINE_FUNC2(root	,TYPESTEM)		\
    MMEC_DEFINE_FUNC2(hypot	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(expm1	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(log1p	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(exp	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(exp2	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(exp10	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(log	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(log2	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(log10	,TYPESTEM)		\
    MMEC_DEFINE_FUNC1(logb	,TYPESTEM)

MMEC_DEFINE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS(float)
MMEC_DEFINE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS(double)
MMEC_DEFINE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS(ldouble)


/** --------------------------------------------------------------------
 ** Trigonometric functions.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_TRIGONOMETRIC_FUNCTIONS
#define MMEC_DEFINE_TRIGONOMETRIC_FUNCTIONS(TYPESTEM)	\
  MMEC_DEFINE_FUNC1(sin,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(cos,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(tan,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(asin,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(acos,	TYPESTEM)	\
    MMEC_DEFINE_FUNC1(atan,	TYPESTEM)	\
    MMEC_DEFINE_FUNC2(atan2,	TYPESTEM)

MMEC_DEFINE_TRIGONOMETRIC_FUNCTIONS(float)
MMEC_DEFINE_TRIGONOMETRIC_FUNCTIONS(double)
MMEC_DEFINE_TRIGONOMETRIC_FUNCTIONS(ldouble)


/** --------------------------------------------------------------------
 ** Hyperbolic functions.
 ** ----------------------------------------------------------------- */

#undef  MMEC_DEFINE_HYPERBOLIC_FUNCTIONS
#define MMEC_DEFINE_HYPERBOLIC_FUNCTIONS(TYPESTEM)	\
  MMEC_DEFINE_FUNC1(sinh,	TYPESTEM)		\
    MMEC_DEFINE_FUNC1(cosh,	TYPESTEM)		\
    MMEC_DEFINE_FUNC1(tanh,	TYPESTEM)		\
    MMEC_DEFINE_FUNC1(asinh,	TYPESTEM)		\
    MMEC_DEFINE_FUNC1(acosh,	TYPESTEM)		\
    MMEC_DEFINE_FUNC1(atanh,	TYPESTEM)

MMEC_DEFINE_HYPERBOLIC_FUNCTIONS(float)
MMEC_DEFINE_HYPERBOLIC_FUNCTIONS(double)
MMEC_DEFINE_HYPERBOLIC_FUNCTIONS(ldouble)


/** --------------------------------------------------------------------
 ** Elisp functions table: arithmetics.
 ** ----------------------------------------------------------------- */

/* 5 type stems, 7 operations for each type stem, 2 percent operations */
#define NUMBER_OF_MODULE_ARITHMETICS_FUNCTIONS	(7 * 5 + 2)
static mmec_module_function_t const
  module_arithmetics_functions_table[NUMBER_OF_MODULE_ARITHMETICS_FUNCTIONS] = {
  /* Functions for  MMEC numbers whose  internal representation is an  Emacs built-in
     value of type `float'. */
  {
    .name		= "mmec-c-double-add",
    .implementation	= Fmmec_c_double_add,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 + OP2, where the operands are number values of type `float'.",
  },
  {
    .name		= "mmec-c-double-sub",
    .implementation	= Fmmec_c_double_sub,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 - OP2, where the operands are number values of type `float'.",
  },
  {
    .name		= "mmec-c-double-mul",
    .implementation	= Fmmec_c_double_mul,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 * OP2, where the operands are number values of type `float'.",
  },
  {
    .name		= "mmec-c-double-div",
    .implementation	= Fmmec_c_double_div,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 / OP2, where the operands are number values of type `float'.",
  },
  {
    .name		= "mmec-c-double-mod",
    .implementation	= Fmmec_c_double_mod,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute mod(DIVIDEND, DIVISOR), where the operands are number values of type `float'.",
  },
  {
    .name		= "mmec-c-double-neg",
    .implementation	= Fmmec_c_double_neg,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute - OP, where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-inv",
    .implementation	= Fmmec_c_double_inv,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 1 / OP, where the operand is a number value of type `float'.",
  },

  /* Functions  for   MMEC  numbers  whose   internal  representation  is   an  Emacs
     user-pointer object of type `float'. */
  {
    .name		= "mmec-c-float-add",
    .implementation	= Fmmec_c_float_add,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 + OP2, where the operands are user-pointer objects of type `float'.",
  },
  {
    .name		= "mmec-c-float-sub",
    .implementation	= Fmmec_c_float_sub,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 - OP2, where the operands are user-pointer objects of type `float'.",
  },
  {
    .name		= "mmec-c-float-mul",
    .implementation	= Fmmec_c_float_mul,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 * OP2, where the operands are user-pointer objects of type `float'.",
  },
  {
    .name		= "mmec-c-float-div",
    .implementation	= Fmmec_c_float_div,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 / OP2, where the operands are user-pointer objects of type `float'.",
  },
  {
    .name		= "mmec-c-float-mod",
    .implementation	= Fmmec_c_float_mod,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute mod(DIVIDEND, DIVISOR), where the operands are user-pointer objects of type `float'.",
  },
  {
    .name		= "mmec-c-float-neg",
    .implementation	= Fmmec_c_float_neg,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute - OP, where the operand is a user-pointer object of type `float'.",
  },
  {
    .name		= "mmec-c-float-inv",
    .implementation	= Fmmec_c_float_inv,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 1 / OP, where the operand is a user-pointer object of type `float'.",
  },

  /* Functions  for   MMEC  numbers  whose   internal  representation  is   an  Emacs
     user-pointer object of type `ldouble'. */
  {
    .name		= "mmec-c-ldouble-add",
    .implementation	= Fmmec_c_ldouble_add,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 + OP2, where the operands are user-pointer objects of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-sub",
    .implementation	= Fmmec_c_ldouble_sub,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 - OP2, where the operands are user-pointer objects of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-mul",
    .implementation	= Fmmec_c_ldouble_mul,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 * OP2, where the operands are user-pointer objects of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-div",
    .implementation	= Fmmec_c_ldouble_div,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 / OP2, where the operands are user-pointer objects of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-mod",
    .implementation	= Fmmec_c_ldouble_mod,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute mod(DIVIDEND, DIVISOR), where the operands are user-pointer objects of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-neg",
    .implementation	= Fmmec_c_ldouble_neg,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute - OP, where the operand is a user-pointer object of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-inv",
    .implementation	= Fmmec_c_ldouble_inv,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 1 / OP, where the operand is a user-pointer object of type `ldouble'.",
  },

  /* Functions  for   MMEC  numbers  whose   internal  representation  is   an  Emacs
     user-pointer object of type `sint64'. */
  {
    .name		= "mmec-c-sint64-add",
    .implementation	= Fmmec_c_sint64_add,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 + OP2, where the operands are user-pointer objects of type `sint64'.",
  },
  {
    .name		= "mmec-c-sint64-sub",
    .implementation	= Fmmec_c_sint64_sub,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 - OP2, where the operands are user-pointer objects of type `sint64'.",
  },
  {
    .name		= "mmec-c-sint64-mul",
    .implementation	= Fmmec_c_sint64_mul,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 * OP2, where the operands are user-pointer objects of type `sint64'.",
  },
  {
    .name		= "mmec-c-sint64-div",
    .implementation	= Fmmec_c_sint64_div,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 / OP2, where the operands are user-pointer objects of type `sint64'.",
  },
  {
    .name		= "mmec-c-sint64-mod",
    .implementation	= Fmmec_c_sint64_mod,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute mod(DIVIDEND, DIVISOR), where the operands are user-pointer objects of type `sint64'.",
  },
  {
    .name		= "mmec-c-sint64-percent",
    .implementation	= Fmmec_c_sint64_percent,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute DIVIDEND %% DIVISOR, where the operands are user-pointer objects of type `sint64'.",
  },
  {
    .name		= "mmec-c-sint64-neg",
    .implementation	= Fmmec_c_sint64_neg,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute - OP, where the operand is a user-pointer object of type `sint64'.",
  },
  {
    .name		= "mmec-c-sint64-inv",
    .implementation	= Fmmec_c_sint64_inv,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 1 / OP, where the operand is a user-pointer object of type `sint64'.",
  },

  /* Functions  for   MMEC  numbers  whose   internal  representation  is   an  Emacs
     user-pointer object of type `uint64'. */
  {
    .name		= "mmec-c-uint64-add",
    .implementation	= Fmmec_c_uint64_add,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 + OP2, where the operands are user-pointer objects of type `uint64'.",
  },
  {
    .name		= "mmec-c-uint64-sub",
    .implementation	= Fmmec_c_uint64_sub,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 - OP2, where the operands are user-pointer objects of type `uint64'.",
  },
  {
    .name		= "mmec-c-uint64-mul",
    .implementation	= Fmmec_c_uint64_mul,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 * OP2, where the operands are user-pointer objects of type `uint64'.",
  },
  {
    .name		= "mmec-c-uint64-div",
    .implementation	= Fmmec_c_uint64_div,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute OP1 / OP2, where the operands are user-pointer objects of type `uint64'.",
  },
  {
    .name		= "mmec-c-uint64-mod",
    .implementation	= Fmmec_c_uint64_mod,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute mod(DIVIDEND, DIVISOR), where the operands are user-pointer objects of type `uint64'.",
  },
  {
    .name		= "mmec-c-uint64-percent",
    .implementation	= Fmmec_c_uint64_percent,
    .min_arity		= 2,
    .max_arity		= 2,
    .documentation	= "Compute DIVIDEND %% DIVISOR, where the operands are user-pointer objects of type `uint64'.",
  },
  {
    .name		= "mmec-c-uint64-neg",
    .implementation	= Fmmec_c_uint64_neg,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute - OP, where the operand is a user-pointer object of type `uint64'.",
  },
  {
    .name		= "mmec-c-uint64-inv",
    .implementation	= Fmmec_c_uint64_inv,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute 1 / OP, where the operand is a user-pointer object of type `uint64'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: parts.
 ** ----------------------------------------------------------------- */

/* 6 operations for each  stem in: float, double, ldouble; 1  operation for each stem
   in: sint64, uint64. */
#define NUMBER_OF_MODULE_PARTS_FUNCTIONS	(6 * 3 + 2)
static mmec_module_function_t const
  module_parts_functions_table[NUMBER_OF_MODULE_PARTS_FUNCTIONS] = {
  /* Functions for  MMEC numbers whose  internal representation is an  Emacs built-in
     value of type `float'. */
  {
    .name		= "mmec-c-double-abs",
    .implementation	= Fmmec_c_double_abs,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute abs(OP), where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-ceil",
    .implementation	= Fmmec_c_double_ceil,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute ceil(OP), where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-floor",
    .implementation	= Fmmec_c_double_floor,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute floor(OP), where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-rint",
    .implementation	= Fmmec_c_double_rint,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute rint(OP), where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-trunc",
    .implementation	= Fmmec_c_double_trunc,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute trunc(OP), where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-double-round",
    .implementation	= Fmmec_c_double_round,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute round(OP), where the operand is a number value of type `float'.",
  },

  /* Functions  for MMEC  numbers  whose internal  representation  is a  user-pointer
     object of type `float'. */
  {
    .name		= "mmec-c-float-abs",
    .implementation	= Fmmec_c_float_abs,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute abs(OP), where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-float-ceil",
    .implementation	= Fmmec_c_float_ceil,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute ceil(OP), where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-float-floor",
    .implementation	= Fmmec_c_float_floor,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute floor(OP), where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-float-rint",
    .implementation	= Fmmec_c_float_rint,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute rint(OP), where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-float-trunc",
    .implementation	= Fmmec_c_float_trunc,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute trunc(OP), where the operand is a number value of type `float'.",
  },
  {
    .name		= "mmec-c-float-round",
    .implementation	= Fmmec_c_float_round,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute round(OP), where the operand is a number value of type `float'.",
  },

  /* Functions  for MMEC  numbers  whose internal  representation  is a  user-pointer
     object of type `ldouble'. */
  {
    .name		= "mmec-c-ldouble-abs",
    .implementation	= Fmmec_c_ldouble_abs,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute abs(OP), where the operand is a number value of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-ceil",
    .implementation	= Fmmec_c_ldouble_ceil,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute ceil(OP), where the operand is a number value of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-floor",
    .implementation	= Fmmec_c_ldouble_floor,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute floor(OP), where the operand is a number value of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-rint",
    .implementation	= Fmmec_c_ldouble_rint,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute rint(OP), where the operand is a number value of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-trunc",
    .implementation	= Fmmec_c_ldouble_trunc,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute trunc(OP), where the operand is a number value of type `ldouble'.",
  },
  {
    .name		= "mmec-c-ldouble-round",
    .implementation	= Fmmec_c_ldouble_round,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute round(OP), where the operand is a number value of type `ldouble'.",
  },

  /* Functions  for MMEC  numbers  whose internal  representation  is a  user-pointer
     object of type `sint64'. */
  {
    .name		= "mmec-c-sint64-abs",
    .implementation	= Fmmec_c_sint64_abs,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute abs(OP), where the operand is a number value of type `sint64'.",
  },

  /* Functions  for MMEC  numbers  whose internal  representation  is a  user-pointer
     object of type `uint64'. */
  {
    .name		= "mmec-c-uint64-abs",
    .implementation	= Fmmec_c_uint64_abs,
    .min_arity		= 1,
    .max_arity		= 1,
    .documentation	= "Compute abs(OP), where the operand is a number value of type `uint64'.",
  },
};


/** --------------------------------------------------------------------
 ** Elisp functions table: exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS	(17 * 3)
static mmec_module_function_t const
  module_exponentiation_and_logarithms_functions_table[NUMBER_OF_MODULE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS] = {
  /* Functions for  MMEC numbers whose  internal representation is an  Emacs built-in
     value of type `float'. */
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

  /* Functions  for   MMEC  numbers  whose   internal  representation  is   an  Emacs
     user-pointer object of type `float'. */
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

  /* Functions  for   MMEC  numbers  whose   internal  representation  is   an  Emacs
     user-pointer object of type `ldouble'. */
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
 ** Elisp functions table: trigonometric functions.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_TRIGONOMETRIC_FUNCTIONS	(7 * 3)
static mmec_module_function_t const module_trigonometric_functions_table[NUMBER_OF_MODULE_TRIGONOMETRIC_FUNCTIONS] = {
  /* Functions for  MMEC numbers whose  internal representation is an  Emacs built-in
     value of type `float'. */
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

  /* Functions  for   MMEC  numbers  whose   internal  representation  is   an  Emacs
     user-pointer object of type `float'. */
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

  /* Functions  for   MMEC  numbers  whose   internal  representation  is   an  Emacs
     user-pointer object of type `ldouble'. */
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
 ** Elisp functions table: hyperbolic functions.
 ** ----------------------------------------------------------------- */

#define NUMBER_OF_MODULE_HYPERBOLIC_FUNCTIONS	(6 * 3)
static mmec_module_function_t const module_hyperbolic_functions_table[NUMBER_OF_MODULE_HYPERBOLIC_FUNCTIONS] = {
  /* Functions for  MMEC numbers whose  internal representation is an  Emacs built-in
     value of type `float'. */
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

  /* Functions  for   MMEC  numbers  whose   internal  representation  is   an  Emacs
     user-pointer object of type `float'. */
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

  /* Functions  for   MMEC  numbers  whose   internal  representation  is   an  Emacs
     user-pointer object of type `ldouble'. */
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
  mmec_define_elisp_functions_from_table(env, module_arithmetics_functions_table,
					 NUMBER_OF_MODULE_ARITHMETICS_FUNCTIONS, 0);
  mmec_define_elisp_functions_from_table(env, module_parts_functions_table,
					 NUMBER_OF_MODULE_PARTS_FUNCTIONS, 0);
  mmec_define_elisp_functions_from_table(env, module_exponentiation_and_logarithms_functions_table,
					 NUMBER_OF_MODULE_EXPONENTIATION_AND_LOGARITHMS_FUNCTIONS, 0);
  mmec_define_elisp_functions_from_table(env, module_trigonometric_functions_table,
					 NUMBER_OF_MODULE_TRIGONOMETRIC_FUNCTIONS, 0);
  mmec_define_elisp_functions_from_table(env, module_hyperbolic_functions_table,
					 NUMBER_OF_MODULE_HYPERBOLIC_FUNCTIONS, 0);
}

/* end of file */
