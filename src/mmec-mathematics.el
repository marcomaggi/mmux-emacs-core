;;; mmec-mathematics.el --- methematics operations

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Sun Mar  1, 2020
;; Time-stamp: <2020-03-02 17:37:03 marco>
;; Keywords: extensions, lisp

;; This file is part of MMUX Emacs Core.
;;
;; This program is  free software: you can redistribute  it and/or modify it under the  terms of the
;; GNU General Public License as published by the  Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied  warranty of MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.
;;
;; You should have  received a copy of the  GNU General Public License along with  this program.  If
;; not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:

;;

;;; Change Log:

;;

;;; Code:

(require 'mmec-number-objects)


;;;; arithmetics generic functions

(cl-defun mmec-add (&rest ops)
  (cond ((null ops)
	 0)
	((null (cdr ops))
	 (car ops))
	(t
	 (cl-loop for rv =  (car ops) then (mmec-add-2 rv op)
		  for op in (cdr ops)
		  finally (return rv)))))

(cl-defun mmec-sub (&rest ops)
  (cond ((null ops)
	 0)
	((null (cdr ops))
	 (mmec-neg (car ops)))
	(t
	 (cl-loop for rv =  (car ops) then (mmec-sub-2 rv op)
		  for op in (cdr ops)
		  finally (return rv)))))

(cl-defun mmec-mul (&rest ops)
  (cond ((null ops)
	 1)
	((null (cdr ops))
	 (car ops))
	(t
	 (cl-loop for rv =  (car ops) then (mmec-mul-2 rv op)
		  for op in (cdr ops)
		  finally (return rv)))))

(cl-defun mmec-div (&rest ops)
  (cond ((null ops)
	 1)
	((null (cdr ops))
	 (mmec-inverse (car ops)))
	(t
	 (cl-loop for rv =  (car ops) then (mmec-div-2 rv op)
		  for op in (cdr ops)
		  finally (return rv)))))

;;; --------------------------------------------------------------------

(cl-defgeneric mmec-add-2 (op1 op2)
  "Add two numbers.")

(cl-defgeneric mmec-sub-2 (op1 op2)
  "Subtract two numbers.")

(cl-defgeneric mmec-mul-2 (op1 op2)
  "Multiply two numbers.")

(cl-defgeneric mmec-div-2 (op1 op2)
  "Divide two numbers.")

(cl-defgeneric mmec-neg (op)
  "Return the result of negating OP.")

(cl-defgeneric mmec-inverse (op)
  "Return the result of 1/OP.")

(cl-defgeneric mmec-mod (dividend divisor)
  "Return mod(DIVIDEND, DIVISOR).")

(cl-defgeneric mmec-% (dividend divisor)
  "Return DIVIDEND % DIVISOR.")


;;;; exponentiation and logarithms generic functions

(cl-defgeneric mmec-square (X)
  "Return the square of X.")

(cl-defgeneric mmec-cube (X)
  "Return the cube of X.")

(cl-defgeneric mmec-pow (X Y)
  "Return the Yth power of X.")

(cl-defgeneric mmec-sqrt (X)
  "Return the square root X.")

(cl-defgeneric mmec-cbrt (X)
  "Return the cubic root X.")

(cl-defgeneric mmec-root (X Y)
  "Return the Yth root of X.")

(cl-defgeneric mmec-hypot (X Y)
  "Return sqrt(X*X + Y*Y).")

(cl-defgeneric mmec-expm1 (X Y)
  "Return exp(X - 1).")

(cl-defgeneric mmec-log1p (X Y)
  "Return log(1 + X).")

(cl-defgeneric mmec-exp (X)
  "Return the base of logarithm numbers raised to the power of X.")

(cl-defgeneric mmec-exp2 (X)
  "Return 2 raised to the power of X.")

(cl-defgeneric mmec-exp10 (X)
  "Return 10 raised to the power of X.")

(cl-defgeneric mmec-log (X)
  "Return the natural logarithm of X.")

(cl-defgeneric mmec-log2 (X)
  "Return the base-2 logarithm of X.")

(cl-defgeneric mmec-log10 (X)
  "Return the base-10 logarithm of X.")

(cl-defgeneric mmec-logb (X)
  "Extract the exponent of X and return it.")


;;;; trigonometric generic functions

(cl-defgeneric mmec-sin (X)
  "Return the trigonometric sine of X.")

(cl-defgeneric mmec-cos (X)
  "Return the trigonometric cosine of X.")

(cl-defgeneric mmec-tan (X)
  "Return the trigonometric tangent of X.")

(cl-defgeneric mmec-asin (X)
  "Return the trigonometric arc sine of X.")

(cl-defgeneric mmec-acos (X)
  "Return the trigonometric arc cosine of X.")

(cl-defgeneric mmec-atan (X)
  "Return the trigonometric arc tangent of X.")

(cl-defgeneric mmec-atan2 (X Y)
  "Return the trigonometric arc tangent of Y/X.")


;;;; hyperbolic generic functions

(cl-defgeneric mmec-sinh (X)
  "Return the hyperbolic sine of X.")

(cl-defgeneric mmec-cosh (X)
  "Return the hyperbolic cosine of X.")

(cl-defgeneric mmec-tanh (X)
  "Return the hyperbolic tangent of X.")

(cl-defgeneric mmec-asinh (X)
  "Return the hyperbolic inverse sine of X.")

(cl-defgeneric mmec-acosh (X)
  "Return the hyperbolic inverse cosine of X.")

(cl-defgeneric mmec-atanh (X)
  "Return the hyperbolic inverse tangent of X.")


;;;; arithmetics methods

(cl-macrolet
    ((mmec--def (FUNCSTEM ELISPFUNC)
		(let ((FUNCNAME		(mmec-sformat "mmec-%s-2" FUNCSTEM)))
		  `(progn
		     (cl-defmethod ,FUNCNAME ((op1 integer) (op2 integer))
		       (,ELISPFUNC op1 op2))

		     (cl-defmethod ,FUNCNAME ((op1 float) (op2 float))
		       (,ELISPFUNC op1 op2))

		     (cl-defmethod ,FUNCNAME ((op1 integer) (op2 float))
		       (,ELISPFUNC op1 op2))

		     (cl-defmethod ,FUNCNAME ((op1 float) (op2 integer))
		       (,ELISPFUNC op1 op2))
		     ))))
  (mmec--def add	+)
  (mmec--def sub	-)
  (mmec--def mul	*)
  (mmec--def div	/))

(cl-macrolet
    ((mmec--defop2 (FUNCSTEM TYPESTEM NORMSTEM)
		   (let* ((NUMTYPE	(mmec-sformat "mmec-%s"   TYPESTEM))
			  (FUNCNAME	(mmec-sformat "mmec-%s-2" FUNCSTEM))
			  (NORMTYPE	(mmec-sformat "mmec-%s"   NORMSTEM)))
		     `(progn
			(cl-defmethod ,FUNCNAME ((op1 integer) (op2 ,NUMTYPE))
			  (mmec--make ,NUMTYPE :obj (,FUNCNAME (,NORMTYPE op1) (,NORMTYPE op2))))
			(cl-defmethod ,FUNCNAME ((op1 ,NUMTYPE) (op2 integer))
			  (mmec--make ,NUMTYPE :obj (,FUNCNAME (,NORMTYPE op1) (,NORMTYPE op2))))
			(cl-defmethod ,FUNCNAME ((op1 ,NUMTYPE) (op2 ,NUMTYPE))
			  (mmec--make ,NUMTYPE :obj (,FUNCNAME (,NORMTYPE op1) (,NORMTYPE op2))))
			)))

     (mmec--defarith (TYPESTEM NORMSTEM)
		     `(progn
			(mmec--defop2 add ,TYPESTEM ,NORMSTEM)
			(mmec--defop2 sub ,TYPESTEM ,NORMSTEM)
			(mmec--defop2 mul ,TYPESTEM ,NORMSTEM)
			(mmec--defop2 div ,TYPESTEM ,NORMSTEM)))

     (mmec--defsint64 (TYPESTEM)
		      `(mmec--defarith ,TYPESTEM sint64))
     (mmec--defuint64 (TYPESTEM)
		      `(mmec--defarith ,TYPESTEM uint64))
     (mmec--defldouble (TYPESTEM)
		       `(mmec--defarith ,TYPESTEM ldouble)))

  (mmec--defsint64	char)
  (mmec--defsint64	schar)
  (mmec--defuint64	uchar)
  (mmec--defuint64	wchar)
  (mmec--defsint64	sshrt)
  (mmec--defuint64	ushrt)
  (mmec--defsint64	sint)
  (mmec--defuint64	uint)
  (mmec--defsint64	slong)
  (mmec--defuint64	ulong)
  (mmec--defsint64	sllong)
  (mmec--defuint64	ullong)
  (mmec--defsint64	ssize)
  (mmec--defuint64	usize)
  (mmec--defsint64	sintmax)
  (mmec--defuint64	uintmax)
  (mmec--defsint64	ptrdiff)
  (mmec--defsint64	sint8)
  (mmec--defuint64	uint8)
  (mmec--defsint64	sint16)
  (mmec--defuint64	uint16)
  (mmec--defsint64	sint32)
  (mmec--defuint64	uint32)
  (mmec--defldouble	float)
  (mmec--defldouble	double))

;;; --------------------------------------------------------------------

(cl-macrolet
    ((mmec--defop2 (FUNCSTEM TYPESTEM)
		   (let* ((NUMTYPE	(mmec-sformat "mmec-%s" TYPESTEM))
			  (FUNCNAME	(mmec-sformat "mmec-%s-2" FUNCSTEM))
			  (CFUNC	(mmec-sformat "mmec-c-%s-%s" TYPESTEM FUNCSTEM)))
		     `(progn
			(cl-defmethod ,FUNCNAME ((op1 integer) (op2 ,NUMTYPE))
			  (mmec--make ,NUMTYPE
				      :obj (,CFUNC (mmec--extract-obj ,NUMTYPE (,NUMTYPE op1))
						   (mmec--extract-obj ,NUMTYPE op2))))

			(cl-defmethod ,FUNCNAME ((op1 ,NUMTYPE) (op2 integer))
			  (mmec--make ,NUMTYPE
				      :obj (,CFUNC (mmec--extract-obj ,NUMTYPE op1)
						   (mmec--extract-obj ,NUMTYPE (,NUMTYPE op2)))))

			(cl-defmethod ,FUNCNAME ((op1 float) (op2 ,NUMTYPE))
			  (mmec--make ,NUMTYPE
				      :obj (,CFUNC (mmec--extract-obj ,NUMTYPE (,NUMTYPE op1))
						   (mmec--extract-obj ,NUMTYPE op2))))

			(cl-defmethod ,FUNCNAME ((op1 ,NUMTYPE) (op2 float))
			  (mmec--make ,NUMTYPE
				      :obj (,CFUNC (mmec--extract-obj ,NUMTYPE op1)
						   (mmec--extract-obj ,NUMTYPE (,NUMTYPE op2)))))

			(cl-defmethod ,FUNCNAME ((op1 ,NUMTYPE) (op2 ,NUMTYPE))
			  (mmec--make ,NUMTYPE
				      :obj (,CFUNC (mmec--extract-obj ,NUMTYPE op1)
						   (mmec--extract-obj ,NUMTYPE op2))))
			)))

     (mmec--def (TYPESTEM)
		`(progn
		   (mmec--defop2 add ,TYPESTEM)
		   (mmec--defop2 sub ,TYPESTEM)
		   (mmec--defop2 mul ,TYPESTEM)
		   (mmec--defop2 div ,TYPESTEM))))

  (mmec--def sint64)
  (mmec--def uint64)
  (mmec--def ldouble))

;;; --------------------------------------------------------------------

(cl-defmethod mmec-neg ((op float))
  (- op))

(cl-defmethod mmec-neg ((op integer))
  (- op))

(cl-macrolet
    ((mmec--def (TYPESTEM NORMSTEM)
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (NORMTYPE	(mmec-sformat "mmec-%s" NORMSTEM)))
		  `(cl-defmethod mmec-neg ((op ,NUMTYPE))
		     (,NUMTYPE (mmec-neg (,NORMTYPE op))))))
     (mmec--defsint64 (TYPESTEM)
		      `(mmec--def ,TYPESTEM sint64))
     (mmec--defuint64 (TYPESTEM)
		      `(mmec--def ,TYPESTEM uint64)))
  (mmec--defsint64	char)
  (mmec--defsint64	schar)
  (mmec--defuint64	uchar)
  (mmec--defuint64	wchar)
  (mmec--defsint64	sshrt)
  (mmec--defuint64	ushrt)
  (mmec--defsint64	sint)
  (mmec--defuint64	uint)
  (mmec--defsint64	slong)
  (mmec--defuint64	ulong)
  (mmec--defsint64	sllong)
  (mmec--defuint64	ullong)
  (mmec--defsint64	ssize)
  (mmec--defuint64	usize)
  (mmec--defsint64	sintmax)
  (mmec--defuint64	uintmax)
  (mmec--defsint64	ptrdiff)
  (mmec--defsint64	sint8)
  (mmec--defuint64	uint8)
  (mmec--defsint64	sint16)
  (mmec--defuint64	uint16)
  (mmec--defsint64	sint32)
  (mmec--defuint64	uint32))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s"       TYPESTEM))
		       (CFUNC		(mmec-sformat "mmec-c-%s-neg" TYPESTEM)))
		  `(cl-defmethod mmec-neg ((op ,NUMTYPE))
		     (mmec--make ,TYPESTEM :obj (,CFUNC (mmec--extract-obj ,TYPESTEM op)))))))
  (mmec--def sint64)
  (mmec--def uint64)
  (mmec--def float)
  (mmec--def double)
  (mmec--def ldouble))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s"       TYPESTEM))
		       (CFUNC		(mmec-sformat "mmec-c-%s-inv" TYPESTEM)))
		  `(cl-defmethod mmec-inverse ((op ,NUMTYPE))
		     (mmec--make ,TYPESTEM :obj (,CFUNC (mmec--extract-obj ,TYPESTEM op)))))))
  (mmec--def sint64)
  (mmec--def uint64)
  (mmec--def float)
  (mmec--def double)
  (mmec--def ldouble))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s"       TYPESTEM))
		       (CFUNC		(mmec-sformat "mmec-c-%s-mod" TYPESTEM)))
		  `(cl-defmethod mmec-mod ((dividend ,NUMTYPE) (divisor ,NUMTYPE))
		     (mmec--make ,TYPESTEM :obj (,CFUNC (mmec--extract-obj ,TYPESTEM dividend)
							(mmec--extract-obj ,TYPESTEM divisor)))))))
  (mmec--def sint64)
  (mmec--def uint64)
  (mmec--def float)
  (mmec--def double)
  (mmec--def ldouble))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s"           TYPESTEM))
		       (CFUNC		(mmec-sformat "mmec-c-%s-percent" TYPESTEM)))
		  `(cl-defmethod mmec-% ((dividend ,NUMTYPE) (divisor ,NUMTYPE))
		     (mmec--make ,TYPESTEM :obj (,CFUNC (mmec--extract-obj ,TYPESTEM dividend)
							(mmec--extract-obj ,TYPESTEM divisor)))))))
  (mmec--def sint64)
  (mmec--def uint64))


;;;; other methods

(cl-macrolet
    ((mmec--func1 (FUNCSTEM TYPESTEM)
		  (let* ((TYPENAME	(mmec-sformat "mmec-%s" TYPESTEM))
			 (EFUNCNAME	(mmec-sformat "mmec-%s" FUNCSTEM))
			 (CFUNCNAME	(mmec-sformat "mmec-c-%s-%s" TYPESTEM FUNCSTEM)))
		    `(cl-defmethod ,EFUNCNAME ((op1 ,TYPENAME))
		       (mmec--make ,TYPENAME :obj (,CFUNCNAME (mmec--extract-obj ,TYPENAME op1))))))
     (mmec--func2 (FUNCSTEM TYPESTEM)
		  (let* ((TYPENAME	(mmec-sformat "mmec-%s" TYPESTEM))
			 (EFUNCNAME	(mmec-sformat "mmec-%s" FUNCSTEM))
			 (CFUNCNAME	(mmec-sformat "mmec-c-%s-%s" TYPESTEM FUNCSTEM)))
		    `(cl-defmethod ,EFUNCNAME ((op1 ,TYPENAME) (op2 ,TYPENAME))
		       (mmec--make ,TYPENAME :obj (,CFUNCNAME (mmec--extract-obj ,TYPENAME op1)
							      (mmec--extract-obj ,TYPENAME op2)))))))

  (cl-macrolet
      ((mmec--def (TYPESTEM)
  		  `(progn
  		     (mmec--func1 square	,TYPESTEM)
  		     (mmec--func1 cube		,TYPESTEM)
  		     (mmec--func2 pow		,TYPESTEM)
  		     (mmec--func1 sqrt		,TYPESTEM)
  		     (mmec--func1 cbrt		,TYPESTEM)
  		     (mmec--func2 root		,TYPESTEM)
  		     (mmec--func2 hypot		,TYPESTEM)
  		     (mmec--func1 expm1		,TYPESTEM)
  		     (mmec--func1 log1p		,TYPESTEM)
  		     (mmec--func1 exp		,TYPESTEM)
  		     (mmec--func1 exp2		,TYPESTEM)
  		     (mmec--func1 exp10		,TYPESTEM)
  		     (mmec--func1 log		,TYPESTEM)
  		     (mmec--func1 log2		,TYPESTEM)
  		     (mmec--func1 log10		,TYPESTEM)
  		     (mmec--func1 logb		,TYPESTEM))))
    (mmec--def float)
    (mmec--def double)
    (mmec--def ldouble))

  (cl-macrolet
      ((mmec--def (TYPESTEM)
  		  `(progn
  		     (mmec--func1 sin	,TYPESTEM)
  		     (mmec--func1 cos	,TYPESTEM)
  		     (mmec--func1 tan	,TYPESTEM)
  		     (mmec--func1 asin	,TYPESTEM)
  		     (mmec--func1 acos	,TYPESTEM)
  		     (mmec--func1 atan	,TYPESTEM)
  		     (mmec--func2 atan2	,TYPESTEM))))
    (mmec--def float)
    (mmec--def double)
    (mmec--def ldouble))

  (cl-macrolet
      ((mmec--def (TYPESTEM)
		  `(progn
		     (mmec--func1 sinh	,TYPESTEM)
		     (mmec--func1 cosh	,TYPESTEM)
		     (mmec--func1 tanh	,TYPESTEM)
		     (mmec--func1 asin	,TYPESTEM)
		     (mmec--func1 acosh	,TYPESTEM)
		     (mmec--func1 atanh	,TYPESTEM))))
    (mmec--def float)
    (mmec--def double)
    (mmec--def ldouble)))


;;;; done

(provide 'mmec-mathematics)

;;; mmec-mathematics.el ends here
