;;; mmec-mathematics.el --- methematics operations

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Sun Mar  1, 2020
;; Time-stamp: <2020-03-01 17:18:26 marco>
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


;;;; arithmetics

(cl-defun mmec-add (op1 &rest ops)
  (if (null ops)
      op1
    (cl-loop with rv = op1
	     for op in ops
	     do (setq rv (mmec-add-2 rv op))
	     finally (return rv))))

(cl-defun mmec-sub (op1 &rest ops)
  (if (null ops)
      (mmec-neg op1)
    (cl-loop with rv = op1
	     for op in ops
	     do (setq rv (mmec-sub-2 rv op))
	     finally (return rv))))

(cl-defun mmec-mul (op1 &rest ops)
  (if (null ops)
      op1
    (cl-loop with rv = op1
	     for op in ops
	     do (setq rv (mmec-mul-2 rv op))
	     finally (return rv))))

(cl-defun mmec-div (op1 &rest ops)
  (if (null ops)
      (mmec-inv op1)
    (cl-loop with rv = op1
	     for op in ops
	     do (setq rv (mmec-div-2 rv op))
	     finally (return rv))))

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

(cl-defgeneric mmec-inv (op)
  "Return the result of 1/OP.")

(cl-defgeneric mmec-mod (dividend divisor)
  "Return the value of DIVIDEND modulo DIVISOR.")


;;;; exponentiation and logarithms

(cl-defgeneric mmec-square (X)
  "Return the square of X.")

(cl-defgeneric mmec-cube (X)
  "Return the cube of X.")

(cl-defgeneric mmec-expt (X Y)
  "Return the Yth power of X.")

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


;;;; trigonometric functions

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


;;;; hyperbolic functions

(cl-defgeneric mmec-sin (X)
  "Return the hyperbolic sine of X.")

(cl-defgeneric mmec-cos (X)
  "Return the hyperbolic cosine of X.")

(cl-defgeneric mmec-tan (X)
  "Return the hyperbolic tangent of X.")

(cl-defgeneric mmec-asin (X)
  "Return the hyperbolic inverse sine of X.")

(cl-defgeneric mmec-acos (X)
  "Return the hyperbolic inverse cosine of X.")

(cl-defgeneric mmec-atan (X)
  "Return the hyperbolic inverse tangent of X.")


;;;; done

(provide 'mmec-mathematics)

;;; mmec-mathematics.el ends here
