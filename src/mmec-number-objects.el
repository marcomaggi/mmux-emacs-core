;;; mmec-number-objects.el --- numeric type definitions for C language intefaces

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-03-05 18:42:18 marco>
;; Keywords: extensions

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

(require 'mmec-basics)


;;;; helpers

(defun mmec--assert-positive-number (num funcname)
  (when (mmec-negative-p num)
    (signal 'mmec-error-value-out-of-range (list funcname num))))


;;;; API macros

(defmacro mmec-limit-min (TYPE-OR-STEM)
  "Expand into the constant name representing the minimum limit in the range of the specified number object"
  (let ((STEM	(intern (mmec--strip-prefix-from-symbol-name TYPE-OR-STEM))))
    (cl-case STEM
      (char	'mmec-char-min)
      (schar	'mmec-schar-min)
      (uchar	'mmec-uchar-min)
      (wchar	'mmec-wchar-min)
      (sshrt	'mmec-sshrt-min)
      (ushrt	'mmec-ushrt-min)
      (sint	'mmec-sint-min)
      (uint	'mmec-uint-min)
      (slong	'mmec-slong-min)
      (ulong	'mmec-ulong-min)
      (sllong	'mmec-sllong-min)
      (ullong	'mmec-ullong-min)
      (ssize	'mmec-ssize-min)
      (usize	'mmec-usize-min)
      (sintmax	'mmec-sintmax-min)
      (uintmax	'mmec-uintmax-min)
      (ptrdiff	'mmec-ptrdiff-min)
      (sint8	'mmec-sint8-min)
      (uint8	'mmec-uint8-min)
      (sint16	'mmec-sint16-min)
      (uint16	'mmec-uint16-min)
      (sint32	'mmec-sint32-min)
      (uint32	'mmec-uint32-min)
      (sint64	'mmec-sint64-min)
      (uint64	'mmec-uint64-min)
      (float	'mmec-float-min)
      (double	'mmec-double-min)
      (ldouble	'mmec-ldouble-min)
      (t
       (signal 'mmec-error-unknown-number-object-type-or-stem (list 'mmec-limits-min TYPE-OR-STEM))))))

(defmacro mmec-limit-max (TYPE-OR-STEM)
  "Expand into the constant name representing the maximum limit in the range of the specified number object"
  (let ((STEM	(intern (mmec--strip-prefix-from-symbol-name TYPE-OR-STEM))))
    (cl-case STEM
      (char	'mmec-char-max)
      (schar	'mmec-schar-max)
      (uchar	'mmec-uchar-max)
      (wchar	'mmec-wchar-max)
      (sshrt	'mmec-sshrt-max)
      (ushrt	'mmec-ushrt-max)
      (sint	'mmec-sint-max)
      (uint	'mmec-uint-max)
      (slong	'mmec-slong-max)
      (ulong	'mmec-ulong-max)
      (sllong	'mmec-sllong-max)
      (ullong	'mmec-ullong-max)
      (ssize	'mmec-ssize-max)
      (usize	'mmec-usize-max)
      (sintmax	'mmec-sintmax-max)
      (uintmax	'mmec-uintmax-max)
      (ptrdiff	'mmec-ptrdiff-max)
      (sint8	'mmec-sint8-max)
      (uint8	'mmec-uint8-max)
      (sint16	'mmec-sint16-max)
      (uint16	'mmec-uint16-max)
      (sint32	'mmec-sint32-max)
      (uint32	'mmec-uint32-max)
      (sint64	'mmec-sint64-max)
      (uint64	'mmec-uint64-max)
      (float	'mmec-float-max)
      (double	'mmec-double-max)
      (ldouble	'mmec-ldouble-max)
      (t
       (signal 'mmec-error-unknown-number-object-type-or-stem (list 'mmec-limits-max TYPE-OR-STEM))))))

(defmacro mmec-sizeof-number-type (TYPE-OR-STEM)
  "Expand into the constant name representing the number of bytes used to represent a C language type.

(mmec-sizeof-number-type char)		==> mmec-sizeof-char
(mmec-sizeof-number-type mmec-ulong)	==> mmec-sizeof-ulong"
  (let ((STEM	(intern (mmec--strip-prefix-from-symbol-name TYPE-OR-STEM))))
    (cl-case STEM
      (char	'mmec-sizeof-char)
      (schar	'mmec-sizeof-schar)
      (uchar	'mmec-sizeof-uchar)
      (wchar	'mmec-sizeof-wchar)
      (sshrt	'mmec-sizeof-sshrt)
      (ushrt	'mmec-sizeof-ushrt)
      (sint	'mmec-sizeof-sint)
      (uint	'mmec-sizeof-uint)
      (slong	'mmec-sizeof-slong)
      (ulong	'mmec-sizeof-ulong)
      (sllong	'mmec-sizeof-sllong)
      (ullong	'mmec-sizeof-ullong)
      (ssize	'mmec-sizeof-ssize)
      (usize	'mmec-sizeof-usize)
      (sintmax	'mmec-sizeof-sintmax)
      (uintmax	'mmec-sizeof-uintmax)
      (ptrdiff	'mmec-sizeof-ptrdiff)
      (sint8	'mmec-sizeof-sint8)
      (uint8	'mmec-sizeof-uint8)
      (sint16	'mmec-sizeof-sint16)
      (uint16	'mmec-sizeof-uint16)
      (sint32	'mmec-sizeof-sint32)
      (uint32	'mmec-sizeof-uint32)
      (sint64	'mmec-sizeof-sint64)
      (uint64	'mmec-sizeof-uint64)
      (float	'mmec-sizeof-float)
      (double	'mmec-sizeof-double)
      (ldouble	'mmec-sizeof-ldouble)
      (t
       (signal 'mmec-error-unknown-number-object-type-or-stem (list 'mmec-sizeof-number-type TYPE-OR-STEM))))))

(defmacro mmec-number-type-p (TYPE-OR-STEM EXPR)
  "Expand into a form returning true if EXPR is of the specified number type."
  (let* ((STEM		(intern (mmec--strip-prefix-from-symbol-name TYPE-OR-STEM)))
	 (PRED		(mmec-sformat "mmec-%s-p" STEM))
	 (PREDFORM	`(,PRED ,EXPR)))
    (cl-case STEM
      (char	PREDFORM)
      (schar	PREDFORM)
      (uchar	PREDFORM)
      (wchar	PREDFORM)
      (sshrt	PREDFORM)
      (ushrt	PREDFORM)
      (sint	PREDFORM)
      (uint	PREDFORM)
      (slong	PREDFORM)
      (ulong	PREDFORM)
      (sllong	PREDFORM)
      (ullong	PREDFORM)
      (ssize	PREDFORM)
      (usize	PREDFORM)
      (sintmax	PREDFORM)
      (uintmax	PREDFORM)
      (ptrdiff	PREDFORM)
      (sint8	PREDFORM)
      (uint8	PREDFORM)
      (sint16	PREDFORM)
      (uint16	PREDFORM)
      (sint32	PREDFORM)
      (uint32	PREDFORM)
      (sint64	PREDFORM)
      (uint64	PREDFORM)
      (float	PREDFORM)
      (double	PREDFORM)
      (ldouble	PREDFORM)
      (t
       (signal 'mmec-error-unknown-number-object-type-or-stem (list 'mmec-number-type-p TYPE-OR-STEM))))))

(defmacro mmec-fits-number-type-p (TYPE-OR-STEM EXPR)
  "Expand into the application of a struct type predicate to the result of the given expression.

Example:

  (mmec-fits-number-type-p sint obj)
  ==> (mmec-fits-sint-p obj)"
  (let* ((STEM		(intern (mmec--strip-prefix-from-symbol-name TYPE-OR-STEM)))
	 (FITFUNC	(mmec-sformat "mmec-fits-%s-p" STEM))
	 (FORM		`(,FITFUNC ,EXPR)))
    (cl-case STEM
      (char	FORM)
      (schar	FORM)
      (uchar	FORM)
      (wchar	FORM)
      (sshrt	FORM)
      (ushrt	FORM)
      (sint	FORM)
      (uint	FORM)
      (slong	FORM)
      (ulong	FORM)
      (sllong	FORM)
      (ullong	FORM)
      (ssize	FORM)
      (usize	FORM)
      (sintmax	FORM)
      (uintmax	FORM)
      (ptrdiff	FORM)
      (sint8	FORM)
      (uint8	FORM)
      (sint16	FORM)
      (uint16	FORM)
      (sint32	FORM)
      (uint32	FORM)
      (sint64	FORM)
      (uint64	FORM)
      (float	FORM)
      (double	FORM)
      (ldouble	FORM)
      (t
       (signal 'mmec-error-unknown-number-object-type-or-stem (list 'mmec-fits-number-type-p TYPE-OR-STEM))))))

(defmacro mmec-number-type-is-signed-p (TYPE-OR-STEM)
  "Expand to true if TYPE-OR-STEM represents a signed type; expand to false otherwise.

(mmec-number-type-is-signed-p sint)            => t
(mmec-number-type-is-signed-p mmec-ulong)      => nil"
  (let ((STEM (intern (mmec--strip-prefix-from-symbol-name TYPE-OR-STEM))))
    (cl-case STEM
      (char	t)
      (schar	t)
      (uchar	nil)
      (wchar	t)
      (sshrt	t)
      (ushrt	nil)
      (sint	t)
      (uint	nil)
      (slong	t)
      (ulong	nil)
      (sllong	t)
      (ullong	nil)
      (ssize	t)
      (usize	nil)
      (sintmax	t)
      (uintmax	nil)
      (ptrdiff	t)
      (sint8	t)
      (uint8	nil)
      (sint16	t)
      (uint16	nil)
      (sint32	t)
      (uint32	nil)
      (sint64	t)
      (uint64	nil)
      (float	t)
      (double	t)
      (ldouble	t)
      (t
       (signal 'mmec-error-unknown-number-object-type-or-stem (list 'mmec-number-type-is-signed-p TYPE-OR-STEM))))))

(eval-and-compile
  (defun mmec-symbol-number-type-is-signed-p (SYMBOL)
    "Return true if SYMBOL represents a signed type; return false otherwise.

(mmec-symbol-number-type-is-signed-p 'sint)		=> t
(mmec-symbol-number-type-is-signed-p 'mmec-ulong)	=> nil"
    (let ((STEM (intern (mmec--strip-prefix-from-symbol-name SYMBOL))))
      (cl-case STEM
	(char		t)
	(schar		t)
	(uchar		nil)
	(wchar		t)
	(sshrt		t)
	(ushrt		nil)
	(sint		t)
	(uint		nil)
	(slong		t)
	(ulong		nil)
	(sllong		t)
	(ullong		nil)
	(ssize		t)
	(usize		nil)
	(sintmax	t)
	(uintmax	nil)
	(ptrdiff	t)
	(sint8		t)
	(uint8		nil)
	(sint16		t)
	(uint16		nil)
	(sint32		t)
	(uint32		nil)
	(sint64		t)
	(uint64		nil)
	(float		t)
	(double		t)
	(ldouble	t)
	(t
	 (signal 'mmec-error-unknown-number-object-type-or-stem (list 'mmec-symbol-number-type-is-signed-p SYMBOL)))))))


;;;; basic numeric type definitions

;; Base type of all the custom number types defined by this module.
(cl-defstruct (mmec-number
	       (:constructor	mmec-number--make)))

;; Base type of all the custom exact integer types defined by this module.
(cl-defstruct (mmec-integer
	       (:include	mmec-number)
	       (:constructor	mmec-integer--make)))

;; Base type of all the custom exact signed integer number types defined by this module.
(cl-defstruct (mmec-signed-integer
	       (:include	mmec-integer)
	       (:constructor	mmec-signed-integer--make)))

;; Base type of all the custom exact unsigned integer number types defined by this module.
(cl-defstruct (mmec-unsigned-integer
	       (:include	mmec-integer)
	       (:constructor	mmec-unsigned-integer--make)))

;; Base type of all the custom floating-point number types defined by this module.
(cl-defstruct (mmec-floating-point
	       (:include	mmec-number)
	       (:constructor	mmec-floating-point--make)))

;;; --------------------------------------------------------------------

(cl-macrolet ((mmec--define-abstract-type-constructor
	       (TYPE)
	       `(defun ,TYPE (&rest args)
		  (signal 'mmec-error-instantiating-abstract-type (quote ,TYPE)))))
  (mmec--define-abstract-type-constructor mmec-number)
  (mmec--define-abstract-type-constructor mmec-integer)
  (mmec--define-abstract-type-constructor mmec-signed-integer)
  (mmec--define-abstract-type-constructor mmec-unsigned-integer)
  (mmec--define-abstract-type-constructor mmec-floating-point))


;;;; bootstrap macros

(defmacro mmec--define-number-object-constructor-as-generic-function (STEM)
  "Define the constructor generic function for a number object type."
  (let* ((NUMTYPE	(mmec-sformat "mmec-%s" STEM))
	 (MAKER		NUMTYPE)
	 (DOCSTRING	(format "Constructor for number objects of type `%s'.

The argument INIT must be a number value." NUMTYPE)))
    `(cl-defgeneric ,MAKER (init)
       ,DOCSTRING)))

;;; --------------------------------------------------------------------

(defmacro mmec--define-number-object-copy-constructor-as-method (STEM)
  "Define the copy constructor method for a number type.

The copy constructor method specialises the generic function constructor
to accept an argument whose type is the same as the number object type."
  (let* ((NUMTYPE	(mmec-sformat "mmec-%s" STEM))
	 (MAKER		NUMTYPE)
	 (DOCSTRING	(format "Constructor for number objects of type `%s'.

This is the copy constructor implemented as method.  This method creates
a   duplicate  of   the  INIT   value,  but   it  reuses   the  internal
representation (which is immutable)."
				NUMTYPE)))
    `(cl-defmethod ,MAKER ((init ,NUMTYPE))
       ,DOCSTRING
       (mmec--make-obj ,STEM (mmec--extract-obj ,STEM init)))))

;;; --------------------------------------------------------------------

(mmec-defmacro mmec--define-number-object-constructor-as-method-parent-argument (STEM &key INTREP-TYPE PARENT-STEM NORM-STEM)
  "Define a constructor method for a number type.

Thsi constructor method specialises  the generic function constructor to
accept an argument  whose type is the  same as the parent  of the number
object type."
  (let* ((NUMTYPE	(mmec-sformat "mmec-%s" STEM))
	 (PARENT-TYPE	(mmec-sformat "mmec-%s" PARENT-STEM))
	 (NORM-TYPE	(mmec-sformat "mmec-%s" NORM-STEM))
	 (MAKER		NUMTYPE)
	 (NORM-MAKER	NORM-TYPE)
	 (INTREP-SPEC	(cl-case INTREP-TYPE
			  (integer	"elisp-integer")
			  (float	"elisp-float")
			  (usrptr	"usrptr")
			  (t
			   (signal 'mmec-error-invalid-argument (list --func--)))))
	 (CFUNC-CTOR	(mmec-sformat "mmec-c-make-%s-%s-from-usrptr-%s" INTREP-SPEC STEM NORM-STEM))
	 (DOCSTRING	(format "Constructor for number objects of type `%s'.

This constructor normalises the argument INIT to an object of type `%s',
then it checks if  the range is valid; if it is: it  builds an object of
type    `%s',    otherwise    it     raises    the    error    condition
`mmec-error-value-out-of-range'."  NUMTYPE NORM-TYPE NUMTYPE)))

    `(mmec-defmethod ,NUMTYPE ((init ,PARENT-TYPE))
       ,DOCSTRING
       (let ((obj (,NORM-MAKER init)))
	 (unless (mmec-fits-number-type-p ,STEM obj)
	   (signal 'mmec-error-value-out-of-range (list --func-- init)))
	 (mmec--make-obj ,STEM (,CFUNC-CTOR (mmec--extract-obj ,NORM-STEM obj)))))))

;;; --------------------------------------------------------------------

(mmec-defmacro mmec--define-number-object-constructor-as-method-integer-argument (STEM &key INTREP-TYPE NORM-STEM)
  (let* ((NUMTYPE	(mmec-sformat "mmec-%s" STEM))
	 (NORM-TYPE	(mmec-sformat "mmec-%s" NORM-STEM))
	 (MAKER		NUMTYPE)
	 (NORM-MAKER	NORM-TYPE)
	 (INTREP-SPEC	(cl-case INTREP-TYPE
			  (integer	"elisp-integer")
			  (float	"elisp-float")
			  (usrptr	"usrptr")
			  (t
			   (signal 'mmec-error-invalid-argument (list --func--)))))
	 (CFUNC-CTOR	(mmec-sformat "mmec-c-make-%s-%s-from-usrptr-%s" INTREP-SPEC STEM NORM-STEM))
	 (DOCSTRING	(format "Constructor for number objects of type `%s'.

This  constructor accepts  as initialisation  argument a  value of  type
`integer'.

This constructor normalises the initialisation  argument to an object of
type `%s', then it checks if the range  is valid; if it is: it builds an
object  of   type  `%s',  otherwise   it  raises  the   error  condition
`mmec-error-value-out-of-range'."  NUMTYPE NORM-TYPE NUMTYPE)))
    `(cl-defmethod ,MAKER ((init integer))
       ,DOCSTRING
       (let ((obj (,NORM-MAKER init)))
	 (unless (mmec-fits-number-type-p ,STEM obj)
	   (signal 'mmec-error-value-out-of-range (list ',NUMTYPE init)))
	 (mmec--make-obj ,STEM (,CFUNC-CTOR (mmec--extract-obj ,NORM-STEM obj)))))))

;;; --------------------------------------------------------------------

(mmec-defmacro mmec--define-number-object-constructor-as-method-float-argument (STEM &key INTREP-TYPE NORM-STEM)
  (let* ((NUMTYPE	(mmec-sformat "mmec-%s" STEM))
	 (NORM-TYPE	(mmec-sformat "mmec-%s" NORM-STEM))
	 (MAKER		NUMTYPE)
	 (NORM-MAKER	NORM-TYPE)
	 (INTREP-SPEC	(cl-case INTREP-TYPE
			  (integer	"elisp-integer")
			  (float	"elisp-float")
			  (usrptr	"usrptr")
			  (t
			   (signal 'mmec-error-invalid-argument (list --func--)))))
	 (CFUNC-CTOR	(mmec-sformat "mmec-c-make-%s-%s-from-usrptr-%s" INTREP-SPEC STEM NORM-STEM))
	 (DOCSTRING	(format "Constructor for number objects of type `%s'.

This  constructor accepts  as initialisation  argument a  value of  type
`float'.

This constructor normalises the initialisation  argument to an object of
type `%s', then it checks if the range  is valid; if it is: it builds an
object  of   type  `%s',  otherwise   it  raises  the   error  condition
`mmec-error-value-out-of-range'."  NUMTYPE NORM-TYPE NUMTYPE)))
    `(cl-defmethod ,MAKER ((init float))
       ,DOCSTRING
       (let ((obj (,NORM-MAKER init)))
	 (unless (mmec-fits-number-type-p ,STEM obj)
	   (signal 'mmec-error-value-out-of-range (list ',NUMTYPE init)))
	 (mmec--make-obj ,STEM (,CFUNC-CTOR (mmec--extract-obj ,NORM-STEM obj)))))))

;;; --------------------------------------------------------------------

(mmec-defmacro mmec--define-number-object-constructor-as-method-signed-integer-argument (STEM &key INTREP-TYPE NORM-STEM)
  (let* ((NUMTYPE	(mmec-sformat "mmec-%s" STEM))
	 (NORM-TYPE	(mmec-sformat "mmec-%s" NORM-STEM))
	 (MAKER		NUMTYPE)
	 (NORM-MAKER	NORM-TYPE)
	 (INTREP-SPEC	(cl-case INTREP-TYPE
			  (integer	"elisp-integer")
			  (float	"elisp-float")
			  (usrptr	"usrptr")
			  (t
			   (signal 'mmec-error-invalid-argument (list --func--)))))
	 (CFUNC-CTOR	(mmec-sformat "mmec-c-make-%s-%s-from-usrptr-%s" INTREP-SPEC STEM NORM-STEM))
	 (DOCSTRING	(format "Constructor for number objects of type `%s'.

This  constructor accepts  as initialisation  argument a  value of  type
`mmec-signed-integer'.

This constructor normalises the initialisation  argument to an object of
type `%s', then it checks if the range  is valid; if it is: it builds an
object  of   type  `%s',  otherwise   it  raises  the   error  condition
`mmec-error-value-out-of-range'."  NUMTYPE NORM-TYPE NUMTYPE)))
    `(cl-defmethod ,MAKER ((init mmec-signed-integer))
       ,DOCSTRING
       (let ((obj (,NORM-MAKER init)))
	 (unless (mmec-fits-number-type-p ,STEM obj)
	   (signal 'mmec-error-value-out-of-range (list ',NUMTYPE init)))
	 (mmec--make-obj ,STEM (,CFUNC-CTOR (mmec--extract-obj ,NORM-STEM obj)))))))

;;; --------------------------------------------------------------------

(mmec-defmacro mmec--define-number-object-constructor-as-method-unsigned-integer-argument (STEM &key INTREP-TYPE NORM-STEM)
  (let* ((NUMTYPE	(mmec-sformat "mmec-%s" STEM))
	 (NORM-TYPE	(mmec-sformat "mmec-%s" NORM-STEM))
	 (MAKER		NUMTYPE)
	 (NORM-MAKER	NORM-TYPE)
	 (INTREP-SPEC	(cl-case INTREP-TYPE
			  (integer	"elisp-integer")
			  (float	"elisp-float")
			  (usrptr	"usrptr")
			  (t
			   (signal 'mmec-error-invalid-argument (list --func--)))))
	 (CFUNC-CTOR	(mmec-sformat "mmec-c-make-%s-%s-from-usrptr-%s" INTREP-SPEC STEM NORM-STEM))
	 (DOCSTRING	(format "Constructor for number objects of type `%s'.

This  constructor accepts  as initialisation  argument a  value of  type
`mmec-unsigned-integer'.

This constructor normalises the initialisation  argument to an object of
type `%s', then it checks if the range  is valid; if it is: it builds an
object  of   type  `%s',  otherwise   it  raises  the   error  condition
`mmec-error-value-out-of-range'."  NUMTYPE NORM-TYPE NUMTYPE)))
    `(cl-defmethod ,MAKER ((init mmec-unsigned-integer))
       ,DOCSTRING
       (let ((obj (,NORM-MAKER init)))
	 (unless (mmec-fits-number-type-p ,STEM obj)
	   (signal 'mmec-error-value-out-of-range (list ',NUMTYPE init)))
	 (mmec--make-obj ,STEM (,CFUNC-CTOR (mmec--extract-obj ,NORM-STEM obj)))))))

;;; --------------------------------------------------------------------

(mmec-defmacro mmec--define-number-object-constructor-as-method-floating-point-argument (STEM &key INTREP-TYPE NORM-STEM)
  (let* ((NUMTYPE	(mmec-sformat "mmec-%s" STEM))
	 (NORM-TYPE	(mmec-sformat "mmec-%s" NORM-STEM))
	 (MAKER		NUMTYPE)
	 (NORM-MAKER	NORM-TYPE)
	 (INTREP-SPEC	(cl-case INTREP-TYPE
			  (integer	"elisp-integer")
			  (float	"elisp-float")
			  (usrptr	"usrptr")
			  (t
			   (signal 'mmec-error-invalid-argument (list --func--)))))
	 (CFUNC-CTOR	(mmec-sformat "mmec-c-make-%s-%s-from-usrptr-%s" INTREP-SPEC STEM NORM-STEM))
	 (DOCSTRING	(format "Constructor for number objects of type `%s'.

This  constructor accepts  as initialisation  argument a  value of  type
`mmec-floating-point'.

This constructor normalises the initialisation  argument to an object of
type `%s', then it checks if the range  is valid; if it is: it builds an
object  of   type  `%s',  otherwise   it  raises  the   error  condition
`mmec-error-value-out-of-range'."  NUMTYPE NORM-TYPE NUMTYPE)))
    `(cl-defmethod ,MAKER ((init mmec-floating-point))
       ,DOCSTRING
       (let ((obj (,NORM-MAKER init)))
	 (unless (mmec-fits-number-type-p ,STEM obj)
	   (signal 'mmec-error-value-out-of-range (list ',NUMTYPE init)))
	 (mmec--make-obj ,STEM (,CFUNC-CTOR (mmec--extract-obj ,NORM-STEM obj)))))))

;;; --------------------------------------------------------------------

(defmacro mmec--define-number-object-constructor-for-unsupported-number-types (STEM)
  (let* ((NUMTYPE	(mmec-sformat "mmec-%s" STEM))
	 (MAKER		NUMTYPE)
	 (DOCSTRING	(format "Constructor for number objects of type `%s'.

This constructor  method signals that the  given initialisation argument
is invalid." NUMTYPE)))
    `(mmec-defmethod ,MAKER ((init mmec-number))
       ,DOCSTRING
       (signal 'mmec-error-unsupported-init-type (list --func-- init)))))

;;; --------------------------------------------------------------------

(mmec-defmacro mmec--define-number-object (STEM &key INTREP-TYPE NORM-STEM PARENT-STEM)
  (let* ((NUMTYPE		(mmec-sformat "mmec-%s" STEM))
	 (PARENT-TYPE		(mmec-sformat "mmec-%s" PARENT-STEM))
	 (STRUCT-CTOR-NAME	(mmec--type-elisp-constructor-name STEM)))
    `(progn
       (cl-defstruct (,NUMTYPE
		      (:include		,PARENT-TYPE)
		      (:constructor	,STRUCT-CTOR-NAME))
	 obj)

       (mmec--define-number-object-constructor-as-generic-function		,STEM)
       (mmec--define-number-object-copy-constructor-as-method			,STEM)
       (mmec--define-number-object-constructor-as-method-parent-argument	,STEM :INTREP-TYPE ,INTREP-TYPE :NORM-STEM ,NORM-STEM
										:PARENT-STEM ,PARENT-STEM)
       (mmec--define-number-object-constructor-as-method-integer-argument	,STEM :INTREP-TYPE ,INTREP-TYPE :NORM-STEM ,NORM-STEM)
       (mmec--define-number-object-constructor-as-method-float-argument		,STEM :INTREP-TYPE ,INTREP-TYPE :NORM-STEM ,NORM-STEM)
       ,(cl-case PARENT-STEM
	  (signed-integer
	   `(mmec--define-number-object-constructor-as-method-signed-integer-argument
	     ,STEM :INTREP-TYPE ,INTREP-TYPE :NORM-STEM ,NORM-STEM))
	  (unsigned-integer
	   `(mmec--define-number-object-constructor-as-method-unsigned-integer-argument
	     ,STEM :INTREP-TYPE ,INTREP-TYPE :NORM-STEM ,NORM-STEM))
	  (floating-point
	   `(progn
	      (mmec--define-number-object-constructor-as-method-signed-integer-argument
	       ,STEM :INTREP-TYPE ,INTREP-TYPE :NORM-STEM ,NORM-STEM)
	      (mmec--define-number-object-constructor-as-method-unsigned-integer-argument
	       ,STEM :INTREP-TYPE ,INTREP-TYPE :NORM-STEM ,NORM-STEM)
	      (mmec--define-number-object-constructor-as-method-floating-point-argument
	       ,STEM :INTREP-TYPE ,INTREP-TYPE :NORM-STEM ,NORM-STEM)))
	  (t
	   (signal 'mmec-error-invalid-argument (list --func-- PARENT-STEM))))
       (mmec--define-number-object-constructor-for-unsupported-number-types	,STEM)
       )))

;;; --------------------------------------------------------------------

(mmec-defmacro mmec--define-normalised-number-object (STEM &key INTREP-TYPE PARENT-STEM)
  (let* ((NUMTYPE		(mmec-sformat "mmec-%s" STEM))
	 (PARENT-TYPE		(mmec-sformat "mmec-%s" PARENT-STEM))
	 (STRUCT-CTOR-NAME	(mmec--type-elisp-constructor-name STEM)))
    `(progn
       (cl-defstruct (,NUMTYPE
		      (:include		,PARENT-TYPE)
		      (:constructor	,STRUCT-CTOR-NAME))
	 obj)

       (mmec--define-number-object-constructor-as-generic-function		,STEM)
       (mmec--define-number-object-copy-constructor-as-method			,STEM)
       (mmec--define-number-object-constructor-for-unsupported-number-types	,STEM)
       )))


;;;; C language type wrappers

(mmec--define-number-object char	:INTREP-TYPE integer	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object schar	:INTREP-TYPE integer	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object uchar	:INTREP-TYPE integer	:NORM-STEM uint64	:PARENT-STEM unsigned-integer)
(mmec--define-number-object wchar	:INTREP-TYPE usrptr 	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object sshrt	:INTREP-TYPE integer	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object ushrt	:INTREP-TYPE integer	:NORM-STEM uint64	:PARENT-STEM unsigned-integer)
(mmec--define-number-object sint	:INTREP-TYPE usrptr 	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object uint	:INTREP-TYPE usrptr 	:NORM-STEM uint64	:PARENT-STEM unsigned-integer)
(mmec--define-number-object slong	:INTREP-TYPE usrptr 	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object ulong	:INTREP-TYPE usrptr 	:NORM-STEM uint64	:PARENT-STEM unsigned-integer)
(mmec--define-number-object sllong	:INTREP-TYPE usrptr 	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object ullong	:INTREP-TYPE usrptr 	:NORM-STEM uint64	:PARENT-STEM unsigned-integer)
(mmec--define-number-object sintmax	:INTREP-TYPE usrptr 	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object uintmax	:INTREP-TYPE usrptr 	:NORM-STEM uint64	:PARENT-STEM unsigned-integer)
(mmec--define-number-object ssize	:INTREP-TYPE usrptr 	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object usize	:INTREP-TYPE usrptr 	:NORM-STEM uint64	:PARENT-STEM unsigned-integer)
(mmec--define-number-object ptrdiff	:INTREP-TYPE usrptr 	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object sint8	:INTREP-TYPE integer 	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object uint8	:INTREP-TYPE integer 	:NORM-STEM uint64	:PARENT-STEM unsigned-integer)
(mmec--define-number-object sint16	:INTREP-TYPE integer 	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object uint16	:INTREP-TYPE integer 	:NORM-STEM uint64	:PARENT-STEM unsigned-integer)
(mmec--define-number-object sint32	:INTREP-TYPE usrptr 	:NORM-STEM sint64	:PARENT-STEM signed-integer)
(mmec--define-number-object uint32	:INTREP-TYPE usrptr 	:NORM-STEM uint64	:PARENT-STEM unsigned-integer)
(mmec--define-number-object float	:INTREP-TYPE usrptr 	:NORM-STEM ldouble	:PARENT-STEM floating-point)
(mmec--define-number-object double	:INTREP-TYPE float 	:NORM-STEM ldouble	:PARENT-STEM floating-point)

(mmec--define-normalised-number-object sint64	:INTREP-TYPE usrptr 	:PARENT-STEM signed-integer)
(mmec--define-normalised-number-object uint64	:INTREP-TYPE usrptr 	:PARENT-STEM unsigned-integer)
(mmec--define-normalised-number-object ldouble	:INTREP-TYPE usrptr 	:PARENT-STEM floating-point)


;;;; C language type wrappers: sint64_t
;;
;;The custom number type  `mmec-sint64' is special because it is  used for normalised representation
;;of all the signed integer numbers, both built-in and custom.
;;

(cl-defmethod mmec-sint64 ((init integer))
  "Constructor for number objects of type `mmec-sint64'.

The argument INIT must be a value of type `integer'."
  (mmec--make sint64 :obj (mmec-c-make-usrptr-sint64-from-elisp-integer init)))

(cl-defmethod mmec-sint64 ((init float))
  "Constructor for number objects of type `mmec-sint64'.

The argument INIT must be a  value of type `float'."
  (mmec--make sint64 :obj (mmec-c-make-usrptr-sint64-from-elisp-float init)))

(cl-macrolet ((mmec--define-sint64-constructor-method-for-integer-intrep-init
	       (INIT-TYPE-OR-STEM)
	       "Expand into a `cl-defmethod' form defining a constructor method for `mmec-sint64' values.

The argument INIT-TYPE-OR-STEM must be a symbol representing a type name
or type stem name; this type must  be a signed integer with `integer' as
internal representation."
	       (let* ((INIT-TYPE	(intern (mmec--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
		      (DOCSTRING	(format "Constructor for number objects of type `mmec-sint64'.

The argument INIT must be an object of type `%s'." INIT-TYPE)))
		 `(cl-defmethod mmec-sint64 ((init ,INIT-TYPE))
		    ,DOCSTRING
		    (mmec--make sint64 :obj (mmec-c-make-usrptr-sint64-from-elisp-integer (mmec--extract-obj ,INIT-TYPE init)))))))
  (mmec--define-sint64-constructor-method-for-integer-intrep-init char)
  (mmec--define-sint64-constructor-method-for-integer-intrep-init schar)
  (mmec--define-sint64-constructor-method-for-integer-intrep-init sshrt)
  (mmec--define-sint64-constructor-method-for-integer-intrep-init sint8)
  (mmec--define-sint64-constructor-method-for-integer-intrep-init sint16))

(cl-macrolet
    ((mmec--define-sint64-constructor-method-for-usrptr-intrep-init
      (INIT-TYPE-OR-STEM)
      "Expand into a `cl-defmethod' form defining a constructor method for `mmec-sint64' values.

The argument INIT-TYPE-OR-STEM must be a symbol representing a type name
or  type  stem  name;  this  type  must  be  a  signed  integer  with  a
user-pointer object as internal representation."
      (let* ((INIT-TYPE		(intern (mmec--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
	     (CLANG-CONSTRUCTOR	(intern (format "mmec-c-make-usrptr-sint64-from-usrptr-%s" INIT-TYPE-OR-STEM)))
	     (DOCSTRING		(format "Constructor for number objects of type `mmec-sint64'.

The argument INIT must be an object of type `%s'." INIT-TYPE)))
	`(cl-defmethod mmec-sint64 ((init ,INIT-TYPE))
	   ,DOCSTRING
	   (mmec--make sint64 :obj (,CLANG-CONSTRUCTOR (mmec--extract-obj ,INIT-TYPE init)))))))
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init sint)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init slong)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init sllong)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init sintmax)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init ssize)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init sint32)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init wchar)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init ptrdiff))


;;;; C language type wrappers: uint64_t
;;
;;The custom number type  `mmec-uint64' is special because it is  used for normalised representation
;;of all the unsigned integer numbers, both built-in and custom.
;;

(mmec-defmethod mmec-uint64 ((init integer))
  "Constructor for number objects of type `mmec-uint64'.

The argument INIT  must be a value  of type `integer'.  If  the value is
negative: the condition `mmec-error-value-out-of-range' is raised."
  (mmec--assert-positive-number init --func--)
  (mmec--make uint64 :obj (mmec-c-make-usrptr-uint64-from-elisp-integer init)))

(mmec-defmethod mmec-uint64 ((init float))
  "Constructor for number objects of type `mmec-uint64'.

The argument  INIT must  be a value  of type `float'.   If the  value is
negative: the condition `mmec-error-value-out-of-range' is raised."
  (mmec--assert-positive-number init --func--)
  (mmec--make uint64 :obj (mmec-c-make-usrptr-uint64-from-elisp-float init)))

;;Define constructor  methods with initialisation  value: signed  integer having `integer'  value as
;;internal representation.
;;
(cl-macrolet
    ((mmec--def (INIT-TYPE-OR-STEM)
		(let* ((INIT-TYPE	(intern (mmec--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
		       (DOCSTRING	(format "Constructor for number objects of type `mmec-uint64'.

This constructor  method accepts as initialisation  argument an instance
of type `%s'." INIT-TYPE)))
		  `(mmec-defmethod mmec-uint64 ((init ,INIT-TYPE))
		     ,DOCSTRING
		     (mmec--assert-positive-number init --func--)
		     (mmec--make uint64 :obj (mmec-c-make-usrptr-uint64-from-elisp-integer (mmec--extract-obj ,INIT-TYPE init)))))))
  (mmec--def char)
  (mmec--def schar)
  (mmec--def sshrt)
  (mmec--def sint8)
  (mmec--def sint16))

;;Define constructor methods  with initialisation value: unsigned integer having  `integer' value as
;;internal representation.
;;
(cl-macrolet
    ((mmec--def (INIT-TYPE-OR-STEM)
		(let* ((INIT-TYPE	(intern (mmec--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
		       (DOCSTRING	(format "Constructor for number objects of type `mmec-uint64'.

This constructor  method accepts as initialisation  argument an instance
of type `%s'." INIT-TYPE)))
		  `(mmec-defmethod mmec-uint64 ((init ,INIT-TYPE))
		     ,DOCSTRING
		     (mmec--make uint64 :obj (mmec-c-make-usrptr-uint64-from-elisp-integer (mmec--extract-obj ,INIT-TYPE init)))))))
  (mmec--def uchar)
  (mmec--def ushrt)
  (mmec--def uint8)
  (mmec--def uint16))

;;Define  constructor methods  with initialisation  value:  unsigned integer  having a  user-pointer
;;object as internal representation.
;;
(cl-macrolet
    ((mmec--def (INIT-TYPE-OR-STEM)
		(let* ((INIT-TYPE		(intern (mmec--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
		       (CLANG-CONSTRUCTOR	(intern (format "mmec-c-make-usrptr-uint64-from-usrptr-%s" INIT-TYPE-OR-STEM)))
		       (DOCSTRING		(format "Constructor for number objects of type `mmec-uint64'.

This  constructor method  accepts as  initialisation argument  an
instance of type `%s'." INIT-TYPE)))
		  `(mmec-defmethod mmec-uint64 ((init ,INIT-TYPE))
		     ,DOCSTRING
		     (mmec--make uint64 :obj (,CLANG-CONSTRUCTOR  (mmec--extract-obj ,INIT-TYPE init)))))))

  (mmec--def uint)
  (mmec--def ulong)
  (mmec--def ullong)
  (mmec--def uintmax)
  (mmec--def usize)
  (mmec--def uint32))


;;;; C language type wrappers: long double float
;;
;;The custom number type `mmec-ldouble' is special  because it is used for normalised representation
;;of all the floating-point numbers, both built-in and custom.
;;

(mmec-defmethod mmec-ldouble ((init integer))
  "Constructor for number objects of type `mmec-ldouble'.

The argument INIT must be a valur of type `integer'.  The initialisation
value is normalised  to an instance of `float', then  the constructor is
recursively applied to the normalised value."
  (mmec-ldouble (float init)))

(mmec-defmethod mmec-ldouble ((init float))
  "Constructor for number objects of type `mmec-ldouble'.

The argument INIT must be a valur of type `float'."
  (mmec--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-elisp-float init)))

;;; --------------------------------------------------------------------

(mmec-defmethod mmec-ldouble ((init mmec-sint64))
  "Constructor for number objects of type `mmec-ldouble'.

This constructor  method initalises the  returned object with  the value
from an instance of `mmec-sint64'"
  (mmec--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-usrptr-sint64 (mmec--extract-obj sint64 init))))

(mmec-defmethod mmec-ldouble ((init mmec-uint64))
  "Constructor for number objects of type `mmec-ldouble'.

This constructor  method initalises the  returned object with  the value
from an instance of `mmec-uint64'"
  (mmec--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-usrptr-uint64 (mmec--extract-obj uint64 init))))

;;; --------------------------------------------------------------------

(mmec-defmethod mmec-ldouble ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-ldouble'.

This    constructor   method    normalises    any    number   of    type
`mmec-signed-integer'  to   an  instance   of  `mmec-sint64',   then  it
recursively calls the constructor on the result."
  (mmec-ldouble (mmec-sint64 init)))

(mmec-defmethod mmec-ldouble ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-ldouble'.

This    constructor   method    normalises    any    number   of    type
`mmec-unsigned-integer'  to  an  instance   of  `mmec-uint64',  then  it
recursively calls the constructor on the result."
  (mmec-ldouble (mmec-uint64 init)))

;;; --------------------------------------------------------------------

(mmec-defmethod mmec-ldouble ((init mmec-float))
  "Constructor for number objects of type `mmec-ldouble'.

The argument INIT is an instance of `mmec-float'."
  (mmec--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-usrptr-float (mmec--extract-obj float init))))

(mmec-defmethod mmec-ldouble ((init mmec-double))
  "Constructor for number objects of type `mmec-ldouble'.

The argument INIT is an instance of `mmec-double'."
  (mmec--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-elisp-float (mmec--extract-obj double init))))


;;;; standard definitions

(cl-macrolet
    ((mmec--def (TYPE-OR-STEM)
		(let* ((STEM		(intern (mmec--strip-prefix-from-symbol-name TYPE-OR-STEM)))
		       (ALIAS-NAME	(mmec-sformat "make-mmec-%s"	STEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s"		STEM))
		       (DOCSTRING	(format "Standard constructor for the object type `%s'." NUMTYPE)))
		  `(defalias (quote ,ALIAS-NAME) (quote ,NUMTYPE) ,DOCSTRING))))
  (mmec--def char)
  (mmec--def schar)
  (mmec--def uchar)
  (mmec--def wchar)
  (mmec--def sshrt)
  (mmec--def ushrt)
  (mmec--def sint)
  (mmec--def uint)
  (mmec--def slong)
  (mmec--def ulong)
  (mmec--def sllong)
  (mmec--def ullong)
  (mmec--def ssize)
  (mmec--def usize)
  (mmec--def sintmax)
  (mmec--def uintmax)
  (mmec--def ptrdiff)
  (mmec--def sint8)
  (mmec--def uint8)
  (mmec--def sint16)
  (mmec--def uint16)
  (mmec--def sint32)
  (mmec--def uint32)
  (mmec--def sint64)
  (mmec--def uint64)
  (mmec--def float)
  (mmec--def double)
  (mmec--def ldouble))


;;;; range inclusion

(cl-macrolet
    ((mmec--def (TYPE-OR-STEM USRPTR-ARGTYPE NORMALISED-TYPE-OR-STEM)
		(let* ((TYPE-STEM.str		(mmec--strip-prefix-from-symbol-name TYPE-OR-STEM))
		       (TYPE			(intern (mmec--prepend-prefix-to-symbol-name TYPE-OR-STEM)))
		       (NORMALISED-STEM.str	(mmec--strip-prefix-from-symbol-name NORMALISED-TYPE-OR-STEM))
		       (NORMALISED-TYPE		(intern (mmec--prepend-prefix-to-symbol-name NORMALISED-TYPE-OR-STEM)))
		       (FUNCNAME		(intern (format "mmec-fits-%s-p" TYPE-STEM.str)))
		       (CLANG-FUNCNAME		(intern (format "mmec-c-%s-fits-%s-p" NORMALISED-STEM.str TYPE-STEM.str)))
		       (DOCSTRING		(format "Return true if the argument fits an object of type `%s'." TYPE)))
		  `(progn
		     (cl-defgeneric ,FUNCNAME (op)
		       ,DOCSTRING)
		     (cl-defmethod  ,FUNCNAME ((op ,USRPTR-ARGTYPE))
		       ,DOCSTRING
		       (,CLANG-FUNCNAME (mmec--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
		     (cl-defmethod  ,FUNCNAME ((op integer))
		       ,DOCSTRING
		       (,CLANG-FUNCNAME (mmec--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
		     (cl-defmethod  ,FUNCNAME ((op float))
		       ,DOCSTRING
		       (,CLANG-FUNCNAME (mmec--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
		     )))
     (mmec--def/signed-integer (TYPE-OR-STEM)
			       `(mmec--def ,TYPE-OR-STEM mmec-signed-integer   mmec-sint64))
     (mmec--def/unsigned-integer (TYPE-OR-STEM)
				 `(mmec--def ,TYPE-OR-STEM mmec-unsigned-integer mmec-uint64))
     (mmec--def/floating-point (TYPE-OR-STEM)
			       `(mmec--def ,TYPE-OR-STEM mmec-floating-point   mmec-ldouble)))

  (mmec--def/signed-integer	char)
  (mmec--def/signed-integer	schar)
  (mmec--def/unsigned-integer	uchar)
  (mmec--def/signed-integer	sshrt)
  (mmec--def/unsigned-integer	ushrt)
  (mmec--def/signed-integer	sint)
  (mmec--def/unsigned-integer	uint)
  (mmec--def/signed-integer	slong)
  (mmec--def/unsigned-integer	ulong)
  (mmec--def/signed-integer	sllong)
  (mmec--def/unsigned-integer	ullong)
  (mmec--def/signed-integer	ssize)
  (mmec--def/unsigned-integer	usize)
  (mmec--def/signed-integer	sintmax)
  (mmec--def/unsigned-integer	uintmax)
  (mmec--def/signed-integer	ptrdiff)
  (mmec--def/signed-integer	wchar)
  (mmec--def/signed-integer	sint8)
  (mmec--def/unsigned-integer	uint8)
  (mmec--def/signed-integer	sint16)
  (mmec--def/unsigned-integer	uint16)
  (mmec--def/signed-integer	sint32)
  (mmec--def/unsigned-integer	uint32)
  (mmec--def/signed-integer	sint64)
  (mmec--def/unsigned-integer	uint64)
  (mmec--def/floating-point	float)
  (mmec--def/floating-point	double)
  (mmec--def/floating-point	ldouble))


;;;; printing objects

(cl-macrolet
    ((mmec--def (TYPESTEM)
		"Define a `cl-print-object' method for number objects having a \
                 built-in Emacs value of type `integer' as internal representation."
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (DOCSTRING	(format "Print to a stream the representation of a number object of type `%s'." NUMTYPE))
		       (TEMPLATE	(format "#s(%s %%d)" NUMTYPE)))
		  `(cl-defmethod cl-print-object ((obj ,NUMTYPE) stream)
		     ,DOCSTRING
		     (princ (format ,TEMPLATE (mmec--extract-obj ,TYPESTEM obj)) stream)))))
  (mmec--def char)
  (mmec--def schar)
  (mmec--def uchar)
  (mmec--def sshrt)
  (mmec--def ushrt)
  (mmec--def sint8)
  (mmec--def uint8)
  (mmec--def sint16)
  (mmec--def uint16))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		"Define a `cl-print-object' method for number objects having a \
                 built-in Emacs value of type `float' as internal representation."
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (DOCSTRING	(format "Print to a stream the representation of a number object of type `%s'." NUMTYPE))
		       (TEMPLATE	(format "#s(%s %%g)" NUMTYPE)))
		  `(cl-defmethod cl-print-object ((obj ,NUMTYPE) stream)
		     ,DOCSTRING
		     (princ (format ,TEMPLATE (mmec--extract-obj ,TYPESTEM obj)) stream)))))
  (mmec--def double))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		"Define a `cl-print-object' method for number objects having a \
		 user-pointer object as internal representation."
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (DOCSTRING	(format "Print to a stream the representation of a number object of type `%s'." NUMTYPE))
		       (TEMPLATE	(format "#s(%s %%s)" NUMTYPE))
		       (CLANG-PRINTER	(mmec-sformat "mmec-c-%s-print-to-string" TYPESTEM)))
		  `(cl-defmethod cl-print-object ((obj ,NUMTYPE) stream)
		     ,DOCSTRING
		     (princ (format ,TEMPLATE (,CLANG-PRINTER (mmec--extract-obj ,TYPESTEM obj))) stream)))))
  (mmec--def sint)
  (mmec--def uint)
  (mmec--def slong)
  (mmec--def ulong)
  (mmec--def sllong)
  (mmec--def ullong)
  (mmec--def ssize)
  (mmec--def usize)
  (mmec--def sintmax)
  (mmec--def uintmax)
  (mmec--def wchar)
  (mmec--def ptrdiff)
  (mmec--def sint32)
  (mmec--def uint32)
  (mmec--def sint64)
  (mmec--def uint64)
  (mmec--def float)
  (mmec--def ldouble))


;;;; numeric comparison operations
;;
;;To perform a comparison operation we normalise the operands as follows:
;;
;;* We convert all the signed integers to `mmec-sint64'.
;;
;;* We convert all the unsigned integers to `mmec-uint64'.
;;
;;* We convert all the floating-point numbers to `mmec-ldouble'.
;;
;;* When  comparing  integers  and floating-point  numbers  we  convert  all  the integer  types  to
;;  `mmec-ldouble'.
;;

(defmacro mmec--define-numeric-comparison-generic-functions (OPERATOR)
  (let* ((MMEC-FUNC	(intern (format "mmec%s"   OPERATOR)))
	 (MMEC-FUNC2	(intern (format "mmec-2%s" OPERATOR)))
	 (DOCSTRING	(format "Return true if every operand is %s to the one following it; otherwise return false." OPERATOR))
	 (DOCSTRING2	(format "Return true if OP1 %s OP2; otherwise return false." OPERATOR)))
    `(progn
       (defun ,MMEC-FUNC (op &rest ops)
    	 ,DOCSTRING
	 ;;FIXME Should I rewrite this to use `cl-loop'?  (Marco Maggi; Feb 5, 2020)
	 (let ((rv t))
	   (while ops
	     (let ((item (car ops)))
	       (if (,MMEC-FUNC2 op item)
		   (progn
		     (setq op item)
		     (setq ops (cdr ops)))
		 (progn
		   (setq rv  nil)
		   (setq ops nil)))))
	   rv))

       (cl-defgeneric ,MMEC-FUNC2 (op1 op2)
	 ,DOCSTRING2)
       )))

(defmacro mmec--def-numeric-compar-method (MMEC-FUNC2 OPERATOR OPERATION TYPE1 CONVERTER1 TYPE2 CONVERTER2)
  ;;Define  a  comparison  methods that  converts  the  operands  and  then invokes  an  appropriate
  ;;operation.  Examples:
  ;;
  ;; (mmec--def-numeric-compar-method mmec-=2 = mmec-=2 integer mmec-sint64 mmec-float mmec-ldouble)
  ;; ==> (cl-defmethod mmec-=2 ((op1 integer) (op2 mmec-float))
  ;;       "..."
  ;;       (mmec-=2 (mmec-sint64 op1) (mmec-ldouble op2)))
  ;;
  ;; (mmec--def-numeric-compar-method mmec-=2 = = integer identity integer identity)
  ;; ==> (cl-defmethod mmec-=2 ((op1 integer) (op2 integer))
  ;;       "..."
  ;;       (= op1 op2))
  ;;
  (let* ((OPERATOR.str	(symbol-name OPERATOR))
	 (TYPE1.str	(symbol-name TYPE1))
	 (TYPE2.str	(symbol-name TYPE2))
	 (DOCSTRING	(format "Return true if OP1 %s OP2; otherwise return false.

The argument OP1 must be of type `%s'.

The argument OP2 must be of type `%s'.
" OPERATOR TYPE1 TYPE2))
	 (CONVERSION1	(if (eq CONVERTER1 'identity) 'op1 `(,CONVERTER1 op1)))
	 (CONVERSION2	(if (eq CONVERTER2 'identity) 'op2 `(,CONVERTER2 op2))))
    `(cl-defmethod ,MMEC-FUNC2 ((op1 ,TYPE1) (op2 ,TYPE2))
       ,DOCSTRING
       (,OPERATION ,CONVERSION1 ,CONVERSION2))))

(defmacro mmec--define-numeric-comparison (OPERATOR)
  ;;Define everything needed to perform a comparison operation among exact integers.
  ;;
  (let* ((MMEC-FUNC2			(intern (format "mmec-2%s" OPERATOR)))
	 (OPERATION-SINT64		(intern (format "mmec-c-sint64%s" OPERATOR)))
	 (OPERATION-UINT64		(intern (format "mmec-c-uint64%s" OPERATOR)))
	 (OPERATION-SINT64-UINT64	(intern (format "mmec-c-sint64-uint64%s" OPERATOR)))
	 (OPERATION-UINT64-SINT64	(intern (format "mmec-c-uint64-sint64%s" OPERATOR)))
	 (OPERATION-LDOUBLE		(intern (format "mmec-c-ldouble%s" OPERATOR))))
    `(progn
       (mmec--define-numeric-comparison-generic-functions ,OPERATOR)

       ;; These are the methods that actually do the operation on built-in numeric objects.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATOR integer identity integer identity)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATOR integer identity float   identity)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATOR float   identity integer identity)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATOR float   identity float   identity)

       ;; These are the methods that actually do the operation on custom user-pointer objects.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATION-SINT64        mmec-sint64 mmec-sint64-obj mmec-sint64 mmec-sint64-obj)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATION-UINT64        mmec-uint64 mmec-uint64-obj mmec-uint64 mmec-uint64-obj)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATION-SINT64-UINT64 mmec-sint64 mmec-sint64-obj mmec-uint64 mmec-uint64-obj)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATION-UINT64-SINT64 mmec-uint64 mmec-uint64-obj mmec-sint64 mmec-sint64-obj)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATION-LDOUBLE
					mmec-ldouble mmec-ldouble-obj
					mmec-ldouble mmec-ldouble-obj)

       ;; These are the methods that normalise operands among operational types.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-sint64 mmec-ldouble mmec-ldouble identity)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-uint64 mmec-ldouble mmec-ldouble identity)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-ldouble identity mmec-sint64 mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-ldouble identity mmec-uint64 mmec-ldouble)

       ;; These are the methods that normalise among integer types.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-signed-integer   mmec-sint64 mmec-signed-integer   mmec-sint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-unsigned-integer mmec-uint64 mmec-unsigned-integer mmec-uint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-signed-integer   mmec-sint64 mmec-unsigned-integer mmec-uint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-unsigned-integer mmec-uint64 mmec-signed-integer   mmec-sint64)

       ;; These are the methods that normalise among floating point types.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-floating-point mmec-ldouble mmec-floating-point mmec-ldouble)

       ;; These are the methods that normalise mixed numeric types: `mmec-floating-point', `mmec-signed-integer', `mmec-unsigned-intger'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-floating-point   mmec-ldouble mmec-signed-integer   mmec-sint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-signed-integer   mmec-sint64      mmec-floating-point   mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-floating-point   mmec-ldouble mmec-unsigned-integer mmec-uint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-unsigned-integer mmec-uint64      mmec-floating-point   mmec-ldouble)

       ;; These are the methods that normalise mixed numeric types: `integer' and `mmec-floating-point'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 integer           mmec-ldouble mmec-floating-point mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-floating-point mmec-ldouble integer           mmec-ldouble)

       ;; These are the methods that normalise mixed numeric types: `integer' and `mmec-signed-integer'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 integer           mmec-sint64 mmec-signed-integer mmec-sint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-signed-integer mmec-sint64 integer           mmec-sint64)

       ;; These are the methods that normalise mixed numeric types: `integer' and `mmec-unsigned-integer'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 integer             mmec-sint64 mmec-unsigned-integer mmec-uint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-unsigned-integer mmec-uint64 integer             mmec-sint64)

       ;; These are the methods that normalise mixed numeric types: `float' and `mmec-floating-point'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 float             mmec-ldouble mmec-floating-point mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-floating-point mmec-ldouble float             mmec-ldouble)

       ;; These are the methods that normalise mixed numeric types: `float' and `mmec-signed-integer'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 float             mmec-ldouble mmec-signed-integer mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-signed-integer mmec-ldouble float             mmec-ldouble)

       ;; These are the methods that normalise mixed numeric types: `float' and `mmec-unsigned-integer'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 float               mmec-ldouble mmec-unsigned-integer mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-unsigned-integer mmec-ldouble float               mmec-ldouble)
       )))

(mmec--define-numeric-comparison =)
(mmec--define-numeric-comparison <)
(mmec--define-numeric-comparison >)
(mmec--define-numeric-comparison <=)
(mmec--define-numeric-comparison >=)
(mmec--define-numeric-comparison /=)


;;;; sign inspection functions

(cl-defgeneric mmec-zero-p (obj)
  "Return true if the numeric argument is zero; otherwise return false.")

(cl-defgeneric mmec-positive-p (obj)
  "Return true if the numeric argument is positive; otherwise return false.")

(cl-defgeneric mmec-negative-p (obj)
  "Return true if the numeric argument is negative; otherwise return false.")

(cl-defgeneric mmec-non-positive-p (obj)
  "Return true if the numeric argument is non-positive; otherwise return false.")

(cl-defgeneric mmec-non-negative-p (obj)
  "Return true if the numeric argument is non-negative; otherwise return false.")

;;; --------------------------------------------------------------------

(cl-macrolet
    ((mmec--def (TYPE)
		`(progn
		   (cl-defmethod mmec-zero-p ((obj ,TYPE))
		     "Return true if the numeric argument is zero; otherwise return false."
		     (zerop obj))
		   (cl-defmethod mmec-positive-p ((obj ,TYPE))
		     "Return true if the numeric argument is positive; otherwise return false."
		     (< 0 obj))
		   (cl-defmethod mmec-negative-p ((obj ,TYPE))
		     "Return true if the numeric argument is negative; otherwise return false."
		     (> 0 obj))
		   (cl-defmethod mmec-non-positive-p ((obj ,TYPE))
		     "Return true if the numeric argument is non-positive; otherwise return false."
		     (>= 0 obj))
		   (cl-defmethod mmec-non-negative-p ((obj ,TYPE))
		     "Return true if the numeric argument is non-negative; otherwise return false."
		     (<= 0 obj))
		   )))
  (mmec--def integer)
  (mmec--def float))

;;; --------------------------------------------------------------------

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((NUMTYPE			(mmec-sformat "mmec-%s" TYPESTEM))
		       (CFUNC-ZERO-P		(mmec-sformat "mmec-c-%s-zero-p" TYPESTEM))
		       (CFUNC-POSITIVE-P	(mmec-sformat "mmec-c-%s-positive-p" TYPESTEM))
		       (CFUNC-NEGATIVE-P	(mmec-sformat "mmec-c-%s-negative-p" TYPESTEM))
		       (CFUNC-NON-POSITIVE-P	(mmec-sformat "mmec-c-%s-positive-p" TYPESTEM))
		       (CFUNC-NON-NEGATIVE-P	(mmec-sformat "mmec-c-%s-negative-p" TYPESTEM)))
		  `(progn
		     (cl-defmethod mmec-zero-p ((obj ,NUMTYPE))
		       (,CFUNC-ZERO-P (mmec--extract-obj ,TYPESTEM obj)))
		     (cl-defmethod mmec-positive-p ((obj ,NUMTYPE))
		       ,(if (mmec-symbol-number-type-is-signed-p TYPESTEM)
			    `(,CFUNC-POSITIVE-P (mmec--extract-obj ,TYPESTEM obj))
			  t))
		     (cl-defmethod mmec-negative-p ((obj ,NUMTYPE))
		       ,(if (mmec-symbol-number-type-is-signed-p TYPESTEM)
			    `(,CFUNC-NEGATIVE-P (mmec--extract-obj ,TYPESTEM obj))
			  nil))
		     (cl-defmethod mmec-non-positive-p ((obj ,NUMTYPE))
		       ,(if (mmec-symbol-number-type-is-signed-p TYPESTEM)
			    `(,CFUNC-NON-POSITIVE-P (mmec--extract-obj ,TYPESTEM obj))
			  `(,CFUNC-ZERO-P (mmec--extract-obj ,TYPESTEM obj))))
		     (cl-defmethod mmec-non-negative-p ((obj ,NUMTYPE))
		       ,(if (mmec-symbol-number-type-is-signed-p TYPESTEM)
			    `(,CFUNC-NON-NEGATIVE-P (mmec--extract-obj ,TYPESTEM obj))
			  t))
		     ))))
  (mmec--def char)
  (mmec--def schar)
  (mmec--def uchar)
  (mmec--def wchar)
  (mmec--def sshrt)
  (mmec--def ushrt)
  (mmec--def sint)
  (mmec--def uint)
  (mmec--def slong)
  (mmec--def ulong)
  (mmec--def sllong)
  (mmec--def ullong)
  (mmec--def ssize)
  (mmec--def usize)
  (mmec--def sintmax)
  (mmec--def uintmax)
  (mmec--def ptrdiff)
  (mmec--def sint8)
  (mmec--def uint8)
  (mmec--def sint16)
  (mmec--def uint16)
  (mmec--def sint32)
  (mmec--def uint32)
  (mmec--def sint64)
  (mmec--def uint64)
  (mmec--def float)
  (mmec--def double)
  (mmec--def ldouble))


;;;; done

(provide 'mmec-number-objects)

;;; end of file
