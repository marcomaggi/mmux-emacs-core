;;; cc-number-objects.el --- numeric type definitions for C language intefaces

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-10 17:26:41 marco>
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

(require 'cc-basics)
(require 'cc-constants)


;;;; basic numeric type definitions

;; Base type of all the custom number types defined by this module.
(cl-defstruct (cc-number
	       (:constructor	cc-number--make)))

;; Base type of all the custom exact integer types defined by this module.
(cl-defstruct (cc-integer
	       (:include	cc-number)
	       (:constructor	cc-integer--make)))

;; Base type of all the custom exact signed integer number types defined by this module.
(cl-defstruct (cc-signed-integer
	       (:include	cc-integer)
	       (:constructor	cc-signed-integer--make)))

;; Base type of all the custom exact unsigned integer number types defined by this module.
(cl-defstruct (cc-unsigned-integer
	       (:include	cc-integer)
	       (:constructor	cc-unsigned-integer--make)))

;; Base type of all the custom floating-point number types defined by this module.
(cl-defstruct (cc-floating-point
	       (:include	cc-number)
	       (:constructor	cc-floating-point--make)))

;;; --------------------------------------------------------------------

(defmacro cc--define-abstract-type-constructor (TYPE)
  `(defun ,TYPE (&rest args)
     (signal 'mmux-core-instantiating-abstract-type (quote ,TYPE))))

(cc--define-abstract-type-constructor cc-number)
(cc--define-abstract-type-constructor cc-integer)
(cc--define-abstract-type-constructor cc-signed-integer)
(cc--define-abstract-type-constructor cc-unsigned-integer)
(cc--define-abstract-type-constructor cc-floating-point)


;;;; C language type wrappers: custom number objects

(defmacro cc--define-custom-number-object (TYPE-OR-STEM ANCESTOR-TYPE)
  ;;Define everything needed to define a new custom number object.
  ;;
  ;;The argument  TYPE-OR-STEM must be  a symbol  representing the new  full type name  (with prefix
  ;;"cc-") or the stem of the type name (without the prefix "cc-").
  ;;
  ;;Usage example:
  ;;
  ;;   (cc--define-custom-number-object cc-char cc-signed-integer)
  ;;
  (let* ((TYPE.str		(cc--prepend-prefix-to-symbol-name TYPE-OR-STEM))
	 (STEM.str		(cc--strip-prefix-from-symbol-name TYPE-OR-STEM))
	 (TYPE			(intern TYPE.str))
	 (PARENT-TYPE		ANCESTOR-TYPE)
	 (DOCSTRING		(concat "Build and return a new instance of `" TYPE.str "'."))
	 (FITS-ASSERTION	(let ((FITS-FUNC (intern (concat "cc-fits-" STEM.str "-p"))))
				  `(unless (,FITS-FUNC obj)
				     (signal 'mmec-error-value-out-of-range (list (quote ,TYPE) init)))))
	 (ELISP-CONSTRUCTOR	(intern (concat TYPE.str "--make"))))
    (cl-multiple-value-bind (NORMALISED-TYPE)
	(cond ((eq ANCESTOR-TYPE 'cc-signed-integer)	(list 'cc-sint64))
	      ((eq ANCESTOR-TYPE 'cc-unsigned-integer)	(list 'cc-uint64))
	      ((eq ANCESTOR-TYPE 'cc-floating-point)	(list 'cc-ldouble))
	      (t
	       (signal 'mmec-error-unsupported-init-type ANCESTOR-TYPE)))
      `(progn
	 (cl-defstruct (,TYPE
			(:include	,PARENT-TYPE)
			(:constructor	,ELISP-CONSTRUCTOR))
	   obj)

	 ;;This is the public object constructor implemented as generic function.
	 ;;
	 (cl-defgeneric ,TYPE (init)
	   ,DOCSTRING)

	 ;;The copy constructor implemented  as method.  This method creates a  duplicate of the elisp
	 ;;object, but it reuses the internal representation (which is immutable).
	 ;;
	 (cl-defmethod ,TYPE ((init ,TYPE))
	   ,DOCSTRING
	   (cc--make ,TYPE :obj (cc--extract-obj ,TYPE init)))

	 ;;This constructor method accepts as initialisation argument a value whose type is the parent
	 ;;of TYPE.
	 ;;
	 (cl-defmethod ,TYPE ((init ,ANCESTOR-TYPE))
	   ,DOCSTRING
	   (let ((obj (,NORMALISED-TYPE init)))
	     ,FITS-ASSERTION
	     (cc--make ,TYPE :obj (cc--clang-constructor ,TYPE (cc--extract-obj ,NORMALISED-TYPE obj)))))

	 ;;This constructor method accepts as initialisation  argument a value of the Emacs's built-in
	 ;;type `integer'.
	 ;;
	 (cl-defmethod ,TYPE ((init integer))
	   ,DOCSTRING
	   (let ((obj (,NORMALISED-TYPE init)))
	     ,FITS-ASSERTION
	     (,ELISP-CONSTRUCTOR :obj (cc--clang-constructor ,TYPE (cc--extract-obj ,NORMALISED-TYPE obj)))))

	 ;;This constructor method accepts as initialisation  argument a value of the Emacs's built-in
	 ;;type `integer'.
	 ;;
	 (cl-defmethod ,TYPE ((init float))
	   ,DOCSTRING
	   (let ((obj (,NORMALISED-TYPE init)))
	     ,FITS-ASSERTION
	     (,ELISP-CONSTRUCTOR :obj (cc--clang-constructor ,TYPE (cc--extract-obj ,NORMALISED-TYPE obj)))))

	 ;;This constructor method signals that the given initialisation argument is invalid.
	 ;;
	 (cl-defmethod ,TYPE ((init cc-number))
	   ,DOCSTRING
	   (signal 'mmec-error-unsupported-init-type (list ',TYPE init)))

	 ))))

;;These number objects have a built-in `integer' value as internal representation.
;;
(cc--define-custom-number-object cc-char	cc-signed-integer)
(cc--define-custom-number-object cc-schar	cc-signed-integer)
(cc--define-custom-number-object cc-uchar	cc-unsigned-integer)
(cc--define-custom-number-object cc-wchar	cc-unsigned-integer)
(cc--define-custom-number-object cc-sshrt	cc-signed-integer)
(cc--define-custom-number-object cc-ushrt	cc-unsigned-integer)
(cc--define-custom-number-object cc-sint8	cc-signed-integer)
(cc--define-custom-number-object cc-uint8	cc-unsigned-integer)
(cc--define-custom-number-object cc-sint16	cc-signed-integer)

;;These number objects have a custom user-pointer object as internal representation.
;;
(cc--define-custom-number-object cc-sint	cc-signed-integer)
(cc--define-custom-number-object cc-uint	cc-unsigned-integer)
(cc--define-custom-number-object cc-slong	cc-signed-integer)
(cc--define-custom-number-object cc-ulong	cc-unsigned-integer)
(cc--define-custom-number-object cc-sllong	cc-signed-integer)
(cc--define-custom-number-object cc-ullong	cc-unsigned-integer)
(cc--define-custom-number-object cc-usize	cc-unsigned-integer)
(cc--define-custom-number-object cc-ssize	cc-signed-integer)
(cc--define-custom-number-object cc-sintmax	cc-signed-integer)
(cc--define-custom-number-object cc-uintmax	cc-unsigned-integer)
(cc--define-custom-number-object cc-ptrdiff	cc-signed-integer)
(cc--define-custom-number-object cc-uint16	cc-unsigned-integer)
(cc--define-custom-number-object cc-sint32	cc-signed-integer)
(cc--define-custom-number-object cc-uint32	cc-unsigned-integer)
(cc--define-custom-number-object cc-sint64	cc-signed-integer)
(cc--define-custom-number-object cc-uint64	cc-unsigned-integer)
(cc--define-custom-number-object cc-float	cc-floating-point)
(cc--define-custom-number-object cc-ldouble	cc-floating-point)


;;;; special initialisation methods

(defmacro cc--define-sint64-maker-method (TYPE-OR-STEM)
  (let* ((TYPE.str	(cc--prepend-prefix-to-symbol-name TYPE-OR-STEM))
	 (TYPE		(intern TYPE.str))
	 (DOCSTRING	(concat "Convert an object of type `" TYPE.str "' into an object of type `cc-sint64'.")))
    `(cl-defmethod cc-sint64 ((init ,TYPE))
       ,DOCSTRING
       (cc--make sint64 :obj (cc--clang-converter ,TYPE-OR-STEM sint64 (cc--extract-obj ,TYPE-OR-STEM init))))))

(cc--define-sint64-maker-method char)
(cc--define-sint64-maker-method schar)
(cc--define-sint64-maker-method sint8)
(cc--define-sint64-maker-method sint16)
(cc--define-sint64-maker-method sint32)
(cc--define-sint64-maker-method sshrt)
(cc--define-sint64-maker-method sint)
(cc--define-sint64-maker-method slong)
(cc--define-sint64-maker-method sllong)
(cc--define-sint64-maker-method sintmax)
(cc--define-sint64-maker-method ssize)
(cc--define-sint64-maker-method ptrdiff)

(cl-defmethod cc-sint64 ((init integer))
  "Convert an object of type `integer' into an object of type `cc-sint64'."
  (cc--make sint64 :obj (cc--clang-converter integer sint64 init)))

(cl-defmethod cc-sint64 ((init float))
  "Convert an object of type `float' into an object of type `cc-sint64'."
  (cc--make sint64 :obj (cc--clang-converter integer sint64 (round init))))

;;; --------------------------------------------------------------------

(defmacro cc--define-uint64-maker-method (TYPE-OR-STEM)
  (let* ((TYPE.str		(cc--prepend-prefix-to-symbol-name TYPE-OR-STEM))
	 (TYPE			(intern TYPE.str))
	 (DOCSTRING		(concat "Convert an object of type `" TYPE.str "' into an object of type `cc-uint64'.")))
    `(cl-defmethod cc-uint64 ((init ,TYPE))
       ,DOCSTRING
       (cc--make uint64 :obj (cc--clang-converter ,TYPE-OR-STEM uint64 (cc--extract-obj ,TYPE-OR-STEM init))))))

(cc--define-uint64-maker-method uchar)
(cc--define-uint64-maker-method uint8)
(cc--define-uint64-maker-method uint16)
(cc--define-uint64-maker-method uint32)
(cc--define-uint64-maker-method ushrt)
(cc--define-uint64-maker-method uint)
(cc--define-uint64-maker-method ulong)
(cc--define-uint64-maker-method ullong)
(cc--define-uint64-maker-method uintmax)
(cc--define-uint64-maker-method usize)
(cc--define-uint64-maker-method wchar)

(cl-defmethod cc-uint64 ((init integer))
  "Convert an object of type `integer' into an object of type `cc-uint64'."
  (cc--make uint64 :obj (cc--clang-converter integer uint64 init)))

(cl-defmethod cc-uint64 ((init float))
  "Convert an object of type `float' into an object of type `cc-sint64'."
  (cc--make uint64 :obj (cc--clang-converter integer uint64 (round init))))

;;; --------------------------------------------------------------------

(cl-defmethod  cc-ldouble ((init integer))
  "Build and return a new instance of `cc-ldouble'."
  (cc-ldouble (float init)))

(defmacro cc--define-ldouble-maker-method (TYPE-OR-STEM)
  (let* ((TYPE.str	(cc--prepend-prefix-to-symbol-name TYPE-OR-STEM))
	 (TYPE		(intern TYPE.str))
	 (DOCSTRING	(concat "Convert an object of type `" TYPE.str "' into an object of type `cc-ldouble'.")))
    `(cl-defmethod cc-ldouble ((init ,TYPE))
       ,DOCSTRING
       (cc--make ldouble :obj (cc--clang-converter ,TYPE-OR-STEM ldouble (cc--extract-obj ,TYPE-OR-STEM init))))))

(cc--define-ldouble-maker-method float)
(cc--define-ldouble-maker-method uint64)
(cc--define-ldouble-maker-method sint64)

(cl-defmethod cc-ldouble ((init cc-unsigned-integer))
  "Convert an object of type `cc-unsigned-integer' to an object of type `cc-ldouble'."
  (cc-ldouble (cc-uint64 init)))

(cl-defmethod cc-ldouble ((init cc-signed-integer))
  "Convert an object of type `cc-signed-integer' to an object of type `cc-ldouble'."
  (cc-ldouble (cc-sint64 init)))

(cl-defmethod cc-ldouble ((init integer))
  "Convert an object of type `integer' into an object of type `cc-ldouble'."
  (cc--make uint64 :obj (cc--clang-converter float ldouble (float init))))

(cl-defmethod cc-ldouble ((init float))
  "Convert an object of type `float' into an object of type `cc-ldouble'."
  (cc--make uint64 :obj (cc--clang-converter float ldouble init)))


;;;; range inclusion

(defmacro cc--define-fits-function (TYPESTEM USRPTR-ARGTYPE NORMALISED-TYPE)
  (let* ((TYPESTEM.str		(symbol-name TYPESTEM))
	 (TYPE			(intern (concat "cc-" TYPESTEM.str)))
	 (FUNCNAME		(intern (concat "cc-fits-" TYPESTEM.str "-p")))
	 (MMUX-FUNCNAME		(intern (concat "mmux-core-c-fits-" TYPESTEM.str "-p")))
	 (DOCSTRING		(concat "Return true if the argument fits an object of type `" (symbol-name TYPE) "'.")))
    `(progn
       (cl-defgeneric ,FUNCNAME (op)
	 ,DOCSTRING)
       (cl-defmethod  ,FUNCNAME ((op ,USRPTR-ARGTYPE))
	 ,DOCSTRING
	 (,MMUX-FUNCNAME (cc--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
       (cl-defmethod  ,FUNCNAME ((op integer))
	 ,DOCSTRING
	 (,MMUX-FUNCNAME (cc--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
       (cl-defmethod  ,FUNCNAME ((op float))
	 ,DOCSTRING
	 (,MMUX-FUNCNAME (cc--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
       )))

(defmacro cc--define-fits-function/signed-integer (TYPESTEM)
  `(cc--define-fits-function ,TYPESTEM cc-signed-integer cc-sint64))

(defmacro cc--define-fits-function/unsigned-integer (TYPESTEM)
  `(cc--define-fits-function ,TYPESTEM cc-unsigned-integer cc-uint64))

(defmacro cc--define-fits-function/floating-point (TYPESTEM)
  `(cc--define-fits-function ,TYPESTEM cc-floating-point cc-ldouble))

(cc--define-fits-function/signed-integer	char)
(cc--define-fits-function/signed-integer	schar)
(cc--define-fits-function/unsigned-integer	uchar)
(cc--define-fits-function/unsigned-integer	wchar)
(cc--define-fits-function/signed-integer	sshrt)
(cc--define-fits-function/unsigned-integer	ushrt)
(cc--define-fits-function/signed-integer	sint)
(cc--define-fits-function/unsigned-integer	uint)
(cc--define-fits-function/signed-integer	slong)
(cc--define-fits-function/unsigned-integer	ulong)
(cc--define-fits-function/signed-integer	sllong)
(cc--define-fits-function/unsigned-integer	ullong)
(cc--define-fits-function/signed-integer	ssize)
(cc--define-fits-function/unsigned-integer	usize)
(cc--define-fits-function/signed-integer	sintmax)
(cc--define-fits-function/unsigned-integer	uintmax)
(cc--define-fits-function/signed-integer	ptrdiff)
(cc--define-fits-function/signed-integer	sint8)
(cc--define-fits-function/unsigned-integer	uint8)
(cc--define-fits-function/signed-integer	sint16)
(cc--define-fits-function/unsigned-integer	uint16)
(cc--define-fits-function/signed-integer	sint32)
(cc--define-fits-function/unsigned-integer	uint32)
(cc--define-fits-function/signed-integer	sint64)
(cc--define-fits-function/unsigned-integer	uint64)
(cc--define-fits-function/floating-point	float)
(cc--define-fits-function/floating-point	ldouble)


;;;; numeric comparison operations
;;
;;To perform a comparison operation we normalise the operands as follows:
;;
;;* We convert all the signed integers to `cc-sint64'.
;;
;;* We convert all the unsigned integers to `cc-uint64'.
;;
;;* We convert all the floating-point numbers to `cc-ldouble'.
;;
;;* When  comparing  integers  and floating-point  numbers  we  convert  all  the integer  types  to
;;  `cc-ldouble'.
;;

(defmacro cc--define-numeric-comparison-generic-functions (OPERATOR)
  (let* ((OPERATOR.str	(symbol-name OPERATOR))
	 (CC-FUNC	(intern (concat "cc"   OPERATOR.str)))
	 (CC-FUNC2	(intern (concat "cc-2" OPERATOR.str)))
	 (DOCSTRING	(concat "Return true if every operand is " OPERATOR.str " to the one following it; otherwise return false."))
	 (DOCSTRING2	(concat "Return true if OP1 " OPERATOR.str " OP2; otherwise return false.")))
    `(progn
       (defun ,CC-FUNC (op &rest ops)
    	 ,DOCSTRING
	 ;;FIXME Should I rewrite this to use `cl-loop'?  (Marco Maggi; Feb 5, 2020)
	 (let ((rv t))
	   (while ops
	     (let ((item (car ops)))
	       (if (,CC-FUNC2 op item)
		   (progn
		     (setq op item)
		     (setq ops (cdr ops)))
		 (progn
		   (setq rv  nil)
		   (setq ops nil)))))
	   rv))

       (cl-defgeneric ,CC-FUNC2 (op1 op2)
	 ,DOCSTRING2)
       )))

(defmacro cc--def-numeric-compar-method (CC-FUNC2 OPERATOR OPERATION TYPE1 CONVERTER1 TYPE2 CONVERTER2)
  ;;Define  a  comparison  methods that  converts  the  operands  and  then invokes  an  appropriate
  ;;operation.  Examples:
  ;;
  ;; (cc--def-numeric-compar-method cc-=2 = cc-=2 integer cc-sint64 cc-float cc-ldouble)
  ;; ==> (cl-defmethod cc-=2 ((op1 integer) (op2 cc-float))
  ;;       "..."
  ;;       (cc-=2 (cc-sint64 op1) (cc-ldouble op2)))
  ;;
  ;; (cc--def-numeric-compar-method cc-=2 = = integer identity integer identity)
  ;; ==> (cl-defmethod cc-=2 ((op1 integer) (op2 integer))
  ;;       "..."
  ;;       (= op1 op2))
  ;;
  (let* ((OPERATOR.str	(symbol-name OPERATOR))
	 (TYPE1.str	(symbol-name TYPE1))
	 (TYPE2.str	(symbol-name TYPE2))
	 (DOCSTRING	(concat "Return true if OP1 " OPERATOR.str " OP2; otherwise return false.

The argument OP1 must be of type `" TYPE1.str "'.

The argument OP2 must be of type `" TYPE2.str "'.
"))
	 (CONVERSION1	(if (eq CONVERTER1 'identity) 'op1 `(,CONVERTER1 op1)))
	 (CONVERSION2	(if (eq CONVERTER2 'identity) 'op2 `(,CONVERTER2 op2))))
    `(cl-defmethod ,CC-FUNC2 ((op1 ,TYPE1) (op2 ,TYPE2))
       ,DOCSTRING
       ;;(cc-debug-print (list ',TYPE1 ',TYPE2 op1 op2))
       (,OPERATION ,CONVERSION1 ,CONVERSION2))
    ))

(defmacro cc--define-numeric-comparison (OPERATOR)
  ;;Define everything needed to perform a comparison operation among exact integers.
  ;;
  (let* ((OPERATOR.str			(symbol-name OPERATOR))
	 (CC-FUNC2			(intern (concat "cc-2" OPERATOR.str)))
	 (OPERATION-SINT64		(intern (concat "mmux-core-c-sint64" OPERATOR.str)))
	 (OPERATION-UINT64		(intern (concat "mmux-core-c-uint64" OPERATOR.str)))
	 (OPERATION-SINT64-UINT64	(intern (concat "mmux-core-c-sint64-uint64" OPERATOR.str)))
	 (OPERATION-UINT64-SINT64	(intern (concat "mmux-core-c-uint64-sint64" OPERATOR.str)))
	 (OPERATION-LDOUBLE		(intern (concat "mmux-core-c-ldouble" OPERATOR.str))))
    `(progn
       (cc--define-numeric-comparison-generic-functions ,OPERATOR)

       ;; These are the methods that actually do the operation on built-in numeric objects.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATOR integer identity integer identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATOR integer identity float   identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATOR float   identity integer identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATOR float   identity float   identity)

       ;; These are the methods that actually do the operation on custom user-pointer objects.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-SINT64        cc-sint64 cc-sint64-obj cc-sint64 cc-sint64-obj)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-UINT64        cc-uint64 cc-uint64-obj cc-uint64 cc-uint64-obj)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-SINT64-UINT64 cc-sint64 cc-sint64-obj cc-uint64 cc-uint64-obj)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-UINT64-SINT64 cc-uint64 cc-uint64-obj cc-sint64 cc-sint64-obj)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-LDOUBLE
				      cc-ldouble cc-ldouble-obj
				      cc-ldouble cc-ldouble-obj)

       ;; These are the methods that normalise operands among operational types.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-sint64 cc-ldouble cc-ldouble identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-uint64 cc-ldouble cc-ldouble identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-ldouble identity cc-sint64 cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-ldouble identity cc-uint64 cc-ldouble)

       ;; These are the methods that normalise among integer types.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer   cc-sint64 cc-signed-integer   cc-sint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64 cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer   cc-sint64 cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64 cc-signed-integer   cc-sint64)

       ;; These are the methods that normalise among floating point types.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point cc-ldouble cc-floating-point cc-ldouble)

       ;; These are the methods that normalise mixed numeric types: `cc-floating-point', `cc-signed-integer', `cc-unsigned-intger'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point   cc-ldouble cc-signed-integer   cc-sint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer   cc-sint64      cc-floating-point   cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point   cc-ldouble cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64      cc-floating-point   cc-ldouble)

       ;; These are the methods that normalise mixed numeric types: `integer' and `cc-floating-point'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 integer           cc-ldouble cc-floating-point cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point cc-ldouble integer           cc-ldouble)

       ;; These are the methods that normalise mixed numeric types: `integer' and `cc-signed-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 integer           cc-sint64 cc-signed-integer cc-sint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer cc-sint64 integer           cc-sint64)

       ;; These are the methods that normalise mixed numeric types: `integer' and `cc-unsigned-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 integer             cc-sint64 cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64 integer             cc-sint64)

       ;; These are the methods that normalise mixed numeric types: `float' and `cc-floating-point'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 float             cc-ldouble cc-floating-point cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point cc-ldouble float             cc-ldouble)

       ;; These are the methods that normalise mixed numeric types: `float' and `cc-signed-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 float             cc-ldouble cc-signed-integer cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer cc-ldouble float             cc-ldouble)

       ;; These are the methods that normalise mixed numeric types: `float' and `cc-unsigned-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 float               cc-ldouble cc-unsigned-integer cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-ldouble float               cc-ldouble)
       )))

(cc--define-numeric-comparison =)
(cc--define-numeric-comparison <)
(cc--define-numeric-comparison >)
(cc--define-numeric-comparison <=)
(cc--define-numeric-comparison >=)
(cc--define-numeric-comparison /=)


;;;; done

(provide 'cc-number-objects)

;;; end of file



;;;; done

(provide 'cc-numeric-type-definitions)

;;; cc-core.el ends here
