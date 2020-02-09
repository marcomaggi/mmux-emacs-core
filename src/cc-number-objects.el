;;; cc-number-objects.el --- numeric type definitions for C language intefaces

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-09 06:04:09 marco>
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

(eval-when-compile
  (require 'cc-basics)
  (require 'cc-constants))
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


;;;; C language type wrappers: char

(defmacro cc--define-custom-number-object (TYPE PARENT-TYPE)
  ;;We want the macro use:
  ;;
  ;;   (cc--define-custom-number-object cc-char cc-signed-integer)
  ;;
  ;;to expand into:
  ;;
  ;;   (cl-defstruct (cc-char
  ;;                   (:include     cc-signed-integer)
  ;;                   (:constructor cc-char--make))
  ;;     obj)
  ;;
  ;;   (cl-defgeneric cc-char (init)
  ;;     "Build and return a new instance of `cc-char'.")
  ;;
  ;;   (cl-defmethod cc-char ((init cc-char))
  ;;     "Build and return a new instance of `cc-char'."
  ;;     (cc-char--make :obj (cc-char-obj init)))
  ;;
  ;;   (cl-defmethod  cc-char ((init cc-signed-integer))
  ;;     "Build and return a new instance of `cc-char'."
  ;;     (let ((obj (cc-sint64 init)))
  ;;       (cl-assert (cc-fits-char-p obj))
  ;;       (cc-char--make :obj (cc-sint64-obj obj))))
  ;;
  (let* ((TYPE.str		(symbol-name TYPE))
	 ;;Strip the leading "cc-" prefix.
	 (TYPESTEM.str		(substring TYPE.str 3))
	 (CONSTRUCTOR		(intern (concat TYPE.str "--make")))
	 (DOCSTRING		(concat "Build and return a new instance of `" TYPE.str "'."))
	 (FITS-FUNC		(intern (concat "cc-fits-" TYPESTEM.str "-p")))
	 (TYPE-OBJ		(intern (concat TYPE.str "-obj"))))
    (cl-multiple-value-bind (BUILTIN-INIT-TYPE NORMALISER)
	(cond ((eq PARENT-TYPE 'cc-signed-integer)	(cl-values 'integer	'cc-sint64))
	      ((eq PARENT-TYPE 'cc-unsigned-integer)	(cl-values 'integer	'cc-uint64))
	      ((eq PARENT-TYPE 'cc-floating-point)	(cl-values 'float	'cc-long-double))
	      (t
	       (signal 'mmec-error-unsupported-init-type PARENT-TYPE)))
      (let* ((NORMALISER-OBJ	(intern (concat (symbol-name NORMALISER) "-obj"))))
	`(progn
	   (cl-defstruct (,TYPE
			  (:include	,PARENT-TYPE)
			  (:constructor	,CONSTRUCTOR))
	     obj)

	   (cl-defgeneric ,TYPE (init)
	     ,DOCSTRING)

	   (cl-defmethod ,TYPE ((init ,TYPE))
	     ,DOCSTRING
	     (,CONSTRUCTOR :obj (,TYPE-OBJ init)))

	   (cl-defmethod ,TYPE ((init ,BUILTIN-INIT-TYPE))
	     ,DOCSTRING
	     (,CONSTRUCTOR :obj (,TYPE-OBJ init)))

	   (cl-defmethod ,TYPE ((init cc-signed-integer))
	     ,DOCSTRING
	     (let ((obj (,NORMALISER init)))
	       (cl-assert (,FITS-FUNC obj))
	       (,CONSTRUCTOR :obj (,NORMALISER-OBJ obj))))

	   )))))

;;These
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
(cc--define-custom-number-object cc-long-double	cc-floating-point)


;;;; special initialisation methods

(defmacro cc--define-sint64-maker-method (TYPE CSTEM)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (OBJ-GETTER	(intern (concat TYPE.str "-obj")))
	 (C-CONVERTER	(intern (concat "mmux-core-c-" CSTEM "-to-sint64")))
	 (DOCSTRING	(concat "Convert an object of type `" TYPE.str "' into an object of type `cc-sint64'.")))
    `(cl-defmethod cc-sint64 ((init ,TYPE))
       ,DOCSTRING
       (cc-sint64--make :obj (,C-CONVERTER (,OBJ-GETTER init))))))

(cc--define-sint64-maker-method cc-char		"char")
(cc--define-sint64-maker-method cc-schar	"schar")
(cc--define-sint64-maker-method cc-sint8	"sint8")
(cc--define-sint64-maker-method cc-sint16	"sint16")
(cc--define-sint64-maker-method cc-sint32	"sint32")
(cc--define-sint64-maker-method cc-sshrt	"sshrt")
(cc--define-sint64-maker-method cc-sint		"sint")
(cc--define-sint64-maker-method cc-slong	"slong")
(cc--define-sint64-maker-method cc-sllong	"sllong")
(cc--define-sint64-maker-method cc-sintmax	"sintmax")
(cc--define-sint64-maker-method cc-ssize	"ssize")
(cc--define-sint64-maker-method cc-ptrdiff	"ptrdiff")

;;; --------------------------------------------------------------------

(defmacro cc--define-uint64-maker-method (TYPE CSTEM)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (OBJ-GETTER	(intern (concat TYPE.str "-obj")))
	 (C-CONVERTER	(intern (concat "mmux-core-c-" CSTEM "-to-uint64")))
	 (DOCSTRING	(concat "Convert an object of type `" TYPE.str "' into an object of type `cc-uint64'.")))
    `(cl-defmethod cc-uint64 ((init ,TYPE))
       ,DOCSTRING
       (cc-uint64--make :obj (,C-CONVERTER (,OBJ-GETTER init))))))

(cc--define-uint64-maker-method cc-uchar	"uchar")
(cc--define-uint64-maker-method cc-uint8	"uint8")
(cc--define-uint64-maker-method cc-uint16	"uint16")
(cc--define-uint64-maker-method cc-uint32	"uint32")
(cc--define-uint64-maker-method cc-ushrt	"ushrt")
(cc--define-uint64-maker-method cc-uint		"uint")
(cc--define-uint64-maker-method cc-ulong	"ulong")
(cc--define-uint64-maker-method cc-ullong	"ullong")
(cc--define-uint64-maker-method cc-uintmax	"uintmax")
(cc--define-uint64-maker-method cc-usize	"usize")
(cc--define-uint64-maker-method cc-wchar	"wchar")

;;; --------------------------------------------------------------------

(cl-defmethod  cc-long-double ((init integer))
  "Build and return a new instance of `cc-long-double'."
  (cc-long-double (float init)))

(defmacro cc--define-long-double-maker-method (TYPE CSTEM)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (OBJ-GETTER	(intern (concat TYPE.str "-obj")))
	 (C-CONVERTER	(intern (concat "mmux-core-c-" CSTEM "-to-long-double")))
	 (DOCSTRING	(concat "Convert an object of type `" TYPE.str "' into an object of type `cc-long-double'.")))
    `(cl-defmethod cc-long-double ((init ,TYPE))
       ,DOCSTRING
       (cc-long-double--make :obj (,C-CONVERTER (,OBJ-GETTER init))))))

(cc--define-long-double-maker-method cc-float	"float")
(cc--define-long-double-maker-method cc-uint64	"uint64")
(cc--define-long-double-maker-method cc-sint64	"sint64")

(cl-defmethod cc-long-double ((init cc-unsigned-integer))
  "Convert an object of type `cc-unsigned-integer' to an object of type `cc-long-double'."
  (cc-long-double (cc-uint64 init)))

(cl-defmethod cc-long-double ((init cc-signed-integer))
  "Convert an object of type `cc-signed-integer' to an object of type `cc-long-double'."
  (cc-long-double (cc-sint64 init)))


;;;; range inclusion

(defmacro cc--define-fits-function (TYPESTEM ARGTYPE NORMALISER-TYPE)
  (let* ((TYPESTEM.str		(symbol-name TYPESTEM))
	 (TYPE			(intern (concat "cc-" TYPESTEM.str)))
	 (FUNCNAME		(intern (concat "cc-fits-" TYPESTEM.str "-p")))
	 (MMUX-FUNCNAME		(intern (concat "mmux-core-c-fits-" TYPESTEM.str "-p")))
	 (DOCSTRING		(concat "Return true if the argument fits an object of type `" (symbol-name TYPE) "'."))
	 (NORMALISER-TYPE.str	(symbol-name NORMALISER-TYPE))
	 (NORMALISED-OBJ	(intern (concat NORMALISER-TYPE.str "-obj"))))
    `(progn
       (cl-defgeneric ,FUNCNAME (op)
	 ,DOCSTRING)
       (cl-defmethod  ,FUNCNAME ((op ,ARGTYPE))
	 ,DOCSTRING
	 (,MMUX-FUNCNAME (,NORMALISED-OBJ (,NORMALISER-TYPE op))))
       )))

(cc--define-fits-function char			cc-signed-integer	cc-sint64)
(cc--define-fits-function schar			cc-signed-integer	cc-sint64)
(cc--define-fits-function uchar			cc-unsigned-integer	cc-uint64)
(cc--define-fits-function wchar			cc-unsigned-integer	cc-uint64)
(cc--define-fits-function sshrt			cc-signed-integer	cc-sint64)
(cc--define-fits-function ushrt			cc-unsigned-integer	cc-uint64)
(cc--define-fits-function sint			cc-signed-integer	cc-sint64)
(cc--define-fits-function uint			cc-unsigned-integer	cc-uint64)
(cc--define-fits-function slong			cc-signed-integer	cc-sint64)
(cc--define-fits-function ulong			cc-unsigned-integer	cc-uint64)
(cc--define-fits-function sllong		cc-signed-integer	cc-sint64)
(cc--define-fits-function ullong		cc-unsigned-integer	cc-uint64)
(cc--define-fits-function ssize			cc-signed-integer	cc-sint64)
(cc--define-fits-function usize			cc-unsigned-integer	cc-uint64)
(cc--define-fits-function sintmax		cc-signed-integer	cc-sint64)
(cc--define-fits-function uintmax		cc-unsigned-integer	cc-uint64)
(cc--define-fits-function ptrdiff		cc-signed-integer	cc-sint64)
(cc--define-fits-function sint8			cc-signed-integer	cc-sint64)
(cc--define-fits-function uint8			cc-unsigned-integer	cc-uint64)
(cc--define-fits-function sint16		cc-signed-integer	cc-sint64)
(cc--define-fits-function uint16		cc-unsigned-integer	cc-uint64)
(cc--define-fits-function sint32		cc-signed-integer	cc-sint64)
(cc--define-fits-function uint32		cc-unsigned-integer	cc-uint64)
(cc--define-fits-function sint64		cc-signed-integer	cc-sint64)
(cc--define-fits-function uint64		cc-unsigned-integer	cc-uint64)
(cc--define-fits-function float			cc-floating-point	cc-long-double)
;;(cc--define-fits-function cc-double		cc-floating-point	cc-long-double)
(cc--define-fits-function long-double		cc-floating-point	cc-long-double)


;;;; numeric comparison operations
;;
;;To perform a comparison operation we normalise the operands as follows:
;;
;;* We convert all the signed integers to `cc-sint64'.
;;
;;* We convert all the unsigned integers to `cc-uint64'.
;;
;;* We convert all the floating-point numbers to `cc-long-double'.
;;
;;* When  comparing  integers  and floating-point  numbers  we  convert  all  the integer  types  to
;;  `cc-long-double'.
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
  ;; (cc--def-numeric-compar-method cc-=2 = cc-=2 integer cc-sint64 cc-float cc-long-double)
  ;; ==> (cl-defmethod cc-=2 ((op1 integer) (op2 cc-float))
  ;;       "..."
  ;;       (cc-=2 (cc-sint64 op1) (cc-long-double op2)))
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
	 (OPERATION-LONG-DOUBLE		(intern (concat "mmux-core-c-long-double" OPERATOR.str))))
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
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-LONG-DOUBLE
				      cc-long-double cc-long-double-obj
				      cc-long-double cc-long-double-obj)

       ;; These are the methods that normalise operands among operational types.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-sint64 cc-long-double cc-long-double identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-uint64 cc-long-double cc-long-double identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-long-double identity cc-sint64 cc-long-double)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-long-double identity cc-uint64 cc-long-double)

       ;; These are the methods that normalise among integer types.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer   cc-sint64 cc-signed-integer   cc-sint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64 cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer   cc-sint64 cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64 cc-signed-integer   cc-sint64)

       ;; These are the methods that normalise among floating point types.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point cc-long-double cc-floating-point cc-long-double)

       ;; These are the methods that normalise mixed numeric types: `cc-floating-point', `cc-signed-integer', `cc-unsigned-intger'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point   cc-long-double cc-signed-integer   cc-sint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer   cc-sint64      cc-floating-point   cc-long-double)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point   cc-long-double cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64      cc-floating-point   cc-long-double)

       ;; These are the methods that normalise mixed numeric types: `integer' and `cc-floating-point'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 integer           cc-long-double cc-floating-point cc-long-double)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point cc-long-double integer           cc-long-double)

       ;; These are the methods that normalise mixed numeric types: `integer' and `cc-signed-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 integer           cc-sint64 cc-signed-integer cc-sint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer cc-sint64 integer           cc-sint64)

       ;; These are the methods that normalise mixed numeric types: `integer' and `cc-unsigned-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 integer             cc-sint64 cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64 integer             cc-sint64)

       ;; These are the methods that normalise mixed numeric types: `float' and `cc-floating-point'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 float             cc-long-double cc-floating-point cc-long-double)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point cc-long-double float             cc-long-double)

       ;; These are the methods that normalise mixed numeric types: `float' and `cc-signed-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 float             cc-long-double cc-signed-integer cc-long-double)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer cc-long-double float             cc-long-double)

       ;; These are the methods that normalise mixed numeric types: `float' and `cc-unsigned-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 float               cc-long-double cc-unsigned-integer cc-long-double)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-long-double float               cc-long-double)
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
