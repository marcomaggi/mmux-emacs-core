;;; cc-number-objects.el --- numeric type definitions for C language intefaces

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-06 15:15:11 marco>
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

(cl-defstruct (cc-char
	       (:include	cc-signed-integer)
	       (:constructor	cc-char--make))
  obj)

(cl-defgeneric cc-char (init)
  "Build and return a new instance of `cc-char'.")
(cl-defmethod  cc-char ((init integer))
  "Build and return a new instance of `cc-char'."
  (cl-assert (cc-char-range-p init))
  (cc-char--make :obj init))

(cc--define-self-object-maker-method cc-char)

(defun cc-char-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `char'."
  (and (integerp op)
       (<= cc-CHAR_MIN op cc-CHAR_MAX)))


;;;; C language type wrappers: signed char

(cl-defstruct (cc-schar
	       (:include	cc-signed-integer)
	       (:constructor	cc-schar--make))
  obj)

(cl-defgeneric cc-schar (init)
  "Build and return a new instance of `cc-schar'.")
(cl-defmethod  cc-schar ((init integer))
  "Build and return a new instance of `cc-schar'."
  (cl-assert (cc-schar-range-p init))
  (cc-schar--make :obj init))

(cc--define-self-object-maker-method cc-schar)

(defun cc-schar-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `signed char'."
  (and (integerp op)
       (<= cc-SCHAR_MIN op cc-SCHAR_MAX)))


;;;; C language type wrappers: unsigned char

(cl-defstruct (cc-uchar
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uchar--make))
  obj)

(cl-defgeneric cc-uchar (init)
  "Build and return a new instance of `cc-uchar'.")
(cl-defmethod  cc-uchar ((init integer))
  "Build and return a new instance of `cc-uchar'."
  (cl-assert (cc-uchar-range-p init))
  (cc-uchar--make :obj init))

(cc--define-self-object-maker-method cc-uchar)

(defun cc-uchar-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `unsigned char'."
  (and (integerp op)
       (<= cc-UCHAR_MIN op cc-UCHAR_MAX)))


;;;; C language type wrappers: wchar

(cl-defstruct (cc-wchar
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-wchar--make))
  obj)

(cl-defgeneric cc-wchar (init)
  "Build and return a new instance of `cc-wchar'.")
(cl-defmethod  cc-wchar ((init integer))
  "Build and return a new instance of `cc-wchar'."
  (cl-assert (cc-wchar-range-p init))
  (cc-wchar--make :obj (mmux-core-c-make-wchar init)))

(cc--define-self-object-maker-method cc-wchar)

(defun cc-wchar-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `wchar_t'."
  (and (integerp op)
       (<= cc-WCHAR_MIN op cc-WCHAR_MAX)))


;;;; C language type wrappers: signed short int

(cl-defstruct (cc-signed-short-int
	       (:include	cc-signed-integer)
	       (:constructor	cc-signed-short-int--make))
  obj)

(cl-defgeneric cc-signed-short-int (init)
  "Build and return a new instance of `cc-signed-short-int'.")
(cl-defmethod  cc-signed-short-int ((init integer))
  "Build and return a new instance of `cc-signed-short-int'."
  (cl-assert (cc-signed-short-int-range-p init))
  (cc-signed-short-int--make :obj init))

(cc--define-self-object-maker-method cc-signed-short-int)

(defun cc-signed-short-int-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `signed short int'."
  (and (integerp op)
       (<= cc-SHRT_MIN op cc-SHRT_MAX)))


;;;; C language type wrappers: unsigned short int

(cl-defstruct (cc-unsigned-short-int
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-unsigned-short-int--make))
  obj)

(cl-defgeneric cc-unsigned-short-int (init)
  "Build and return a new instance of `cc-unsigned-short-int'.")
(cl-defmethod  cc-unsigned-short-int ((init integer))
  "Build and return a new instance of `cc-unsigned-short-int'."
  (cl-assert (cc-unsigned-short-int-range-p init))
  (cc-unsigned-short-int--make :obj init))

(cc--define-self-object-maker-method cc-unsigned-short-int)

(defun cc-unsigned-short-int-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `unsigned short int'."
  (and (integerp op)
       (<= cc-USHRT_MIN op cc-USHRT_MAX)))


;;;; C language type wrappers: signed int

(cl-defstruct (cc-signed-int
	       (:include	cc-signed-integer)
	       (:constructor	cc-signed-int--make))
  obj)

(cl-defgeneric cc-signed-int (init)
  "Build and return a new instance of `cc-signed-int'.")
(cl-defmethod  cc-signed-int ((init integer))
  "Build and return a new instance of `cc-signed-int'."
  (cl-assert (cc-signed-int-range-p init))
  (cc-signed-int--make :obj (mmux-core-c-make-sint init)))

(cc--define-self-object-maker-method cc-signed-int)

(defun cc-signed-int-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `signed int'."
  (and (integerp op)
       (<= cc-INT_MIN op cc-INT_MAX)))


;;;; C language type wrappers: unsigned int

(cl-defstruct (cc-unsigned-int
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-unsigned-int--make))
  obj)

(cl-defgeneric cc-unsigned-int (init)
  "Build and return a new instance of `cc-unsigned-int'.")
(cl-defmethod  cc-unsigned-int ((init integer))
  "Build and return a new instance of `cc-unsigned-int'."
  (cl-assert (cc-unsigned-int-range-p init))
  (cc-unsigned-int--make :obj (mmux-core-c-make-uint init)))

(cc--define-self-object-maker-method cc-unsigned-int)

(defun cc-unsigned-int-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `unsigned int'."
  (and (integerp op)
       (<= cc-UINT_MIN op cc-UINT_MAX)))


;;;; C language type wrappers: signed long int

(cl-defstruct (cc-signed-long-int
	       (:include	cc-signed-integer)
	       (:constructor	cc-signed-long-int--make))
  obj)

(cl-defgeneric cc-signed-long-int (init)
  "Build and return a new instance of `cc-signed-long-int'.")
(cl-defmethod  cc-signed-long-int ((init integer))
  "Build and return a new instance of `cc-signed-long-int'."
  (cl-assert (cc-signed-long-int-range-p init))
  (cc-signed-long-int--make :obj (mmux-core-c-make-slong init)))

(cc--define-self-object-maker-method cc-signed-long-int)

(defun cc-signed-long-int-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `signed long int'."
  (and (integerp op)
       (<= cc-LONG_MIN op cc-LONG_MAX)))


;;;; C language type wrappers: unsigned long int

(cl-defstruct (cc-unsigned-long-int
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-unsigned-long-int--make))
  obj)

(cl-defgeneric cc-unsigned-long-int (init)
  "Build and return a new instance of `cc-unsigned-long-int'.")
(cl-defmethod  cc-unsigned-long-int ((init integer))
  "Build and return a new instance of `cc-unsigned-long-int'."
  (cl-assert (cc-unsigned-long-int-range-p init))
  (cc-unsigned-long-int--make :obj (mmux-core-c-make-ulong init)))

(cc--define-self-object-maker-method cc-unsigned-long-int)

(defun cc-unsigned-long-int-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `unsigned long int'."
  (and (integerp op)
       (<= cc-ULONG_MIN op cc-ULONG_MAX)))


;;;; C language type wrappers: signed long long int

(cl-defstruct (cc-signed-long-long-int
	       (:include	cc-signed-integer)
	       (:constructor	cc-signed-long-long-int--make))
  obj)

(cl-defgeneric cc-signed-long-long-int (init)
  "Build and return a new instance of `cc-signed-long-long-int'.")
(cl-defmethod  cc-signed-long-long-int ((init integer))
  "Build and return a new instance of `cc-signed-long-long-int'."
  (cl-assert (cc-signed-long-long-int-range-p init))
  (cc-signed-long-long-int--make :obj (mmux-core-c-make-sllong init)))

(cc--define-self-object-maker-method cc-signed-long-long-int)

(defun cc-signed-long-long-int-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `signed long long int'."
  (and (integerp op)
       (<= cc-LLONG_MIN op cc-LLONG_MAX)))


;;;; C language type wrappers: unsigned long long int

(cl-defstruct (cc-unsigned-long-long-int
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-unsigned-long-long-int--make))
  obj)

(cl-defgeneric cc-unsigned-long-long-int (init)
  "Build and return a new instance of `cc-unsigned-long-long-int'.")
(cl-defmethod  cc-unsigned-long-long-int ((init integer))
  "Build and return a new instance of `cc-unsigned-long-long-int'."
  (cl-assert (cc-unsigned-long-long-int-range-p init))
  (cc-unsigned-long-long-int--make :obj (mmux-core-c-make-ullong init)))

(cc--define-self-object-maker-method cc-unsigned-long-long-int)

(defun cc-unsigned-long-long-int-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `unsigned long long int'."
  (and (integerp op)
       (<= cc-ULLONG_MIN op cc-ULLONG_MAX)))


;;;; C language type wrappers: size_t

(cl-defstruct (cc-usize
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-usize--make))
  obj)

(cl-defgeneric cc-usize (init)
  "Build and return a new instance of `cc-usize'.")
(cl-defmethod  cc-usize ((init integer))
  "Build and return a new instance of `cc-usize'."
  (cl-assert (cc-usize-range-p init))
  (cc-usize--make :obj (mmux-core-c-make-usize init)))

(cc--define-self-object-maker-method cc-usize)

(defun cc-usize-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `size_t'."
  (and (integerp op)
       (<= cc-SIZE_T_MIN op cc-SIZE_T_MAX)))


;;;; C language type wrappers: ssize_t

(cl-defstruct (cc-ssize
	       (:include	cc-signed-integer)
	       (:constructor	cc-ssize--make))
  obj)

(cl-defgeneric cc-ssize (init)
  "Build and return a new instance of `cc-ssize'.")
(cl-defmethod  cc-ssize ((init integer))
  "Build and return a new instance of `cc-ssize'."
  (cl-assert (cc-ssize-range-p init))
  (cc-ssize--make :obj (mmux-core-c-make-ssize init)))

(cc--define-self-object-maker-method cc-ssize)

(defun cc-ssize-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `ssize_t'."
  (and (integerp op)
       (<= cc-SSIZE_T_MIN op cc-SSIZE_T_MAX)))


;;;; C language type wrappers: signed intmax

(cl-defstruct (cc-sintmax
	       (:include	cc-signed-integer)
	       (:constructor	cc-sintmax--make))
  obj)

(cl-defgeneric cc-sintmax (init)
  "Build and return a new instance of `cc-sintmax'.")
(cl-defmethod  cc-sintmax ((init integer))
  "Build and return a new instance of `cc-sintmax'."
  (cl-assert (cc-sintmax-range-p init))
  (cc-sintmax--make :obj (mmux-core-c-make-sintmax init)))

(cc--define-self-object-maker-method cc-sintmax)

(defun cc-sintmax-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `intmax_t'."
  (and (integerp op)
       (<= cc-INTMAX_MIN op cc-INTMAX_MAX)))


;;;; C language type wrappers: uintmax

(cl-defstruct (cc-uintmax
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uintmax--make))
  obj)

(cl-defgeneric cc-uintmax (init)
  "Build and return a new instance of `cc-uintmax'.")
(cl-defmethod  cc-uintmax ((init integer))
  "Build and return a new instance of `cc-uintmax'."
  (cl-assert (cc-uintmax-range-p init))
  (cc-uintmax--make :obj (mmux-core-c-make-uintmax init)))

(cc--define-self-object-maker-method cc-uintmax)

(defun cc-uintmax-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `uintmax_t'."
  (and (integerp op)
       (<= cc-UINTMAX_MIN op cc-UINTMAX_MAX)))


;;;; C language type wrappers: signed intmax

(cl-defstruct (cc-ptrdiff
	       (:include	cc-signed-integer)
	       (:constructor	cc-ptrdiff--make))
  obj)

(cl-defgeneric cc-ptrdiff (init)
  "Build and return a new instance of `cc-ptrdiff'.")
(cl-defmethod  cc-ptrdiff ((init integer))
  "Build and return a new instance of `cc-ptrdiff'."
  (cl-assert (cc-ptrdiff-range-p init))
  (cc-ptrdiff--make :obj (mmux-core-c-make-ptrdiff init)))

(cc--define-self-object-maker-method cc-ptrdiff)

(defun cc-ptrdiff-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `intmax_t'."
  (and (integerp op)
       (<= cc-PTRDIFF_MIN op cc-PTRDIFF_MAX)))


;;;; C language type wrappers: int8_t

(cl-defstruct (cc-sint8
	       (:include	cc-signed-integer)
	       (:constructor	cc-sint8--make))
  obj)

(cl-defgeneric cc-sint8 (init)
  "Build and return a new instance of `cc-sint8'.")
(cl-defmethod  cc-sint8 ((init integer))
  "Build and return a new instance of `cc-sint8'."
  (cl-assert (cc-sint8-range-p init))
  (cc-sint8--make :obj init))

(cc--define-self-object-maker-method cc-sint8)

(defun cc-sint8-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `int8_t'."
  (and (integerp op)
       (<= cc-INT8_MIN op cc-INT8_MAX)))


;;;; C language type wrappers: uint8_t

(cl-defstruct (cc-uint8
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uint8--make))
  obj)

(cl-defgeneric cc-uint8 (init)
  "Build and return a new instance of `cc-uint8'.")
(cl-defmethod  cc-uint8 ((init integer))
  "Build and return a new instance of `cc-uint8'."
  (cl-assert (cc-uint8-range-p init))
  (cc-uint8--make :obj init))

(cc--define-self-object-maker-method cc-uint8)

(defun cc-uint8-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `uint8_t'."
  (and (integerp op)
       (<= cc-UINT8_MIN op cc-UINT8_MAX)))


;;;; C language type wrappers: int16_t

(cl-defstruct (cc-sint16
	       (:include	cc-signed-integer)
	       (:constructor	cc-sint16--make))
  obj)

(cl-defgeneric cc-sint16 (init)
  "Build and return a new instance of `cc-sint16'.")
(cl-defmethod  cc-sint16 ((init integer))
  "Build and return a new instance of `cc-sint16'."
  (cl-assert (cc-sint16-range-p init))
  (cc-sint16--make :obj init))

(cc--define-self-object-maker-method cc-sint16)

(defun cc-sint16-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `int16_t'."
  (and (integerp op)
       (<= cc-INT16_MIN op cc-INT16_MAX)))


;;;; C language type wrappers: uint16_t

(cl-defstruct (cc-uint16
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uint16--make))
  obj)

(cl-defgeneric cc-uint16 (init)
  "Build and return a new instance of `cc-uint16'.")
(cl-defmethod  cc-uint16 ((init integer))
  "Build and return a new instance of `cc-uint16'."
  (cl-assert (cc-uint16-range-p init))
  (cc-uint16--make :obj init))

(cc--define-self-object-maker-method cc-uint16)

(defun cc-uint16-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `uint16_t'."
  (and (integerp op)
       (<= cc-UINT16_MIN op cc-UINT16_MAX)))


;;;; C language type wrappers: int32_t

(cl-defstruct (cc-sint32
	       (:include	cc-signed-integer)
	       (:constructor	cc-sint32--make))
  obj)

(cl-defgeneric cc-sint32 (init)
  "Build and return a new instance of `cc-sint32'.")
(cl-defmethod  cc-sint32 ((init integer))
  "Build and return a new instance of `cc-sint32'."
  (cl-assert (cc-sint32-range-p init))
  (cc-sint32--make :obj (mmux-core-c-make-sint32 init)))

(cc--define-self-object-maker-method cc-sint32)

(defun cc-sint32-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `int32_t'."
  (and (integerp op)
       (<= cc-INT32_MIN op cc-INT32_MAX)))


;;;; C language type wrappers: uint32_t

(cl-defstruct (cc-uint32
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uint32--make))
  obj)

(cl-defgeneric cc-uint32 (init)
  "Build and return a new instance of `cc-uint32'.")
(cl-defmethod  cc-uint32 ((init integer))
  "Build and return a new instance of `cc-uint32'."
  (cl-assert (cc-uint32-range-p init))
  (cc-uint32--make :obj (mmux-core-c-make-uint32 init)))

(cc--define-self-object-maker-method cc-uint32)

(defun cc-uint32-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `uint32_t'."
  (and (integerp op)
       (<= cc-UINT32_MIN op cc-UINT32_MAX)))


;;;; C language type wrappers: int64_t

(cl-defstruct (cc-sint64
	       (:include	cc-signed-integer)
	       (:constructor	cc-sint64--make))
  obj)

(cl-defgeneric cc-sint64 (init)
  "Build and return a new instance of `cc-sint64'.")

(cl-defmethod cc-sint64 ((init integer))
  "Build and return a new instance of `cc-sint64'."
  (cl-assert (cc-sint64-range-p init))
  (cc-sint64--make :obj (mmux-core-c-make-sint64 init)))

(defmacro cc--define-sint64-maker-method (TYPE CSTEM)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (OBJ-GETTER	(intern (concat TYPE.str "-obj")))
	 (C-CONVERTER	(intern (concat "mmux-core-c-" CSTEM "-to-sint64")))
	 (DOCSTRING	(concat "Convert an object of type `" TYPE.str "' into an object of type `cc-sint64'.")))
    `(cl-defmethod cc-sint64 ((init ,TYPE))
       ,DOCSTRING
       (cc-sint64--make :obj (,C-CONVERTER (,OBJ-GETTER init))))))

(cc--define-sint64-maker-method cc-char			"char")
(cc--define-sint64-maker-method cc-schar		"schar")
(cc--define-sint64-maker-method cc-sint8		"sint8")
(cc--define-sint64-maker-method cc-sint16		"sint16")
(cc--define-sint64-maker-method cc-sint32		"sint32")
(cc--define-sint64-maker-method cc-signed-short-int	"sshrt")
(cc--define-sint64-maker-method cc-signed-int		"sint")
(cc--define-sint64-maker-method cc-signed-long-int	"slong")
(cc--define-sint64-maker-method cc-signed-long-long-int	"sllong")
(cc--define-sint64-maker-method cc-sintmax		"sintmax")
(cc--define-sint64-maker-method cc-ssize		"ssize")
(cc--define-sint64-maker-method cc-ptrdiff		"ptrdiff")

(cc--define-self-object-maker-method cc-sint64)

(defun cc-sint64-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `int64_t'."
  (and (integerp op)
       (<= cc-INT64_MIN op cc-INT64_MAX)))


;;;; C language type wrappers: uint64_t

(cl-defstruct (cc-uint64
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uint64--make))
  obj)

(cl-defgeneric cc-uint64 (init)
  "Build and return a new instance of `cc-uint64'.")
(cl-defmethod cc-uint64 ((init integer))
  "Build and return a new instance of `cc-uint64'."
  (cl-assert (cc-uint64-range-p init))
  (cc-uint64--make :obj (mmux-core-c-make-uint64 init)))

(defmacro cc--define-uint64-maker-method (TYPE CSTEM)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (OBJ-GETTER	(intern (concat TYPE.str "-obj")))
	 (C-CONVERTER	(intern (concat "mmux-core-c-" CSTEM "-to-uint64")))
	 (DOCSTRING	(concat "Convert an object of type `" TYPE.str "' into an object of type `cc-uint64'.")))
    `(cl-defmethod cc-uint64 ((init ,TYPE))
       ,DOCSTRING
       (cc-uint64--make :obj (,C-CONVERTER (,OBJ-GETTER init))))))

(cc--define-uint64-maker-method cc-uchar			"uchar")
(cc--define-uint64-maker-method cc-uint8			"uint8")
(cc--define-uint64-maker-method cc-uint16			"uint16")
(cc--define-uint64-maker-method cc-uint32			"uint32")
(cc--define-uint64-maker-method cc-unsigned-short-int		"ushrt")
(cc--define-uint64-maker-method cc-unsigned-int			"uint")
(cc--define-uint64-maker-method cc-unsigned-long-int		"ulong")
(cc--define-uint64-maker-method cc-unsigned-long-long-int	"ullong")
(cc--define-uint64-maker-method cc-uintmax			"uintmax")
(cc--define-uint64-maker-method cc-usize			"usize")
(cc--define-uint64-maker-method cc-wchar			"wchar")

(cc--define-self-object-maker-method cc-uint64)

(defun cc-uint64-range-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `uint64_t'."
  (and (integerp op)
       (<= cc-UINT64_MIN op cc-UINT64_MAX)))


;;;; C language type wrappers: float

(cl-defstruct (cc-float
	       (:include	cc-floating-point)
	       (:constructor	cc-float--make))
  obj)

(cl-defgeneric cc-float (init)
  "Build and return a new instance of `cc-float'.")
(cl-defmethod  cc-float ((init float))
  "Build and return a new instance of `cc-float'."
  (cl-assert (cc-float-range-p init))
  (cc-float--make :obj (mmux-core-c-make-float init)))

(cc--define-self-object-maker-method cc-float)

(defun cc-float-range-p (op)
  "Return true if OP is a floating-point object and it is in the range representable `float'."
  (and (floatp op)
       (<= cc-FLT_MIN op cc-FLT_MAX)))


;;;; C language type wrappers: long double

(cl-defstruct (cc-long-double
	       (:include	cc-floating-point)
	       (:constructor	cc-long-double--make))
  obj)

(cl-defgeneric cc-long-double (init)
  "Build and return a new instance of `cc-long-double'.")

(cl-defmethod  cc-long-double ((init float))
  "Build and return a new instance of `cc-long-double'."
  (cl-assert (cc-long-double-range-p init))
  (cc-long-double--make :obj (mmux-core-c-make-long-double init)))

(cl-defmethod  cc-long-double ((init integer))
  "Build and return a new instance of `cc-long-double'."
  (cc-long-double (float init)))

(cc--define-self-object-maker-method cc-long-double)

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

(defun cc-long-double-range-p (op)
  "Return true if OP is a floating-point object and it is in the range representable `long double'."
  (and (floatp op)
       (<= cc-LDBL_MIN op cc-LDBL_MAX)))


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
