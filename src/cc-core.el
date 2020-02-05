;;; cc-core.el --- core definitions for C language intefaces

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  1, 2020
;; Time-stamp: <2020-02-05 07:38:07 marco>
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
  (load "libmmux-emacs-core"))
(load "libmmux-emacs-core")
(require 'cl-lib)
(require 'cc-constants)


;;;; error symbols

(define-error 'mmux-core-error
  "Error while executing a MMUX Emacs Core operation."
  'error)

(define-error 'mmux-core-no-memory-error
  "Error allocating memory."
  'mmux-core-error)

(define-error 'mmux-core-index-out-of-range
  "Attempt to access the internal represenation of an object with an index out of range."
  'mmux-core-error)

(define-error 'mmux-core-bytevector-index-out-of-range
  "Attempt to access the internal represenation of a bytevector object with an index out of range."
  'mmux-core-index-out-of-range)


;;;; helpers

(defmacro cc--define-self-object-maker-method (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (CC-MAKER	(intern (concat TYPE.str "--make")))
	 (OBJ-GETTER	(intern (concat TYPE.str "-obj")))
	 (DOCSTRING	(concat "Build and return a new instance of `" TYPE.str "'.")))
    `(cl-defmethod ,TYPE ((init ,TYPE))
       ,DOCSTRING
       (,CC-MAKER :obj (,OBJ-GETTER init)))))

(defun cc-debug-print (&rest args)
  (pp args 'external-debugging-output))


;;;; C language type wrappers: char

(cl-defstruct (cc-char (:constructor cc-char--make))
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

(cl-defstruct (cc-schar (:constructor cc-schar--make))
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

(cl-defstruct (cc-uchar (:constructor cc-uchar--make))
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

(cl-defstruct (cc-wchar (:constructor cc-wchar--make))
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

(cl-defstruct (cc-signed-short-int (:constructor cc-signed-short-int--make))
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

(cl-defstruct (cc-unsigned-short-int (:constructor cc-unsigned-short-int--make))
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

(cl-defstruct (cc-signed-int (:constructor cc-signed-int--make))
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

(cl-defstruct (cc-unsigned-int (:constructor cc-unsigned-int--make))
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

(cl-defstruct (cc-signed-long-int (:constructor cc-signed-long-int--make))
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

(cl-defstruct (cc-unsigned-long-int (:constructor cc-unsigned-long-int--make))
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

(cl-defstruct (cc-signed-long-long-int (:constructor cc-signed-long-long-int--make))
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

(cl-defstruct (cc-unsigned-long-long-int (:constructor cc-unsigned-long-long-int--make))
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

(cl-defstruct (cc-usize (:constructor cc-usize--make))
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

(cl-defstruct (cc-ssize (:constructor cc-ssize--make))
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

(cl-defstruct (cc-sintmax (:constructor cc-sintmax--make))
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

(cl-defstruct (cc-uintmax (:constructor cc-uintmax--make))
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

(cl-defstruct (cc-ptrdiff (:constructor cc-ptrdiff--make))
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


;;;; C language type wrappers: int8_t, uint8_t

(cl-defstruct (cc-sint8 (:constructor cc-sint8--make))
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

;;; --------------------------------------------------------------------

(cl-defstruct (cc-uint8 (:constructor cc-uint8--make))
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


;;;; C language type wrappers: int16_t, uint16_t

(cl-defstruct (cc-sint16 (:constructor cc-sint16--make))
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

;;; --------------------------------------------------------------------

(cl-defstruct (cc-uint16 (:constructor cc-uint16--make))
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


;;;; C language type wrappers: int32_t, uint32_t

(cl-defstruct (cc-sint32 (:constructor cc-sint32--make))
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

;;; --------------------------------------------------------------------

(cl-defstruct (cc-uint32 (:constructor cc-uint32--make))
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


;;;; C language type wrappers: int64_t, uint64_t

(cl-defstruct (cc-sint64 (:constructor cc-sint64--make))
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

;;; --------------------------------------------------------------------

(cl-defstruct (cc-uint64 (:constructor cc-uint64--make))
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

(cl-defstruct (cc-float (:constructor cc-float--make))
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

(cl-defstruct (cc-long-double (:constructor cc-long-double--make))
  obj)

(cl-defgeneric cc-long-double (init)
  "Build and return a new instance of `cc-long-double'.")

(cl-defmethod  cc-long-double ((init float))
  "Build and return a new instance of `cc-long-double'."
  (cl-assert (cc-long-double-range-p init))
  (cc-long-double--make :obj (mmux-core-c-make-long-double init)))

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

(defun cc-long-double-range-p (op)
  "Return true if OP is a floating-point object and it is in the range representable `long double'."
  (and (floatp op)
       (<= cc-LDBL_MIN op cc-LDBL_MAX)))


;;;; comparison operations among exact integers
;;
;;To perform a comparison operation among exact integers:
;;
;;* We convert all the signed integers to `cc-sint64'.
;;
;;* We convert all the unsigned integers to `cc-uint64'.
;;
;;* We convert all the floating-point numbers to `cc-long-double'.
;;

(defmacro cc--define-integer-comparison-method (OPERATION TYPE CONVERTER)
  ;;Define  a couple  of  comparison  methods that  conver  the operands  into  a  `cc-sint64' or  a
  ;;`cc-uint64', then invoke the generic function again.  Example:
  ;;
  ;;   (cl-defmethod cc= ((op1 cc-sint) (op2 t))
  ;;     (cc= (cc-sint64 op1) op2))
  ;;   (cl-defmethod cc= ((op1 t) (op2 cc-sint64))
  ;;     (cc= op1 (cc-sint64 op2)))
  ;;
  (let* ((OPERATION.str	(symbol-name OPERATION))
	 (CC-FUNC	(intern (concat "cc" (symbol-name OPERATION))))
	 (DOCSTRING	(concat "Return true if OP1 " OPERATION.str " OP2; otherwise return false.")))
    `(progn
       (cl-defmethod ,CC-FUNC ((op1 ,TYPE) (op2 t))
	 ,DOCSTRING
	 ;;(cc-debug-print (list ',CC-FUNC op1 ',TYPE op2 't))
	 (,CC-FUNC (,CONVERTER op1) op2))
       (cl-defmethod ,CC-FUNC ((op1 t) (op2 ,TYPE))
	 ,DOCSTRING
	 ;;(cc-debug-print (list ',CC-FUNC op1 't op2 ',TYPE))
	 (,CC-FUNC op1 (,CONVERTER op2)))
       )))

(defmacro cc--define-integer-comparison (OPERATION)
  ;;Define everything needed to perform a comparison operation among exact integers.
  ;;
  (let* ((OPERATION.str			(symbol-name OPERATION))
	 (CC-FUNC			(intern (concat "cc" OPERATION.str)))
	 (DOCSTRING			(concat "Return true if OP1 " OPERATION.str " OP2; otherwise return false."))
	 (OPERATION-SINT64		(intern (concat "mmux-core-c-sint64" OPERATION.str)))
	 (OPERATION-UINT64		(intern (concat "mmux-core-c-uint64" OPERATION.str)))
	 (OPERATION-SINT64-UINT64	(intern (concat "mmux-core-c-sint64-uint64" OPERATION.str)))
	 (OPERATION-UINT64-SINT64	(intern (concat "mmux-core-c-uint64-sint64" OPERATION.str))))
    `(progn
       (cl-defgeneric ,CC-FUNC (op1 op2)
    	 ,DOCSTRING)

       (cl-defmethod  ,CC-FUNC ((op1 integer)   (op2 integer))   ,DOCSTRING (,OPERATION op1 op2))
       (cl-defmethod  ,CC-FUNC ((op1 cc-sint64) (op2 cc-sint64)) ,DOCSTRING (,OPERATION-SINT64 (cc-sint64-obj op1) (cc-sint64-obj op2)))
       (cl-defmethod  ,CC-FUNC ((op1 cc-uint64) (op2 cc-uint64)) ,DOCSTRING (,OPERATION-UINT64 (cc-uint64-obj op1) (cc-uint64-obj op2)))
       (cl-defmethod  ,CC-FUNC ((op1 cc-sint64) (op2 cc-uint64)) ,DOCSTRING (,OPERATION-SINT64-UINT64 (cc-sint64-obj op1) (cc-uint64-obj op2)))
       (cl-defmethod  ,CC-FUNC ((op1 cc-uint64) (op2 cc-sint64)) ,DOCSTRING (,OPERATION-UINT64-SINT64 (cc-uint64-obj op1) (cc-sint64-obj op2)))

       (cc--define-integer-comparison-method ,OPERATION cc-char				cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-schar			cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-uchar			cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-wchar			cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-signed-short-int		cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-unsigned-short-int		cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-signed-int			cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-unsigned-int			cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-signed-long-int		cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-unsigned-long-int		cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-signed-long-int		cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-unsigned-long-int		cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-signed-long-long-int		cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-unsigned-long-long-int	cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-ssize			cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-usize			cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-sintmax			cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-uintmax			cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-sint8			cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-uint8			cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-sint16			cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-uint16			cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-sint32			cc-sint64)
       (cc--define-integer-comparison-method ,OPERATION cc-uint32			cc-uint64)
       (cc--define-integer-comparison-method ,OPERATION cc-ptrdiff			cc-sint64)
       )))

(cc--define-integer-comparison =)
(cc--define-integer-comparison <)
(cc--define-integer-comparison >)
(cc--define-integer-comparison <=)
(cc--define-integer-comparison >=)
(cc--define-integer-comparison /=)


;;;; comparison operations among floating-point numbers
;;
;;To perform a comparison operation among floating-point numbers:
;;
;;* We convert all the floating-point types to `cc-long-double'.
;;

(defmacro cc--define-floating-point-comparison-method (OPERATION TYPE CONVERTER)
  ;;Define a  couple of comparison  methods that conver the  operands into a  `cc-long-double', then
  ;;invoke the generic function again.  Example:
  ;;
  ;;   (cl-defmethod cc= ((op1 cc-float) (op2 t))
  ;;     (cc= (cc-long-double op1) op2))
  ;;   (cl-defmethod cc= ((op1 t) (op2 cc-float))
  ;;     (cc= op1 (cc-long-double op2)))
  ;;
  (let* ((OPERATION.str	(symbol-name OPERATION))
	 (CC-FUNC	(intern (concat "cc" (symbol-name OPERATION))))
	 (DOCSTRING	(concat "Return true if OP1 " OPERATION.str " OP2; otherwise return false.")))
    `(progn
       (cl-defmethod ,CC-FUNC ((op1 ,TYPE) (op2 t))
	 ,DOCSTRING
	 ;;(cc-debug-print (list ',CC-FUNC op1 ',TYPE op2 't))
	 (,CC-FUNC (,CONVERTER op1) op2))
       (cl-defmethod ,CC-FUNC ((op1 t) (op2 ,TYPE))
	 ,DOCSTRING
	 ;;(cc-debug-print (list ',CC-FUNC op1 't op2 ',TYPE))
	 (,CC-FUNC op1 (,CONVERTER op2)))
       )))

(defmacro cc--define-floating-point-comparison (OPERATION)
  ;;Define everything needed to perform a comparison operation among floating-point numbers.
  ;;
  (let* ((OPERATION.str		(symbol-name OPERATION))
	 (CC-FUNC		(intern (concat "cc" OPERATION.str)))
	 (DOCSTRING		(concat "Return true if OP1 " OPERATION.str " OP2; otherwise return false."))
	 (OPERATION-LONG-DOUBLE	(intern (concat "mmux-core-c-long-double" OPERATION.str))))
    `(progn
       (cl-defgeneric ,CC-FUNC (op1 op2)
    	 ,DOCSTRING)

       (cl-defmethod  ,CC-FUNC ((op1 float) (op2 float))
    	 ,DOCSTRING
    	 (,OPERATION op1 op2))

       (cl-defmethod  ,CC-FUNC ((op1 cc-long-double) (op2 cc-long-double))
    	 ,DOCSTRING
    	 (,OPERATION-LONG-DOUBLE (cc-long-double-obj op1) (cc-long-double-obj op2)))

       (cl-defmethod  ,CC-FUNC ((op1 float) (op2 t))
    	 ,DOCSTRING
    	 (,CC-FUNC (cc-long-double op1) op2))

       (cl-defmethod  ,CC-FUNC ((op1 t) (op2 float))
    	 ,DOCSTRING
    	 (,CC-FUNC op1 (cc-long-double op2)))

       (cc--define-floating-point-comparison-method ,OPERATION cc-float cc-long-double)
       )))

(cc--define-floating-point-comparison =)
(cc--define-floating-point-comparison <)
(cc--define-floating-point-comparison >)
(cc--define-floating-point-comparison <=)
(cc--define-floating-point-comparison >=)
(cc--define-floating-point-comparison /=)


;;;; bytevector objects: object definitions
;;
;; mmux-core-c-bytevector-make NUMBER-OF-SLOTS SLOT-SIZE SIGNED
;;
;;Defined a the C language level.  Build and return a new custom pointer object.
;;
;;The  argument NUMBER-OF-SLOTS  must be  a  non-negative exact  integer  in the  range of  "size_t"
;;representing the number of slots in the bytevector.
;;
;;The argument SLOT-SIZE must be a non-negative  exact integer in the range of "size_t" representing
;;the  size  of  each slot  measured  in  bytes;  valid  values  are:  1, 2,  4,  8,  sizeof(float),
;;sizeof(double), sizeof(long double).
;;
;;The argument SIGNED must be 1 or 0: if the value is 1, the bytevector holds signed integers in its
;;slots; if the value is 0, the bytevector holds unsigned integers in its slots.
;;

(cl-defstruct cc-bytevector
  number-of-slots
  slot-size
  number-of-allocated-bytes
  obj)

(cl-defstruct (cc-integer-bytevector
	       (:include cc-bytevector))
  signed)

(cl-defstruct (cc-float-bytevector
	       (:include cc-bytevector)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-u8
	       (:constructor	cc-bytevector-u8--make)
	       (:include	cc-integer-bytevector)))

(cl-defgeneric cc-bytevector-u8 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-u8'.")
(cl-defmethod  cc-bytevector-u8 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-u8'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-u8--make
   :number-of-slots		number-of-slots
   :slot-size			1
   :signed			nil
   :obj				(mmux-core-c-bytevector-make number-of-slots 1 0)
   :number-of-allocated-bytes	(* number-of-slots 1)))

(cl-defstruct (cc-bytevector-s8
	       (:constructor	cc-bytevector-s8--make)
	       (:include	cc-integer-bytevector)))

(cl-defgeneric cc-bytevector-s8 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-s8'.")
(cl-defmethod  cc-bytevector-s8 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-s8'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-s8--make
   :number-of-slots		number-of-slots
   :slot-size			1
   :signed			t
   :obj				(mmux-core-c-bytevector-make number-of-slots 1 1)
   :number-of-allocated-bytes	(* number-of-slots 1)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-u16
	       (:constructor	cc-bytevector-u16--make)
	       (:include	cc-integer-bytevector)))

(cl-defgeneric cc-bytevector-u16 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-u16'.")
(cl-defmethod  cc-bytevector-u16 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-u16'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-u16--make
   :number-of-slots		number-of-slots
   :slot-size			2
   :signed			nil
   :obj				(mmux-core-c-bytevector-make number-of-slots 2 0)
   :number-of-allocated-bytes	(* number-of-slots 2)))

(cl-defstruct (cc-bytevector-s16
	       (:constructor	cc-bytevector-s16--make)
	       (:include	cc-integer-bytevector)))

(cl-defgeneric cc-bytevector-s16 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-s16'.")
(cl-defmethod  cc-bytevector-s16 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-s16'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-s16--make
   :number-of-slots		number-of-slots
   :slot-size			2
   :signed			t
   :obj				(mmux-core-c-bytevector-make number-of-slots 2 1)
   :number-of-allocated-bytes	(* number-of-slots 2)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-u32
	       (:constructor	cc-bytevector-u32--make)
	       (:include	cc-integer-bytevector)))

(cl-defgeneric cc-bytevector-u32 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-u32'.")
(cl-defmethod  cc-bytevector-u32 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-u32'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-u32--make
   :number-of-slots		number-of-slots
   :slot-size			4
   :signed			nil
   :obj				(mmux-core-c-bytevector-make number-of-slots 4 0)
   :number-of-allocated-bytes	(* number-of-slots 4)))

(cl-defstruct (cc-bytevector-s32
	       (:constructor	cc-bytevector-s32--make)
	       (:include	cc-integer-bytevector)))

(cl-defgeneric cc-bytevector-s32 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-s32'.")
(cl-defmethod  cc-bytevector-s32 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-s32'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-s32--make
   :number-of-slots		number-of-slots
   :slot-size			4
   :signed			t
   :obj				(mmux-core-c-bytevector-make number-of-slots 4 1)
   :number-of-allocated-bytes	(* number-of-slots 4)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-u64
	       (:constructor	cc-bytevector-u64--make)
	       (:include	cc-integer-bytevector)))

(cl-defgeneric cc-bytevector-u64 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-u64'.")
(cl-defmethod  cc-bytevector-u64 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-u64'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-u64--make
   :number-of-slots		number-of-slots
   :slot-size			8
   :signed			nil
   :obj				(mmux-core-c-bytevector-make number-of-slots 8 0)
   :number-of-allocated-bytes	(* number-of-slots 8)))

(cl-defstruct (cc-bytevector-s64
	       (:constructor	cc-bytevector-s64--make)
	       (:include	cc-integer-bytevector)))

(cl-defgeneric cc-bytevector-s64 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-s64'.")
(cl-defmethod  cc-bytevector-s64 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-s64'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-s64--make
   :number-of-slots		number-of-slots
   :slot-size			8
   :signed			t
   :obj				(mmux-core-c-bytevector-make number-of-slots 8 1)
   :number-of-allocated-bytes	(* number-of-slots 8)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-float
	       (:constructor	cc-bytevector-float--make)
	       (:include	cc-float-bytevector)))

(cl-defgeneric cc-bytevector-float (number-of-slots)
  "Build and return a new instance of `cc-bytevector-float'.")
(cl-defmethod  cc-bytevector-float ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-float'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-float--make
   :number-of-slots		number-of-slots
   :slot-size			cc-SIZEOF_FLOAT
   :obj				(mmux-core-c-bytevector-make number-of-slots cc-SIZEOF_FLOAT 1)
   :number-of-allocated-bytes	(* number-of-slots cc-SIZEOF_FLOAT)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-double
	       (:constructor	cc-bytevector-double--make)
	       (:include	cc-float-bytevector)))

(cl-defgeneric cc-bytevector-double (number-of-slots)
  "Build and return a new instance of `cc-bytevector-double'.")
(cl-defmethod  cc-bytevector-double ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-double'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-double--make
   :number-of-slots		number-of-slots
   :slot-size			cc-SIZEOF_DOUBLE
   :obj				(mmux-core-c-bytevector-make number-of-slots cc-SIZEOF_DOUBLE 1)
   :number-of-allocated-bytes	(* number-of-slots cc-SIZEOF_DOUBLE)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-long-double
	       (:constructor	cc-bytevector-long-double--make)
	       (:include	cc-float-bytevector)))

(cl-defgeneric cc-bytevector-long-double (number-of-slots)
  "Build and return a new instance of `cc-bytevector-long-double'.")
(cl-defmethod  cc-bytevector-long-double ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-long-double'."
  (cl-assert (<= 0 number-of-slots))
  (cc-bytevector-long-double--make
   :number-of-slots		number-of-slots
   :slot-size			cc-SIZEOF_LONG_DOUBLE
   :obj				(mmux-core-c-bytevector-make number-of-slots cc-SIZEOF_LONG_DOUBLE 1)
   :number-of-allocated-bytes	(* number-of-slots cc-SIZEOF_LONG_DOUBLE)))


;;;; bytevector objects: getters and setters

(cl-defgeneric cc-bytevector-ref (bv idx)
  "Extract a value from a bytevector.")

(cl-defgeneric cc-bytevector-set! (bv idx val)
  "Store the value VAL into the bytevector BV at index IDX.")

(defmacro cc--define-bytevector-getter (TYPESTEM LTYPE CTYPE)
  (let* ((TYPESTEM.str		(symbol-name TYPESTEM))
	 (LTYPE.str		(symbol-name LTYPE))
	 (BYTEVECTOR-TYPE	(intern (concat "cc-bytevector-" TYPESTEM.str)))
	 (DOCSTRING		(concat "Extract a value of type `" (symbol-name CTYPE) "' from the bytevector BV at index IDX."))
	 (MAKE-LTYPE		LTYPE)
	 (C-FUNC		(intern (concat "mmux-core-c-bytevector-" TYPESTEM.str "-ref"))))
    `(cl-defmethod cc-bytevector-ref ((bv ,BYTEVECTOR-TYPE) (idx integer))
       ,DOCSTRING
       (cl-assert (cc-usize-range-p idx))
       (,MAKE-LTYPE (,C-FUNC (cc-bytevector-obj bv) idx)))))

(defmacro cc--define-bytevector-setter (TYPESTEM LTYPE CTYPE)
  (let* ((TYPESTEM.str		(symbol-name TYPESTEM))
	 (LTYPE.str		(symbol-name LTYPE))
	 (BYTEVECTOR-TYPE	(intern (concat "cc-bytevector-" TYPESTEM.str)))
	 (DOCSTRING		(concat "Store a value VAL of type `" (symbol-name CTYPE) "' into the bytevector BV at index IDX."))
	 (LTYPE-OBJ		(intern (concat LTYPE.str "-obj")))
	 (C-FUNC		(intern (concat "mmux-core-c-bytevector-" TYPESTEM.str "-set!"))))
    `(cl-defmethod cc-bytevector-set! ((bv ,BYTEVECTOR-TYPE) (idx integer) (val ,LTYPE))
       ,DOCSTRING
       (cl-assert (cc-usize-range-p idx))
       (,C-FUNC (cc-bytevector-obj bv) idx (,LTYPE-OBJ val)))))

(defmacro cc--define-bytevector-getter-and-setter (TYPESTEM LTYPE CTYPE)
  `(progn
     (cc--define-bytevector-getter ,TYPESTEM ,LTYPE ,CTYPE)
     (cc--define-bytevector-setter ,TYPESTEM ,LTYPE ,CTYPE)))

(cc--define-bytevector-getter-and-setter u8	cc-uint8	uint8_t)
(cc--define-bytevector-getter-and-setter s8	cc-sint8	int8_t)
(cc--define-bytevector-getter-and-setter u16	cc-uint16	uint16_t)
(cc--define-bytevector-getter-and-setter s16	cc-sint16	int16_t)
(cc--define-bytevector-getter-and-setter u32	cc-uint32	uint32_t)
(cc--define-bytevector-getter-and-setter s32	cc-sint32	int32_t)
(cc--define-bytevector-getter-and-setter u64	cc-uint64	uint64_t)
(cc--define-bytevector-getter-and-setter s64	cc-sint64	int64_t)


;;;; done

(provide 'cc-core)

;;; cc-core.el ends here
