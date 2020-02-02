;;; cc-core.el --- core definitions for C language intefaces

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  1, 2020
;; Time-stamp: <2020-02-02 17:53:59 marco>
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



;;;; C language type wrappers: char

(cl-defstruct (cc-char (:constructor cc--make-char))
  obj)

(cl-defgeneric cc-char (init)
  "Build and return a new instance of `cc-char'.")
(cl-defgeneric cc-char ((init integer))
  "Build and return a new instance of `cc-char'."
  (cl-assert (cc-range-char-p init))
  (cc--make-char :obj init))

(defun cc-range-char-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `char'."
  (and (integerp op)
       (<= cc-CHAR_MIN op cc-CHAR_MAX)))


;;;; C language type wrappers: signed char

(cl-defstruct (cc-schar (:constructor cc--make-schar))
  obj)

(cl-defgeneric cc-schar (init)
  "Build and return a new instance of `cc-schar'.")
(cl-defgeneric cc-schar ((init integer))
  "Build and return a new instance of `cc-schar'."
  (cl-assert (cc-range-schar-p init))
  (cc--make-schar :obj init))

(defun cc-range-schar-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `signed char'."
  (and (integerp op)
       (<= cc-SCHAR_MIN op cc-SCHAR_MAX)))


;;;; C language type wrappers: unsigned char

(cl-defstruct (cc-uchar (:constructor cc--make-uchar))
  obj)

(cl-defgeneric cc-uchar (init)
  "Build and return a new instance of `cc-uchar'.")
(cl-defgeneric cc-uchar ((init integer))
  "Build and return a new instance of `cc-uchar'."
  (cl-assert (cc-range-uchar-p init))
  (cc--make-uchar :obj init))

(defun cc-range-uchar-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `unsigned char'."
  (and (integerp op)
       (<= cc-UCHAR_MIN op cc-UCHAR_MAX)))


;;;; C language type wrappers: wchar

(cl-defstruct (cc-wchar (:constructor cc--make-wchar))
  obj)

(cl-defgeneric cc-wchar (init)
  "Build and return a new instance of `cc-wchar'.")
(cl-defgeneric cc-wchar ((init integer))
  "Build and return a new instance of `cc-wchar'."
  (cl-assert (cc-range-wchar-p init))
  (cc--make-wchar :obj init))

(defun cc-range-wchar-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `wchar_t'."
  (and (integerp op)
       (<= cc-WCHAR_MIN op cc-WCHAR_MAX)))


;;;; C language type wrappers: signed short int

(cl-defstruct (cc-signed-short-int (:constructor cc--make-signed-short-int))
  obj)

(cl-defgeneric cc-signed-short-int (init)
  "Build and return a new instance of `cc-signed-short-int'.")
(cl-defgeneric cc-signed-short-int ((init integer))
  "Build and return a new instance of `cc-signed-short-int'."
  (cl-assert (cc-range-signed-short-int-p init))
  (cc--make-signed-short-int :obj init))

(defun cc-range-signed-short-int-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `signed short int'."
  (and (integerp op)
       (<= cc-SHRT_MIN op cc-SHRT_MAX)))


;;;; C language type wrappers: unsigned short int

(cl-defstruct (cc-unsigned-short-int (:constructor cc--make-unsigned-short-int))
  obj)

(cl-defgeneric cc-unsigned-short-int (init)
  "Build and return a new instance of `cc-unsigned-short-int'.")
(cl-defgeneric cc-unsigned-short-int ((init integer))
  "Build and return a new instance of `cc-unsigned-short-int'."
  (cl-assert (cc-range-unsigned-short-int-p init))
  (cc--make-unsigned-short-int :obj init))

(defun cc-range-unsigned-short-int-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `unsigned short int'."
  (and (integerp op)
       (<= cc-USHRT_MIN op cc-USHRT_MAX)))


;;;; C language type wrappers: signed int

(cl-defstruct (cc-signed-int (:constructor cc--make-signed-int))
  obj)

(cl-defgeneric cc-signed-int (init)
  "Build and return a new instance of `cc-signed-int'.")
(cl-defgeneric cc-signed-int ((init integer))
  "Build and return a new instance of `cc-signed-int'."
  (cl-assert (cc-range-signed-int-p init))
  (cc--make-signed-int :obj init))

(defun cc-range-signed-int-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `signed int'."
  (and (integerp op)
       (<= cc-INT_MIN op cc-INT_MAX)))


;;;; C language type wrappers: unsigned int

(cl-defstruct (cc-unsigned-int (:constructor cc--make-unsigned-int))
  obj)

(cl-defgeneric cc-unsigned-int (init)
  "Build and return a new instance of `cc-unsigned-int'.")
(cl-defgeneric cc-unsigned-int ((init integer))
  "Build and return a new instance of `cc-unsigned-int'."
  (cl-assert (cc-range-unsigned-int-p init))
  (cc--make-unsigned-int :obj init))

(defun cc-range-unsigned-int-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `unsigned int'."
  (and (integerp op)
       (<= cc-UINT_MIN op cc-UINT_MAX)))


;;;; C language type wrappers: signed long int

(cl-defstruct (cc-signed-long-int (:constructor cc--make-signed-long-int))
  obj)

(cl-defgeneric cc-signed-long-int (init)
  "Build and return a new instance of `cc-signed-long-int'.")
(cl-defgeneric cc-signed-long-int ((init integer))
  "Build and return a new instance of `cc-signed-long-int'."
  (cl-assert (cc-range-signed-long-int-p init))
  (cc--make-signed-long-int :obj init))

(defun cc-range-signed-long-int-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `signed long int'."
  (and (integerp op)
       (<= cc-LONG_MIN op cc-LONG_MAX)))


;;;; C language type wrappers: unsigned long int

(cl-defstruct (cc-unsigned-long-int (:constructor cc--make-unsigned-long-int))
  obj)

(cl-defgeneric cc-unsigned-long-int (init)
  "Build and return a new instance of `cc-unsigned-long-int'.")
(cl-defgeneric cc-unsigned-long-int ((init integer))
  "Build and return a new instance of `cc-unsigned-long-int'."
  (cl-assert (cc-range-unsigned-long-int-p init))
  (cc--make-unsigned-long-int :obj init))

(defun cc-range-unsigned-long-int-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `unsigned long int'."
  (and (integerp op)
       (<= cc-ULONG_MIN op cc-ULONG_MAX)))


;;;; C language type wrappers: signed long long int

(cl-defstruct (cc-signed-long-long-int (:constructor cc--make-signed-long-long-int))
  obj)

(cl-defgeneric cc-signed-long-long-int (init)
  "Build and return a new instance of `cc-signed-long-long-int'.")
(cl-defgeneric cc-signed-long-long-int ((init integer))
  "Build and return a new instance of `cc-signed-long-long-int'."
  (cl-assert (cc-range-signed-long-long-int-p init))
  (cc--make-signed-long-long-int :obj init))

(defun cc-range-signed-long-long-int-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `signed long long int'."
  (and (integerp op)
       (<= cc-LLONG_MIN op cc-LLONG_MAX)))


;;;; C language type wrappers: unsigned long long int

(cl-defstruct (cc-unsigned-long-long-int (:constructor cc--make-unsigned-long-long-int))
  obj)

(cl-defgeneric cc-unsigned-long-long-int (init)
  "Build and return a new instance of `cc-unsigned-long-long-int'.")
(cl-defgeneric cc-unsigned-long-long-int ((init integer))
  "Build and return a new instance of `cc-unsigned-long-long-int'."
  (cl-assert (cc-range-unsigned-long-long-int-p init))
  (cc--make-unsigned-long-long-int :obj init))

(defun cc-range-unsigned-long-long-int-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `unsigned long long int'."
  (and (integerp op)
       (<= cc-ULLONG_MIN op cc-ULLONG_MAX)))


;;;; C language type wrappers: size_t

(cl-defstruct (cc-usize (:constructor cc--make-size_t))
  obj)

(cl-defgeneric cc-usize (init)
  "Build and return a new instance of `cc-usize'.")
(cl-defgeneric cc-usize ((init integer))
  "Build and return a new instance of `cc-usize'."
  (cl-assert (cc-range-usize-p init))
  (cc--make-size_t :obj init))

(defun cc-range-usize-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `size_t'."
  (and (integerp op)
       (<= cc-SIZE_T_MIN op cc-SIZE_T_MAX)))


;;;; C language type wrappers: ssize_t

(cl-defstruct (cc-ssize (:constructor cc--make-ssize_t))
  obj)

(cl-defgeneric cc-ssize (init)
  "Build and return a new instance of `cc-ssize'.")
(cl-defgeneric cc-ssize ((init integer))
  "Build and return a new instance of `cc-ssize'."
  (cl-assert (cc-range-ssize-p init))
  (cc--make-ssize_t :obj init))

(defun cc-range-ssize-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `ssize_t'."
  (and (integerp op)
       (<= cc-SSIZE_T_MIN op cc-SSIZE_T_MAX)))


;;;; C language type wrappers: signed intmax

(cl-defstruct (cc-intmax (:constructor cc--make-intmax))
  obj)

(cl-defgeneric cc-intmax (init)
  "Build and return a new instance of `cc-intmax'.")
(cl-defgeneric cc-intmax ((init integer))
  "Build and return a new instance of `cc-intmax'."
  (cl-assert (cc-range-intmax-p init))
  (cc--make-intmax :obj init))

(defun cc-range-intmax-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `intmax_t'."
  (and (integerp op)
       (<= cc-INTMAX_MIN op cc-INTMAX_MAX)))


;;;; C language type wrappers: uintmax

(cl-defstruct (cc-uintmax (:constructor cc--make-uintmax))
  obj)

(cl-defgeneric cc-uintmax (init)
  "Build and return a new instance of `cc-uintmax'.")
(cl-defgeneric cc-uintmax ((init integer))
  "Build and return a new instance of `cc-uintmax'."
  (cl-assert (cc-range-uintmax-p init))
  (cc--make-uintmax :obj init))

(defun cc-range-uintmax-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `uintmax_t'."
  (and (integerp op)
       (<= cc-UINTMAX_MIN op cc-UINTMAX_MAX)))


;;;; C language type wrappers: int8_t, uint8_t

(cl-defstruct (cc-int8 (:constructor cc--make-int8))
  obj)

(cl-defgeneric cc-int8 (init)
  "Build and return a new instance of `cc-int8'.")
(cl-defgeneric cc-int8 ((init integer))
  "Build and return a new instance of `cc-int8'."
  (cl-assert (cc-range-int8-p init))
  (cc--make-int8 :obj init))

(defun cc-range-int8-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `int8_t'."
  (and (integerp op)
       (<= cc-INT8_MIN op cc-INT8_MAX)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-uint8 (:constructor cc--make-uint8))
  obj)

(cl-defgeneric cc-uint8 (init)
  "Build and return a new instance of `cc-uint8'.")
(cl-defgeneric cc-uint8 ((init integer))
  "Build and return a new instance of `cc-uint8'."
  (cl-assert (cc-range-uint8-p init))
  (cc--make-uint8 :obj init))

(defun cc-range-uint8-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `uint8_t'."
  (and (integerp op)
       (<= cc-UINT8_MIN op cc-UINT8_MAX)))


;;;; C language type wrappers: int16_t, uint16_t

(cl-defstruct (cc-int16 (:constructor cc--make-int16))
  obj)

(cl-defgeneric cc-int16 (init)
  "Build and return a new instance of `cc-int16'.")
(cl-defgeneric cc-int16 ((init integer))
  "Build and return a new instance of `cc-int16'."
  (cl-assert (cc-range-int16-p init))
  (cc--make-int16 :obj init))

(defun cc-range-int16-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `int16_t'."
  (and (integerp op)
       (<= cc-INT16_MIN op cc-INT16_MAX)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-uint16 (:constructor cc--make-uint16))
  obj)

(cl-defgeneric cc-uint16 (init)
  "Build and return a new instance of `cc-uint16'.")
(cl-defgeneric cc-uint16 ((init integer))
  "Build and return a new instance of `cc-uint16'."
  (cl-assert (cc-range-uint16-p init))
  (cc--make-uint16 :obj init))

(defun cc-range-uint16-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `uint16_t'."
  (and (integerp op)
       (<= cc-UINT16_MIN op cc-UINT16_MAX)))


;;;; C language type wrappers: int32_t, uint32_t

(cl-defstruct (cc-int32 (:constructor cc--make-int32))
  obj)

(cl-defgeneric cc-int32 (init)
  "Build and return a new instance of `cc-int32'.")
(cl-defgeneric cc-int32 ((init integer))
  "Build and return a new instance of `cc-int32'."
  (cl-assert (cc-range-int32-p init))
  (cc--make-int32 :obj init))

(defun cc-range-int32-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `int32_t'."
  (and (integerp op)
       (<= cc-INT32_MIN op cc-INT32_MAX)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-uint32 (:constructor cc--make-uint32))
  obj)

(cl-defgeneric cc-uint32 (init)
  "Build and return a new instance of `cc-uint32'.")
(cl-defgeneric cc-uint32 ((init integer))
  "Build and return a new instance of `cc-uint32'."
  (cl-assert (cc-range-uint32-p init))
  (cc--make-uint32 :obj init))

(defun cc-range-uint32-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `uint32_t'."
  (and (integerp op)
       (<= cc-UINT32_MIN op cc-UINT32_MAX)))


;;;; C language type wrappers: int64_t, uint64_t

(cl-defstruct (cc-int64 (:constructor cc--make-int64))
  obj)

(cl-defgeneric cc-int64 (init)
  "Build and return a new instance of `cc-int64'.")
(cl-defgeneric cc-int64 ((init integer))
  "Build and return a new instance of `cc-int64'."
  (cl-assert (cc-range-int64-p init))
  (cc--make-int64 :obj init))

(defun cc-range-int64-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `int64_t'."
  (and (integerp op)
       (<= cc-INT64_MIN op cc-INT64_MAX)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-uint64 (:constructor cc--make-uint64))
  obj)

(cl-defgeneric cc-uint64 (init)
  "Build and return a new instance of `cc-uint64'.")
(cl-defgeneric cc-uint64 ((init integer))
  "Build and return a new instance of `cc-uint64'."
  (cl-assert (cc-range-uint64-p init))
  (cc--make-uint64 :obj init))

(defun cc-range-uint64-p (op)
  "Return true if OP is an exact integer object and it is in the range representable by `uint64_t'."
  (and (integerp op)
       (<= cc-UINT64_MIN op cc-UINT64_MAX)))


;;;; basic integer operations

(defmacro cc--define-integer-comparison-method (OPERATION TYPE)
  (let* ((OPERATION.str	(symbol-name OPERATION))
	 (CC-FUNC	(intern (concat "cc" (symbol-name OPERATION))))
	 (GETTER	(intern (concat (symbol-name TYPE) "-obj")))
	 (DOCSTRING	(concat "Return true if OP1 " OPERATION.str " OP2; otherwise return false.")))
    `(progn
       (cl-defmethod ,CC-FUNC ((op1 ,TYPE) (op2 t))
	 ,DOCSTRING
	 (,CC-FUNC (,GETTER op1) op2))
       (cl-defmethod ,CC-FUNC ((op1 t) (op2 ,TYPE))
	 ,DOCSTRING
	 (,CC-FUNC op1 (,GETTER op2)))
       )))

(defmacro cc--define-integer-comparison (OPERATION)
  (let* ((OPERATION.str	(symbol-name OPERATION))
	 (CC-FUNC	(intern (concat "cc" OPERATION.str)))
	 (DOCSTRING	(concat "Return true if OP1 " OPERATION.str " OP2; otherwise return false.")))
    `(progn
       (cl-defgeneric ,CC-FUNC (op1 op2)
    	 ,DOCSTRING)
       (cl-defmethod  ,CC-FUNC ((op1 integer) (op2 integer))
    	 ,DOCSTRING
    	 (,OPERATION op1 op2))
       (cc--define-integer-comparison-method ,OPERATION cc-char)
       (cc--define-integer-comparison-method ,OPERATION cc-schar)
       (cc--define-integer-comparison-method ,OPERATION cc-uchar)
       (cc--define-integer-comparison-method ,OPERATION cc-wchar)
       (cc--define-integer-comparison-method ,OPERATION cc-signed-short-int)
       (cc--define-integer-comparison-method ,OPERATION cc-unsigned-short-int)
       (cc--define-integer-comparison-method ,OPERATION cc-signed-int)
       (cc--define-integer-comparison-method ,OPERATION cc-unsigned-int)
       (cc--define-integer-comparison-method ,OPERATION cc-signed-long-int)
       (cc--define-integer-comparison-method ,OPERATION cc-unsigned-long-int)
       (cc--define-integer-comparison-method ,OPERATION cc-signed-long-int)
       (cc--define-integer-comparison-method ,OPERATION cc-unsigned-long-int)
       (cc--define-integer-comparison-method ,OPERATION cc-signed-long-long-int)
       (cc--define-integer-comparison-method ,OPERATION cc-unsigned-long-long-int)
       (cc--define-integer-comparison-method ,OPERATION cc-ssize)
       (cc--define-integer-comparison-method ,OPERATION cc-usize)
       (cc--define-integer-comparison-method ,OPERATION cc-intmax)
       (cc--define-integer-comparison-method ,OPERATION cc-uintmax)
       (cc--define-integer-comparison-method ,OPERATION cc-int8)
       (cc--define-integer-comparison-method ,OPERATION cc-uint8)
       (cc--define-integer-comparison-method ,OPERATION cc-int16)
       (cc--define-integer-comparison-method ,OPERATION cc-uint16)
       (cc--define-integer-comparison-method ,OPERATION cc-int32)
       (cc--define-integer-comparison-method ,OPERATION cc-uint32)
       (cc--define-integer-comparison-method ,OPERATION cc-int64)
       (cc--define-integer-comparison-method ,OPERATION cc-uint64)
       )))

(cc--define-integer-comparison =)
(cc--define-integer-comparison <)
(cc--define-integer-comparison >)
(cc--define-integer-comparison <=)
(cc--define-integer-comparison >=)
(cc--define-integer-comparison /=)


;;;; bytevector objects: object definitions
;;
;; mmux-core-c-bytevector-make NUMBER-OF-SLOTS SLOT-SIZE SIGNED
;;
;;Defined a the C language level.  Build and return a new custom pointer object.
;;
;;The argument NUMBER-OF-SLOTS must be a non-negative exact integer representing the number of slots
;;in the bytevector.
;;
;;The argument  SLOT-SIZE must be a  non-negative exact integer  representing the size of  each slot
;;measured in bytes; valid values are: 1, 2, 4, 8.
;;
;;The argument SIGNED must be 1 or 0: if the value is 1, the bytevector holds signed integers in its
;;slots; if the value is 0, the bytevector holds unsigned integers in its slots.
;;

(cl-defstruct cc-bytevector
  number-of-slots
  slot-size
  number-of-allocated-bytes
  signed
  obj)

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-u8 (:constructor	cc--make-bytevector-u8)
				(:include	cc-bytevector)))

(cl-defgeneric cc-bytevector-u8 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-u8'.")
(cl-defgeneric cc-bytevector-u8 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-u8'."
  (cl-assert (<= 0 number-of-slots))
  (cc--make-bytevector-u8
   :number-of-slots		number-of-slots
   :slot-size			1
   :signed			nil
   :obj				(mmux-core-c-bytevector-make number-of-slots 1 0)
   :number-of-allocated-bytes	(* number-of-slots 1)))

(cl-defstruct (cc-bytevector-s8 (:constructor	cc--make-bytevector-s8)
				(:include	cc-bytevector)))

(cl-defgeneric cc-bytevector-s8 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-s8'.")
(cl-defgeneric cc-bytevector-s8 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-s8'."
  (cl-assert (<= 0 number-of-slots))
  (cc--make-bytevector-s8
   :number-of-slots		number-of-slots
   :slot-size			1
   :signed			t
   :obj				(mmux-core-c-bytevector-make number-of-slots 1 1)
   :number-of-allocated-bytes	(* number-of-slots 1)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-u16 (:constructor	cc--make-bytevector-u16)
				 (:include	cc-bytevector)))

(cl-defgeneric cc-bytevector-u16 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-u16'.")
(cl-defgeneric cc-bytevector-u16 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-u16'."
  (cl-assert (<= 0 number-of-slots))
  (cc--make-bytevector-u16
   :number-of-slots		number-of-slots
   :slot-size			2
   :signed			nil
   :obj				(mmux-core-c-bytevector-make number-of-slots 2 0)
   :number-of-allocated-bytes	(* number-of-slots 2)))

(cl-defstruct (cc-bytevector-s16 (:constructor	cc--make-bytevector-s16)
				 (:include	cc-bytevector)))

(cl-defgeneric cc-bytevector-s16 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-s16'.")
(cl-defgeneric cc-bytevector-s16 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-s16'."
  (cl-assert (<= 0 number-of-slots))
  (cc--make-bytevector-s16
   :number-of-slots		number-of-slots
   :slot-size			2
   :signed			t
   :obj				(mmux-core-c-bytevector-make number-of-slots 2 1)
   :number-of-allocated-bytes	(* number-of-slots 2)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-u32 (:constructor	cc--make-bytevector-u32)
				 (:include	cc-bytevector)))

(cl-defgeneric cc-bytevector-u32 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-u32'.")
(cl-defgeneric cc-bytevector-u32 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-u32'."
  (cl-assert (<= 0 number-of-slots))
  (cc--make-bytevector-u32
   :number-of-slots		number-of-slots
   :slot-size			4
   :signed			nil
   :obj				(mmux-core-c-bytevector-make number-of-slots 4 0)
   :number-of-allocated-bytes	(* number-of-slots 4)))

(cl-defstruct (cc-bytevector-s32 (:constructor	cc--make-bytevector-s32)
				 (:include	cc-bytevector)))

(cl-defgeneric cc-bytevector-s32 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-s32'.")
(cl-defgeneric cc-bytevector-s32 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-s32'."
  (cl-assert (<= 0 number-of-slots))
  (cc--make-bytevector-s32
   :number-of-slots		number-of-slots
   :slot-size			4
   :signed			t
   :obj				(mmux-core-c-bytevector-make number-of-slots 4 1)
   :number-of-allocated-bytes	(* number-of-slots 4)))

;;; --------------------------------------------------------------------

(cl-defstruct (cc-bytevector-u64 (:constructor	cc--make-bytevector-u64)
				 (:include	cc-bytevector)))

(cl-defgeneric cc-bytevector-u64 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-u64'.")
(cl-defgeneric cc-bytevector-u64 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-u64'."
  (cl-assert (<= 0 number-of-slots))
  (cc--make-bytevector-u64
   :number-of-slots		number-of-slots
   :slot-size			8
   :signed			nil
   :obj				(mmux-core-c-bytevector-make number-of-slots 8 0)
   :number-of-allocated-bytes	(* number-of-slots 8)))

(cl-defstruct (cc-bytevector-s64 (:constructor	cc--make-bytevector-s64)
				 (:include	cc-bytevector)))

(cl-defgeneric cc-bytevector-s64 (number-of-slots)
  "Build and return a new instance of `cc-bytevector-s64'.")
(cl-defgeneric cc-bytevector-s64 ((number-of-slots integer))
  "Build and return a new instance of `cc-bytevector-s64'."
  (cl-assert (<= 0 number-of-slots))
  (cc--make-bytevector-s64
   :number-of-slots		number-of-slots
   :slot-size			8
   :signed			t
   :obj				(mmux-core-c-bytevector-make number-of-slots 8 1)
   :number-of-allocated-bytes	(* number-of-slots 8)))


;;;; bytevector objects: inspection

(cl-defgeneric cc-bytevector-length (bv)
  "Return an exact integer representing the length of a `cc-bytevector' object measured in bytes.")

(cl-defgeneric cc-bytevector-length ((bv cc-bytevector))
  "Return an exact integer representing the length of a `cc-bytevector' object measured in bytes."
  (* (cc-bytevector-number-of-slots bv)
     (cc-bytevector-slot-size bv)))


;;;; bytevector objects: setters and getters

(cl-defgeneric cc-bytevector-ref (bv idx)
  "Extract a value from a bytevector.")

(cl-defmethod cc-bytevector-ref ((bv cc-bytevector) (idx integer))
  "Extract a value from a bytevector."
  (mmux-core-c-bytevector-u8-ref (cc-bytevector-obj bv) idx))

;;; --------------------------------------------------------------------

(cl-defgeneric cc-bytevector-set! (bv idx val)
  "Extract a value from a bytevector.")

(cl-defmethod cc-bytevector-set! ((bv cc-bytevector) (idx integer) (val integer))
  "Extract a value from a bytevector."
  (mmux-core-c-bytevector-u8-set! (cc-bytevector-obj bv) idx val))


;;;; done

(provide 'cc-core)

;;; cc-core.el ends here
