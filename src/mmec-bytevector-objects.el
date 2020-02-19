;;; mmec-bytevector-objects.el --- bytevector objects

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-18 18:24:20 marco>
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

(require 'mmec-constants)
(require 'mmec-basics)
(require 'mmec-number-objects)


;;;; bytevector objects: object definitions
;;
;; mmec-c-make-bytevector NUMBER-OF-SLOTS SLOT-SIZE SIGNED
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

(cl-defstruct (mmec-bytevector
	       (:constructor mmec-bytevector--make))
  number-of-slots
  slot-size
  number-of-allocated-bytes
  obj)

(cl-defstruct (mmec-integer-bytevector
	       (:include	mmec-bytevector)
	       (:constructor	mmec-integer-bytevector--make))
  signed)

(cl-defstruct (mmec-signed-integer-bytevector
	       (:include	mmec-integer-bytevector)
	       (:constructor	mmec-signed-integer-bytevector--make)))

(cl-defstruct (mmec-unsigned-integer-bytevector
	       (:include	mmec-integer-bytevector)
	       (:constructor	mmec-unsigned-integer-bytevector--make)))

(cl-defstruct (mmec-floating-point-bytevector
	       (:include	mmec-bytevector)
	       (:constructor	mmec-floating-point-bytevector--make)))

(cl-macrolet ((mmec--define-abstract-type-constructor
	       (TYPE)
	       `(defun ,TYPE (&rest args)
		  (signal 'mmux-core-instantiating-abstract-type (quote ,TYPE)))))
  (mmec--define-abstract-type-constructor mmec-bytevector)
  (mmec--define-abstract-type-constructor mmec-integer-bytevector)
  (mmec--define-abstract-type-constructor mmec-signed-integer-bytevector)
  (mmec--define-abstract-type-constructor mmec-unsigned-integer-bytevector)
  (mmec--define-abstract-type-constructor mmec-floating-point-bytevector))


;;;; bytevector objects: slots of type char, schar, uchar

(cl-defstruct (mmec-char-bytevector
	       (:constructor	mmec-char-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-char-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-char-bytevector'.")
(cl-defmethod  mmec-char-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-char-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-char-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_CHAR
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_CHAR t)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_CHAR)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-schar-bytevector
	       (:constructor	mmec-schar-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-schar-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-schar-bytevector'.")
(cl-defmethod  mmec-schar-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-schar-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-schar-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_SIGNED_CHAR
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_SIGNED_CHAR 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_SIGNED_CHAR)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-uchar-bytevector
	       (:constructor	mmec-uchar-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-uchar-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-uchar-bytevector'.")
(cl-defmethod  mmec-uchar-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-uchar-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-uchar-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_UNSIGNED_CHAR
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_UNSIGNED_CHAR 0)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_UNSIGNED_CHAR)))


;;;; bytevector objects: slots of type sshrt and ushrt

(cl-defstruct (mmec-sshrt-bytevector
	       (:constructor	mmec-sshrt-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-sshrt-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-sshrt-bytevector'.")
(cl-defmethod  mmec-sshrt-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-sshrt-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-sshrt-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_SIGNED_SHORT_INT
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_SIGNED_SHORT_INT 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_SIGNED_SHORT_INT)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-ushrt-bytevector
	       (:constructor	mmec-ushrt-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-ushrt-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-ushrt-bytevector'.")
(cl-defmethod  mmec-ushrt-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-ushrt-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-ushrt-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_UNSIGNED_SHORT_INT
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_UNSIGNED_SHORT_INT 0)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_UNSIGNED_SHORT_INT)))


;;;; bytevector objects: slots of type sint and uint

(cl-defstruct (mmec-sint-bytevector
	       (:constructor	mmec-sint-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-sint-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-sint-bytevector'.")
(cl-defmethod  mmec-sint-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-sint-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-sint-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_SIGNED_INT
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_SIGNED_INT 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_SIGNED_INT)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-uint-bytevector
	       (:constructor	mmec-uint-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-uint-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-uint-bytevector'.")
(cl-defmethod  mmec-uint-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-uint-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-uint-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_UNSIGNED_INT
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_UNSIGNED_INT 0)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_UNSIGNED_INT)))


;;;; bytevector objects: slots of type slong and ulong

(cl-defstruct (mmec-slong-bytevector
	       (:constructor	mmec-slong-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-slong-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-slong-bytevector'.")
(cl-defmethod  mmec-slong-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-slong-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-slong-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_SIGNED_LONG_INT
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_SIGNED_LONG_INT 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_SIGNED_LONG_INT)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-ulong-bytevector
	       (:constructor	mmec-ulong-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-ulong-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-ulong-bytevector'.")
(cl-defmethod  mmec-ulong-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-ulong-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-ulong-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_UNSIGNED_LONG_INT
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_UNSIGNED_LONG_INT 0)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_UNSIGNED_LONG_INT)))


;;;; bytevector objects: slots of type sllong and ullong

(cl-defstruct (mmec-sllong-bytevector
	       (:constructor	mmec-sllong-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-sllong-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-sllong-bytevector'.")
(cl-defmethod  mmec-sllong-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-sllong-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-sllong-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_SIGNED_LONG_LONG_INT
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_SIGNED_LONG_LONG_INT 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_SIGNED_LONG_LONG_INT)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-ullong-bytevector
	       (:constructor	mmec-ullong-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-ullong-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-ullong-bytevector'.")
(cl-defmethod  mmec-ullong-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-ullong-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-ullong-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_UNSIGNED_LONG_LONG_INT
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_UNSIGNED_LONG_LONG_INT 0)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_UNSIGNED_LONG_LONG_INT)))


;;;; bytevector objects: slots of type sintmax and uintmax

(cl-defstruct (mmec-sintmax-bytevector
	       (:constructor	mmec-sintmax-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-sintmax-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-sintmax-bytevector'.")
(cl-defmethod  mmec-sintmax-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-sintmax-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-sintmax-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_INTMAX_T
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_INTMAX_T 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_INTMAX_T)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-uintmax-bytevector
	       (:constructor	mmec-uintmax-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-uintmax-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-uintmax-bytevector'.")
(cl-defmethod  mmec-uintmax-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-uintmax-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-uintmax-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_UINTMAX_T
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_UINTMAX_T 0)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_UINTMAX_T)))


;;;; bytevector objects: slots of type ssize and usize

(cl-defstruct (mmec-ssize-bytevector
	       (:constructor	mmec-ssize-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-ssize-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-ssize-bytevector'.")
(cl-defmethod  mmec-ssize-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-ssize-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-ssize-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_SSIZE_T
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_SSIZE_T 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_SSIZE_T)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-usize-bytevector
	       (:constructor	mmec-usize-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-usize-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-usize-bytevector'.")
(cl-defmethod  mmec-usize-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-usize-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-usize-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_SIZE_T
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_SIZE_T 0)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_SIZE_T)))


;;;; bytevector objects: slots of type ptrdiff

(cl-defstruct (mmec-ptrdiff-bytevector
	       (:constructor	mmec-ptrdiff-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-ptrdiff-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-ptrdiff-bytevector'.")
(cl-defmethod  mmec-ptrdiff-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-ptrdiff-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-ptrdiff-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_PTRDIFF_T
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_PTRDIFF_T 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_PTRDIFF_T)))


;;;; bytevector objects: slots of type wchar

(cl-defstruct (mmec-wchar-bytevector
	       (:constructor	mmec-wchar-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-wchar-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-wchar-bytevector'.")
(cl-defmethod  mmec-wchar-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-wchar-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-wchar-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_WCHAR_T
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_WCHAR_T 0)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_WCHAR_T)))


;;;; bytevector objects: slots of type sint8 and uint8

(cl-defstruct (mmec-uint8-bytevector
	       (:constructor	mmec-uint8-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-uint8-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-uint8-bytevector'.")
(cl-defmethod  mmec-uint8-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-uint8-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-uint8-bytevector
   :number-of-slots		number-of-slots
   :slot-size			1
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots 1 0)
   :number-of-allocated-bytes	(* number-of-slots 1)))

(cl-defstruct (mmec-sint8-bytevector
	       (:constructor	mmec-sint8-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-sint8-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-sint8-bytevector'.")
(cl-defmethod  mmec-sint8-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-sint8-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-sint8-bytevector
   :number-of-slots		number-of-slots
   :slot-size			1
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots 1 1)
   :number-of-allocated-bytes	(* number-of-slots 1)))


;;;; bytevector objects: slots of type sint16 and uint16

(cl-defstruct (mmec-uint16-bytevector
	       (:constructor	mmec-uint16-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-uint16-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-uint16-bytevector'.")
(cl-defmethod  mmec-uint16-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-uint16-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-uint16-bytevector
   :number-of-slots		number-of-slots
   :slot-size			2
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots 2 0)
   :number-of-allocated-bytes	(* number-of-slots 2)))

(cl-defstruct (mmec-sint16-bytevector
	       (:constructor	mmec-sint16-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-sint16-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-sint16-bytevector'.")
(cl-defmethod  mmec-sint16-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-sint16-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-sint16-bytevector
   :number-of-slots		number-of-slots
   :slot-size			2
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots 2 1)
   :number-of-allocated-bytes	(* number-of-slots 2)))


;;;; bytevector objects: slots of type sint32 and uint32

(cl-defstruct (mmec-uint32-bytevector
	       (:constructor	mmec-uint32-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-uint32-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-uint32-bytevector'.")
(cl-defmethod  mmec-uint32-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-uint32-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-uint32-bytevector
   :number-of-slots		number-of-slots
   :slot-size			4
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots 4 0)
   :number-of-allocated-bytes	(* number-of-slots 4)))

(cl-defstruct (mmec-sint32-bytevector
	       (:constructor	mmec-sint32-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-sint32-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-sint32-bytevector'.")
(cl-defmethod  mmec-sint32-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-sint32-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-sint32-bytevector
   :number-of-slots		number-of-slots
   :slot-size			4
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots 4 1)
   :number-of-allocated-bytes	(* number-of-slots 4)))


;;;; bytevector objects: slots of type sint64 and uint64

(cl-defstruct (mmec-uint64-bytevector
	       (:constructor	mmec-uint64-bytevector--make)
	       (:include	mmec-unsigned-integer-bytevector)))

(cl-defgeneric mmec-uint64-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-uint64-bytevector'.")
(cl-defmethod  mmec-uint64-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-uint64-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-uint64-bytevector
   :number-of-slots		number-of-slots
   :slot-size			8
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots 8 0)
   :number-of-allocated-bytes	(* number-of-slots 8)))

(cl-defstruct (mmec-sint64-bytevector
	       (:constructor	mmec-sint64-bytevector--make)
	       (:include	mmec-signed-integer-bytevector)))

(cl-defgeneric mmec-sint64-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-sint64-bytevector'.")
(cl-defmethod  mmec-sint64-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-sint64-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-sint64-bytevector
   :number-of-slots		number-of-slots
   :slot-size			8
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots 8 1)
   :number-of-allocated-bytes	(* number-of-slots 8)))


;;;; bytevector objects: slots of type float

(cl-defstruct (mmec-float-bytevector
	       (:constructor	mmec-float-bytevector--make)
	       (:include	mmec-floating-point-bytevector)))

(cl-defgeneric mmec-float-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-float-bytevector'.")
(cl-defmethod  mmec-float-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-float-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-float-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_FLOAT
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_FLOAT 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_FLOAT)))


;;;; bytevector objects: slots of type double

(cl-defstruct (mmec-double-bytevector
	       (:constructor	mmec-double-bytevector--make)
	       (:include	mmec-floating-point-bytevector)))

(cl-defgeneric mmec-double-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-double-bytevector'.")
(cl-defmethod  mmec-double-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-double-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-double-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_DOUBLE
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_DOUBLE 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_DOUBLE)))


;;;; bytevector objects: slots of type ldouble

(cl-defstruct (mmec-ldouble-bytevector
	       (:constructor	mmec-ldouble-bytevector--make)
	       (:include	mmec-floating-point-bytevector)))

(cl-defgeneric mmec-ldouble-bytevector (number-of-slots)
  "Build and return a new instance of `mmec-ldouble-bytevector'.")
(cl-defmethod  mmec-ldouble-bytevector ((number-of-slots integer))
  "Build and return a new instance of `mmec-ldouble-bytevector'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-ldouble-bytevector
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_LONG_DOUBLE
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_LONG_DOUBLE 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_LONG_DOUBLE)))


;;;; bytevector objects: getters and setters

(cl-defgeneric mmec-bytevector-ref (bv idx)
  "Extract a value from a bytevector.

The  argument  BV   must  be  a  value  whose  type   is  a  subtype  of
`mmec-bytevector'.

The argumet IDX must be a value of type `integer' representing the index
of a slot into the bytevector BV.")

(cl-defgeneric mmec-bytevector-set (bv idx val)
  "Store the value VAL into the bytevector BV at index IDX.

The  argument  BV   must  be  a  value  whose  type   is  a  subtype  of
`mmec-bytevector'.

The argumet IDX must be a value of type `integer' representing the index
of a slot into the bytevector BV.

The argument  VAL must be  a numeric value  compatible with the  type of
slots in the bytevector BV.")

(defmacro mmec--define-bytevector-getter (TYPESTEM CTYPE)
  (let* ((TYPESTEM.str		(symbol-name TYPESTEM))
	 (NUMTYPE.str		(concat "mmec-" TYPESTEM.str))
	 (NUMTYPE		(intern NUMTYPE.str))
	 (BYTEVECTOR-TYPE	(intern (concat "mmec-" TYPESTEM.str "-bytevector")))
	 (DOCSTRING		(concat "Extract a value of type `" CTYPE "' from the bytevector BV at index IDX."))
	 (C-FUNC		(intern (concat "mmec-c-bytevector-" TYPESTEM.str "-ref"))))
    `(cl-defmethod mmec-bytevector-ref ((bv ,BYTEVECTOR-TYPE) (idx integer))
       ,DOCSTRING
       (cl-assert (mmec-fits-usize-p idx))
       (mmec--make ,NUMTYPE :obj (,C-FUNC (mmec-bytevector-obj bv) idx)))))

(defmacro mmec--define-bytevector-setter (TYPESTEM CTYPE)
  (let* ((TYPESTEM.str		(symbol-name TYPESTEM))
	 (NUMTYPE.str		(concat "mmec-" TYPESTEM.str))
	 (NUMTYPE		(intern NUMTYPE.str))
	 (BYTEVECTOR-TYPE	(intern (concat "mmec-" TYPESTEM.str "-bytevector")))
	 (DOCSTRING		(concat "Store a value VAL of type `" CTYPE "' into the bytevector BV at index IDX."))
	 (C-FUNC		(intern (concat "mmec-c-bytevector-" TYPESTEM.str "-set"))))
    `(cl-defmethod mmec-bytevector-set ((bv ,BYTEVECTOR-TYPE) (idx integer) (val ,NUMTYPE))
       ,DOCSTRING
       (cl-assert (mmec-fits-usize-p idx))
       (,C-FUNC (mmec-bytevector-obj bv) idx (mmec--extract-obj ,NUMTYPE val)))))

(defmacro mmec--define-bytevector-getter-and-setter (TYPESTEM CTYPE)
  `(progn
     (mmec--define-bytevector-getter ,TYPESTEM ,CTYPE)
     (mmec--define-bytevector-setter ,TYPESTEM ,CTYPE)))

(mmec--define-bytevector-getter-and-setter char		"char")
(mmec--define-bytevector-getter-and-setter schar	"signed char")
(mmec--define-bytevector-getter-and-setter uchar	"unsigned char")
(mmec--define-bytevector-getter-and-setter wchar	"wchar_t")
(mmec--define-bytevector-getter-and-setter sshrt	"signed shrt int")
(mmec--define-bytevector-getter-and-setter ushrt	"unsigned shrt int")
(mmec--define-bytevector-getter-and-setter sint		"signed int")
(mmec--define-bytevector-getter-and-setter uint		"unsigned int")
(mmec--define-bytevector-getter-and-setter slong	"signed long int")
(mmec--define-bytevector-getter-and-setter ulong	"unsigned long int")
(mmec--define-bytevector-getter-and-setter sllong	"signed long long int")
(mmec--define-bytevector-getter-and-setter ullong	"unsigned long long int")
(mmec--define-bytevector-getter-and-setter sintmax	"intmax_t")
(mmec--define-bytevector-getter-and-setter uintmax	"uintmax_t")
(mmec--define-bytevector-getter-and-setter ssize	"ssize_t")
(mmec--define-bytevector-getter-and-setter usize	"size_t")
(mmec--define-bytevector-getter-and-setter ptrdiff	"ptrdiff_t")
(mmec--define-bytevector-getter-and-setter sint8	"int8_t")
(mmec--define-bytevector-getter-and-setter uint8	"uint8_t")
(mmec--define-bytevector-getter-and-setter sint16	"int16_t")
(mmec--define-bytevector-getter-and-setter uint16	"uint16_t")
(mmec--define-bytevector-getter-and-setter sint32	"int32_t")
(mmec--define-bytevector-getter-and-setter uint32	"uint32_t")
(mmec--define-bytevector-getter-and-setter sint64	"int64_t")
(mmec--define-bytevector-getter-and-setter uint64	"uint64_t")
(mmec--define-bytevector-getter-and-setter float	"float")
(mmec--define-bytevector-getter-and-setter double	"double")
(mmec--define-bytevector-getter-and-setter ldouble	"ldouble")


;;;; done

(provide 'mmec-bytevector-objects)

;;; end of file
