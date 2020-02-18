;;; mmec-bytevector-objects.el --- bytevector objects

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-18 07:47:23 marco>
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

(cl-defstruct (mmec-float-bytevector
	       (:include	mmec-bytevector)
	       (:constructor	mmec-float-bytevector--make)))

(cl-macrolet ((mmec--define-abstract-type-constructor
	       (TYPE)
	       `(defun ,TYPE (&rest args)
		  (signal 'mmux-core-instantiating-abstract-type (quote ,TYPE)))))
  (mmec--define-abstract-type-constructor mmec-bytevector)
  (mmec--define-abstract-type-constructor mmec-integer-bytevector)
  (mmec--define-abstract-type-constructor mmec-float-bytevector))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-bytevector-u8
	       (:constructor	mmec-bytevector-u8--make)
	       (:include	mmec-integer-bytevector)))

(cl-defgeneric mmec-bytevector-u8 (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-u8'.")
(cl-defmethod  mmec-bytevector-u8 ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-u8'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-u8
   :number-of-slots		number-of-slots
   :slot-size			1
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots 1 0)
   :number-of-allocated-bytes	(* number-of-slots 1)))

(cl-defstruct (mmec-bytevector-s8
	       (:constructor	mmec-bytevector-s8--make)
	       (:include	mmec-integer-bytevector)))

(cl-defgeneric mmec-bytevector-s8 (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-s8'.")
(cl-defmethod  mmec-bytevector-s8 ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-s8'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-s8
   :number-of-slots		number-of-slots
   :slot-size			1
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots 1 1)
   :number-of-allocated-bytes	(* number-of-slots 1)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-bytevector-u16
	       (:constructor	mmec-bytevector-u16--make)
	       (:include	mmec-integer-bytevector)))

(cl-defgeneric mmec-bytevector-u16 (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-u16'.")
(cl-defmethod  mmec-bytevector-u16 ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-u16'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-u16
   :number-of-slots		number-of-slots
   :slot-size			2
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots 2 0)
   :number-of-allocated-bytes	(* number-of-slots 2)))

(cl-defstruct (mmec-bytevector-s16
	       (:constructor	mmec-bytevector-s16--make)
	       (:include	mmec-integer-bytevector)))

(cl-defgeneric mmec-bytevector-s16 (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-s16'.")
(cl-defmethod  mmec-bytevector-s16 ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-s16'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-s16
   :number-of-slots		number-of-slots
   :slot-size			2
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots 2 1)
   :number-of-allocated-bytes	(* number-of-slots 2)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-bytevector-u32
	       (:constructor	mmec-bytevector-u32--make)
	       (:include	mmec-integer-bytevector)))

(cl-defgeneric mmec-bytevector-u32 (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-u32'.")
(cl-defmethod  mmec-bytevector-u32 ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-u32'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-u32
   :number-of-slots		number-of-slots
   :slot-size			4
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots 4 0)
   :number-of-allocated-bytes	(* number-of-slots 4)))

(cl-defstruct (mmec-bytevector-s32
	       (:constructor	mmec-bytevector-s32--make)
	       (:include	mmec-integer-bytevector)))

(cl-defgeneric mmec-bytevector-s32 (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-s32'.")
(cl-defmethod  mmec-bytevector-s32 ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-s32'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-s32
   :number-of-slots		number-of-slots
   :slot-size			4
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots 4 1)
   :number-of-allocated-bytes	(* number-of-slots 4)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-bytevector-u64
	       (:constructor	mmec-bytevector-u64--make)
	       (:include	mmec-integer-bytevector)))

(cl-defgeneric mmec-bytevector-u64 (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-u64'.")
(cl-defmethod  mmec-bytevector-u64 ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-u64'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-u64
   :number-of-slots		number-of-slots
   :slot-size			8
   :signed			nil
   :obj				(mmec-c-make-bytevector number-of-slots 8 0)
   :number-of-allocated-bytes	(* number-of-slots 8)))

(cl-defstruct (mmec-bytevector-s64
	       (:constructor	mmec-bytevector-s64--make)
	       (:include	mmec-integer-bytevector)))

(cl-defgeneric mmec-bytevector-s64 (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-s64'.")
(cl-defmethod  mmec-bytevector-s64 ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-s64'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-s64
   :number-of-slots		number-of-slots
   :slot-size			8
   :signed			t
   :obj				(mmec-c-make-bytevector number-of-slots 8 1)
   :number-of-allocated-bytes	(* number-of-slots 8)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-bytevector-float
	       (:constructor	mmec-bytevector-float--make)
	       (:include	mmec-float-bytevector)))

(cl-defgeneric mmec-bytevector-float (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-float'.")
(cl-defmethod  mmec-bytevector-float ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-float'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-float
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_FLOAT
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_FLOAT 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_FLOAT)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-bytevector-double
	       (:constructor	mmec-bytevector-double--make)
	       (:include	mmec-float-bytevector)))

(cl-defgeneric mmec-bytevector-double (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-double'.")
(cl-defmethod  mmec-bytevector-double ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-double'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-double
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_DOUBLE
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_DOUBLE 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_DOUBLE)))

;;; --------------------------------------------------------------------

(cl-defstruct (mmec-bytevector-ldouble
	       (:constructor	mmec-bytevector-ldouble--make)
	       (:include	mmec-float-bytevector)))

(cl-defgeneric mmec-bytevector-ldouble (number-of-slots)
  "Build and return a new instance of `mmec-bytevector-ldouble'.")
(cl-defmethod  mmec-bytevector-ldouble ((number-of-slots integer))
  "Build and return a new instance of `mmec-bytevector-ldouble'."
  (cl-assert (<= 0 number-of-slots))
  (mmec--make mmec-bytevector-ldouble
   :number-of-slots		number-of-slots
   :slot-size			mmec-SIZEOF_LDOUBLE
   :obj				(mmec-c-make-bytevector number-of-slots mmec-SIZEOF_LDOUBLE 1)
   :number-of-allocated-bytes	(* number-of-slots mmec-SIZEOF_LDOUBLE)))


;;;; bytevector objects: getters and setters

(cl-defgeneric mmec-bytevector-ref (bv idx)
  "Extract a value from a bytevector.

The  argument BV  must be  a  value whose  type is  a subtype  of
`mmec-bytevector'.

The argumet  IDX must be  a value of type  `integer' representing
the index of a slot into the bytevector BV.")

(cl-defgeneric mmec-bytevector-set (bv idx val)
  "Store the value VAL into the bytevector BV at index IDX.

The  argument BV  must be  a  value whose  type is  a subtype  of
`mmec-bytevector'.

The argumet  IDX must be  a value of type  `integer' representing
the index of a slot into the bytevector BV.

The argument VAL must be a numeric value compatible with the type
of slots in the bytevector BV.")

(defmacro mmec--define-bytevector-getter (TYPESTEM LTYPE CTYPE)
  (let* ((TYPESTEM.str		(symbol-name TYPESTEM))
	 (LTYPE.str		(symbol-name LTYPE))
	 (BYTEVECTOR-TYPE	(intern (concat "mmec-bytevector-" TYPESTEM.str)))
	 (DOCSTRING		(concat "Extract a value of type `" (symbol-name CTYPE) "' from the bytevector BV at index IDX."))
	 (C-FUNC		(intern (concat "mmec-c-bytevector-" TYPESTEM.str "-ref"))))
    `(cl-defmethod mmec-bytevector-ref ((bv ,BYTEVECTOR-TYPE) (idx integer))
       ,DOCSTRING
       (cl-assert (mmec-fits-usize-p idx))
       (mmec--make ,LTYPE :obj (,C-FUNC (mmec-bytevector-obj bv) idx)))))

(defmacro mmec--define-bytevector-setter (TYPESTEM LTYPE CTYPE)
  (let* ((TYPESTEM.str		(symbol-name TYPESTEM))
	 (LTYPE.str		(symbol-name LTYPE))
	 (BYTEVECTOR-TYPE	(intern (concat "mmec-bytevector-" TYPESTEM.str)))
	 (DOCSTRING		(concat "Store a value VAL of type `" (symbol-name CTYPE) "' into the bytevector BV at index IDX."))
	 (LTYPE-OBJ		(intern (concat LTYPE.str "-obj")))
	 (C-FUNC		(intern (concat "mmec-c-bytevector-" TYPESTEM.str "-set"))))
    `(cl-defmethod mmec-bytevector-set ((bv ,BYTEVECTOR-TYPE) (idx integer) (val ,LTYPE))
       ,DOCSTRING
       (cl-assert (mmec-fits-usize-p idx))
       (,C-FUNC (mmec-bytevector-obj bv) idx (,LTYPE-OBJ val)))))

(defmacro mmec--define-bytevector-getter-and-setter (TYPESTEM LTYPE CTYPE)
  `(progn
     (mmec--define-bytevector-getter ,TYPESTEM ,LTYPE ,CTYPE)
     (mmec--define-bytevector-setter ,TYPESTEM ,LTYPE ,CTYPE)))

(mmec--define-bytevector-getter-and-setter u8	mmec-uint8	uint8_t)
(mmec--define-bytevector-getter-and-setter s8	mmec-sint8	int8_t)
(mmec--define-bytevector-getter-and-setter u16	mmec-uint16	uint16_t)
(mmec--define-bytevector-getter-and-setter s16	mmec-sint16	int16_t)
(mmec--define-bytevector-getter-and-setter u32	mmec-uint32	uint32_t)
(mmec--define-bytevector-getter-and-setter s32	mmec-sint32	int32_t)
(mmec--define-bytevector-getter-and-setter u64	mmec-uint64	uint64_t)
(mmec--define-bytevector-getter-and-setter s64	mmec-sint64	int64_t)


;;;; done

(provide 'mmec-bytevector-objects)

;;; end of file
