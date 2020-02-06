;;; cc-bytevector-objects.el --- bytevector objects

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-06 07:02:43 marco>
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
  (require 'cc-constants)
  (require 'cc-basics)
  (require 'cc-numeric-type-definitions)
  (require 'cc-numeric-comparison-operations))
(require 'cc-constants)
(require 'cc-basics)
(require 'cc-numeric-type-definitions)
(require 'cc-numeric-comparison-operations)


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

(provide 'cc-bytevector-objects)

;;; end of file
