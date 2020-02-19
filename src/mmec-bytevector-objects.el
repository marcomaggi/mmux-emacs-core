;;; mmec-bytevector-objects.el --- bytevector objects

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-19 11:36:13 marco>
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
  signed
  obj)

(cl-defstruct (mmec-integer-bytevector
	       (:include	mmec-bytevector)
	       (:constructor	mmec-integer-bytevector--make)))

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


;;;; bytevector type definitions

(cl-macrolet
    ((mmec--define-bytevector-type
      (TYPESTEM PARENT-STEM)
      (let* ((BYTEVECTOR-TYPE		(intern (format "mmec-%s-bytevector" TYPESTEM)))
	     (BYTEVECTOR-TYPE-MAKER	(intern (format "mmec-%s-bytevector--make" TYPESTEM)))
	     (PARENT-BYTEVECTOR-TYPE	(intern (format "mmec-%s-bytevector" PARENT-STEM)))
	     (SIZEOF-SLOT		(intern (format "mmec-sizeof-%s" TYPESTEM)))
	     (SIGNED-BOOL		(cl-case PARENT-STEM
					  (signed-integer	't)
					  (unsigned-integer	'nil)
					  (floating-point	't)
					  (t
					   (signal 'mmec-error (list 'mmec--define-bytevector-type PARENT-STEM)))))
	     (DOCSTRING			(format "Build and return a new instance of `mmec-%s-bytevector'." TYPESTEM)))
	`(progn
	   (cl-defstruct (,BYTEVECTOR-TYPE
			  (:constructor	,BYTEVECTOR-TYPE-MAKER)
			  (:include		,PARENT-BYTEVECTOR-TYPE)))

	   (cl-defgeneric ,BYTEVECTOR-TYPE (number-of-slots)
	     ,DOCSTRING)
	   (cl-defmethod  ,BYTEVECTOR-TYPE ((number-of-slots integer))
	     ,DOCSTRING
	     (mmec--make ,BYTEVECTOR-TYPE
			 :number-of-slots		number-of-slots
			 :slot-size			,SIZEOF-SLOT
			 :signed			,SIGNED-BOOL
			 :obj			(mmec-c-make-bytevector number-of-slots ,SIZEOF-SLOT ,SIGNED-BOOL)
			 :number-of-allocated-bytes	(* number-of-slots ,SIZEOF-SLOT)))
	   ))))

  (mmec--define-bytevector-type char	signed-integer)
  (mmec--define-bytevector-type schar	signed-integer)
  (mmec--define-bytevector-type uchar	unsigned-integer)
  (mmec--define-bytevector-type wchar	unsigned-integer)
  (mmec--define-bytevector-type sshrt	signed-integer)
  (mmec--define-bytevector-type ushrt	unsigned-integer)
  (mmec--define-bytevector-type sint	signed-integer)
  (mmec--define-bytevector-type uint	unsigned-integer)
  (mmec--define-bytevector-type slong	signed-integer)
  (mmec--define-bytevector-type ulong	unsigned-integer)
  (mmec--define-bytevector-type sllong	signed-integer)
  (mmec--define-bytevector-type ullong	unsigned-integer)
  (mmec--define-bytevector-type sintmax	signed-integer)
  (mmec--define-bytevector-type uintmax	unsigned-integer)
  (mmec--define-bytevector-type ssize	signed-integer)
  (mmec--define-bytevector-type usize	unsigned-integer)
  (mmec--define-bytevector-type ptrdiff	signed-integer)
  (mmec--define-bytevector-type sint8	signed-integer)
  (mmec--define-bytevector-type uint8	unsigned-integer)
  (mmec--define-bytevector-type sint16	signed-integer)
  (mmec--define-bytevector-type uint16	unsigned-integer)
  (mmec--define-bytevector-type sint32	signed-integer)
  (mmec--define-bytevector-type uint32	unsigned-integer)
  (mmec--define-bytevector-type sint64	signed-integer)
  (mmec--define-bytevector-type uint64	unsigned-integer)
  (mmec--define-bytevector-type float	floating-point)
  (mmec--define-bytevector-type double	floating-point)
  (mmec--define-bytevector-type ldouble	floating-point))


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
