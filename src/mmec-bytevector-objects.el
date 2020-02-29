;;; mmec-bytevector-objects.el --- bytevector objects

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-28 14:51:28 marco>
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
  signed-p
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

(defmacro mmec--define-bytevector-type (TYPESTEM PARENT-STEM)
  (let* ((BYTEVECTOR-TYPE		(mmec-sformat "mmec-%s-bytevector"		TYPESTEM))
	 (BYTEVECTOR-TYPE-MAKER		(mmec-sformat "mmec-%s-bytevector--make"	TYPESTEM))
	 (PARENT-BYTEVECTOR-TYPE	(mmec-sformat "mmec-%s-bytevector"		PARENT-STEM))
	 (SIZEOF-SLOT			(mmec-sformat "mmec-sizeof-%s"			TYPESTEM))
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
	 (when (> 0 number-of-slots)
	   (signal 'mmec-error-bytevector-constructor-invalid-number-of-slots (list (quote ,BYTEVECTOR-TYPE) number-of-slots)))
	 (mmec--make ,BYTEVECTOR-TYPE
		     :number-of-slots		number-of-slots
		     :slot-size			,SIZEOF-SLOT
		     :signed-p			,SIGNED-BOOL
		     :obj			(mmec-c-make-bytevector number-of-slots ,SIZEOF-SLOT ,SIGNED-BOOL)
		     :number-of-allocated-bytes	(* number-of-slots ,SIZEOF-SLOT)))
       )))

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
(mmec--define-bytevector-type ldouble	floating-point)


;;;; bytevector objects: inspection functions

(cl-defgeneric mmec-bytevector-empty-p (bv)
  "Return true if the bytevector BV is empty; otherwise return false.")
(cl-defmethod  mmec-bytevector-empty-p ((bv mmec-bytevector))
  "Return true if the bytevector BV is empty; otherwise return false."
  (cl-assert (mmec-bytevector-p bv))
  (zerop (mmec-bytevector-number-of-slots bv)))

(cl-defgeneric mmec-bytevector-not-empty-p (bv)
  "Return true if the bytevector BV is not empty; otherwise return false.")
(cl-defmethod  mmec-bytevector-not-empty-p ((bv mmec-bytevector))
  "Return true if the bytevector BV is not empty; otherwise return false."
  (cl-assert (mmec-bytevector-p bv))
  (not (zerop (mmec-bytevector-number-of-slots bv))))

(cl-defgeneric mmec-bytevector-last-slot-index (bv)
  "Return a value of type `integer' representing the slot index of the last slot.")
(cl-defmethod  mmec-bytevector-last-slot-index ((bv mmec-bytevector))
  "Return a value of type `integer' representing the slot index of the last slot.

If the bytevector is empty: signal the condition `mmec-error-bytevector-is-empty'."
  (cl-assert (mmec-bytevector-p bv))
  (let ((N (mmec-bytevector-number-of-slots bv)))
    (if (zerop N)
	(signal 'mmec-error-bytevector-is-empty (list 'mmec-bytevector-last-slot-index bv))
      (1- N))))

(cl-defgeneric mmec-bytevector-valid-slot-index-p (bv idx)
  "Return true if IDX is a valid slot index for the bytevector BV; otherwise return false.")
(cl-defmethod  mmec-bytevector-valid-slot-index-p ((bv mmec-bytevector) (idx integer))
  "Return true if IDX is a valid slot index for the bytevector BV; otherwise return false."
  (and (<= 0 idx) (< idx (mmec-bytevector-number-of-slots bv))))

(cl-defgeneric mmec-bytevector-valid-past-slot-index-p (bv idx)
  "Return true if IDX is a valid end-of-span slot index for the bytevector BV; otherwise return false.")
(cl-defmethod  mmec-bytevector-valid-past-slot-index-p ((bv mmec-bytevector) (idx integer))
  "Return true if IDX is a valid end-of-span slot index for the bytevector BV; otherwise return false."
  (and (<= 0 idx) (<= idx (mmec-bytevector-number-of-slots bv))))

(cl-defgeneric mmec-bytevector-valid-start-and-past-p (bv start past)
  "Return true if START and PAST are valid slot span selectorsfor the bytevector BV; otherwise return false.")
(cl-defmethod  mmec-bytevector-valid-start-and-past-p ((bv mmec-bytevector) (start integer) (past integer))
  "Return true if START and PAST are valid slot span selectors for the bytevector BV; otherwise return false."
  (and (mmec-bytevector-valid-slot-index-p bv start)
       (mmec-bytevector-valid-past-slot-index-p bv past)))


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

(cl-macrolet
    ((mmec--defgetter (TYPESTEM CTYPE)
		      (let* ((TYPESTEM.str	(symbol-name TYPESTEM))
			     (NUMTYPE.str	(concat "mmec-" TYPESTEM.str))
			     (NUMTYPE		(intern NUMTYPE.str))
			     (BYTEVECTOR-TYPE	(intern (concat "mmec-" TYPESTEM.str "-bytevector")))
			     (DOCSTRING		(concat "Extract a value of type `" CTYPE "' from the bytevector BV at index IDX."))
			     (C-FUNC		(intern (concat "mmec-c-bytevector-" TYPESTEM.str "-ref"))))
			`(cl-defmethod mmec-bytevector-ref ((bv ,BYTEVECTOR-TYPE) (idx integer))
			   ,DOCSTRING
			   (cl-assert (mmec-fits-usize-p idx))
			   (if (mmec-bytevector-not-empty-p bv)
			       (mmec--make ,NUMTYPE :obj (,C-FUNC (mmec-bytevector-obj bv) idx))
			     (signal 'mmec-error-bytevector-is-empty (list bv idx))))))

     (mmec--defsetter (TYPESTEM CTYPE)
		      (let* ((TYPESTEM.str	(symbol-name TYPESTEM))
			     (NUMTYPE.str	(concat "mmec-" TYPESTEM.str))
			     (NUMTYPE		(intern NUMTYPE.str))
			     (BYTEVECTOR-TYPE	(intern (concat "mmec-" TYPESTEM.str "-bytevector")))
			     (DOCSTRING		(concat "Store a value VAL of type `" CTYPE "' into the bytevector BV at index IDX."))
			     (C-FUNC		(intern (concat "mmec-c-bytevector-" TYPESTEM.str "-set"))))
			`(cl-defmethod mmec-bytevector-set ((bv ,BYTEVECTOR-TYPE) (idx integer) (val ,NUMTYPE))
			   ,DOCSTRING
			   (cl-assert (mmec-fits-usize-p idx))
			   (if (mmec-bytevector-not-empty-p bv)
			       (,C-FUNC (mmec-bytevector-obj bv) idx (mmec--extract-obj ,NUMTYPE val))
			     (signal 'mmec-error-bytevector-is-empty (list bv idx))))))

     (mmec--defgetter-and-setter (TYPESTEM CTYPE)
				 `(progn
				    (mmec--defgetter ,TYPESTEM ,CTYPE)
				    (mmec--defsetter ,TYPESTEM ,CTYPE))))

  (mmec--defgetter-and-setter char	"char")
  (mmec--defgetter-and-setter schar	"signed char")
  (mmec--defgetter-and-setter uchar	"unsigned char")
  (mmec--defgetter-and-setter wchar	"wchar_t")
  (mmec--defgetter-and-setter sshrt	"signed shrt int")
  (mmec--defgetter-and-setter ushrt	"unsigned shrt int")
  (mmec--defgetter-and-setter sint	"signed int")
  (mmec--defgetter-and-setter uint	"unsigned int")
  (mmec--defgetter-and-setter slong	"signed long int")
  (mmec--defgetter-and-setter ulong	"unsigned long int")
  (mmec--defgetter-and-setter sllong	"signed long long int")
  (mmec--defgetter-and-setter ullong	"unsigned long long int")
  (mmec--defgetter-and-setter sintmax	"intmax_t")
  (mmec--defgetter-and-setter uintmax	"uintmax_t")
  (mmec--defgetter-and-setter ssize	"ssize_t")
  (mmec--defgetter-and-setter usize	"size_t")
  (mmec--defgetter-and-setter ptrdiff	"ptrdiff_t")
  (mmec--defgetter-and-setter sint8	"int8_t")
  (mmec--defgetter-and-setter uint8	"uint8_t")
  (mmec--defgetter-and-setter sint16	"int16_t")
  (mmec--defgetter-and-setter uint16	"uint16_t")
  (mmec--defgetter-and-setter sint32	"int32_t")
  (mmec--defgetter-and-setter uint32	"uint32_t")
  (mmec--defgetter-and-setter sint64	"int64_t")
  (mmec--defgetter-and-setter uint64	"uint64_t")
  (mmec--defgetter-and-setter float	"float")
  (mmec--defgetter-and-setter double	"double")
  (mmec--defgetter-and-setter ldouble	"ldouble"))


;;;; printing bytevectors

(cl-macrolet
    ((mmec--def (TYPESTEM)
		"This is for bytevector objects having an object of type `integer' as slot number internal representation."
		(let* ((BVTYPE		(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (OBJ-EXTRACTOR	(mmec-sformat "mmec-bytevector-obj" TYPESTEM))
		       (CLANG-GETTER	(mmec-sformat "mmec-c-bytevector-%s-ref" TYPESTEM))
		       (DOCSTRING	(format "Print to a stream the representation of a bytevector object of type `%s'." BVTYPE))
		       (PREFIX		(format "#s(%s" BVTYPE))
		       (SUFFIX		"\)"))
		  `(cl-defmethod cl-print-object ((bv ,BVTYPE) stream)
		     ,DOCSTRING
		     (princ ,PREFIX stream)
		     (let ((bv-intrep (,OBJ-EXTRACTOR bv)))
		       (dotimes (i (mmec-bytevector-number-of-slots bv))
			 (princ " " stream)
			 (princ (,CLANG-GETTER bv-intrep i) stream)))
		     (princ ,SUFFIX stream)))))
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
		"This is for bytevector objects having an object of type `float' as slot number internal representation."
		(let* ((BVTYPE		(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (OBJ-EXTRACTOR	(mmec-sformat "mmec-bytevector-obj" TYPESTEM))
		       (CLANG-GETTER	(mmec-sformat "mmec-c-bytevector-%s-ref" TYPESTEM))
		       (DOCSTRING	(format "Print to a stream the representation of a bytevector object of type `%s'." BVTYPE))
		       (PREFIX		(format "#s(%s" BVTYPE))
		       (SUFFIX		"\)"))
		  `(cl-defmethod cl-print-object ((bv ,BVTYPE) stream)
		     ,DOCSTRING
		     (princ ,PREFIX stream)
		     (let ((bv-intrep (,OBJ-EXTRACTOR bv)))
		       (dotimes (i (mmec-bytevector-number-of-slots bv))
			 (princ " " stream)
			 (princ (format "%g" (,CLANG-GETTER bv-intrep i)) stream)))
		     (princ ,SUFFIX stream)))))
  (mmec--def double))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		"This is for bytevector objects having a user-pointer object as internal representation."
		(let* ((BVTYPE		(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (OBJ-EXTRACTOR	(mmec-sformat "mmec-bytevector-obj" TYPESTEM))
		       (CLANG-GETTER	(mmec-sformat "mmec-c-bytevector-%s-ref" TYPESTEM))
		       (CLANG-PRINTER	(mmec-sformat "mmec-c-%s-print-to-string" TYPESTEM))
		       (DOCSTRING	(format "Print to a stream the representation of a bytevector object of type `%s'." BVTYPE))
		       (PREFIX		(format "#s(%s" BVTYPE))
		       (SUFFIX		"\)"))
		  `(cl-defmethod cl-print-object ((bv ,BVTYPE) stream)
		     ,DOCSTRING
		     (princ ,PREFIX stream)
		     (let ((bv-intrep (,OBJ-EXTRACTOR bv)))
		       (dotimes (i (mmec-bytevector-number-of-slots bv))
			 (princ " " stream)
			 (princ (,CLANG-PRINTER (,CLANG-GETTER bv-intrep i)) stream)))
		     (princ ,SUFFIX stream)))))
  (mmec--def wchar)
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
  (mmec--def sint32)
  (mmec--def uint32)
  (mmec--def sint64)
  (mmec--def uint64)
  (mmec--def float)
  (mmec--def ldouble))


;;;; bytevector objects: comparison functions

;;; API functions
(cl-macrolet
    ((mmec--def (FUNCSTEM RESULT-DOCSTRING)
		(let* ((FUNCNAME	(mmec-sformat "mmec-bytevector-%s"   FUNCSTEM))
		       (FUNCNAME-6	(mmec-sformat "mmec-bytevector-%s-6" FUNCSTEM))
		       (DOCSTRING	(format "Compare the two bytevectors BV1 and BV2.

This function is an adapter for the generic function `%s' and return its
return value.

This function  accepts the  following keys  that specify  a span  in the
bytevectors to use in the comparison:

:start1
   Select  the inclusive  slot  index in  BV1 from  which  to start  the
   comparison.  Defaults to 0.

:start2
   Select  the inclusive  slot  index in  BV2 from  which  to start  the
   comparison.  Defaults to 0.

:past1
   Select  the  exclusive  slot  index  in  BV1  at  which  to  end  the
   comparison.  Defaults to the number of slots in BV1.

:past2
   Select  the  exclusive  slot  index  in  BV2  at  which  to  end  the
   comparison.  Defaults to the number of slots in BV2.

The arguments START  and PAST must be valid slot  indexes satisfying the
conditions:

  0 <= START <= PAST <= number of slots

otherwise the  behaviour of the  functions is undefined.   The functions
compare a  span in  the data  area of the  bytevectors starting  at slot
index START, included, and ending at slot index PAST, excluded.

%s" FUNCNAME-6 RESULT-DOCSTRING)))
		  `(cl-defun ,FUNCNAME (bv1 bv2 &key (start1 0) (start2 0)
					    (past1 (mmec-bytevector-number-of-slots bv1))
					    (past2 (mmec-bytevector-number-of-slots bv2)))
		     ,DOCSTRING
		     (cl-assert (<= 0 start1 past1 (mmec-bytevector-number-of-slots bv1)))
		     (cl-assert (<= 0 start2 past2 (mmec-bytevector-number-of-slots bv2)))
		     (,FUNCNAME-6 bv1 start1 past1 bv2 start2 past2)))))
  (mmec--def compare "Return the following code:

 0, if the following condition is true:

    (PAST1 - START1) == (PAST2 - START2)

  and all the values in the slots are equal slot by slot.

+1, if the following condition is true:

    (PAST1 - START1) > (PAST2 - START2)

  or, while  visiting the  slots from  START to PAST,  BV1 holds  a slot
  value that is greater than the corresponding slot value in BV2.

-1, if the following condition is true:

   (PAST1 - START1) < (PAST2 - START2)

  or, while  visiting the  slots from  START to PAST,  BV1 holds  a slot
  value that is less than the corresponding slot value in BV2.")
  (mmec--def equal	"Return true if BV1 = BV2 according to `mmec-bytevector-compare'; otherwise return false.")
  (mmec--def less	"Return true if BV1 < BV2 according to `mmec-bytevector-compare'; otherwise return false.")
  (mmec--def greater	"Return true if BV1 > BV2 according to `mmec-bytevector-compare'; otherwise return false.")
  (mmec--def leq	"Return true if BV1 <= BV2 according to `mmec-bytevector-compare'; otherwise return false.")
  (mmec--def geq	"Return true if BV1 >= BV2 according to `mmec-bytevector-compare'; otherwise return false."))

;;; --------------------------------------------------------------------
;;; bytevector span comparison generic functions

(cl-defgeneric mmec-bytevector-compare-6 (bv1 start1 past1 bv2 start2 past2)
  "Compare the selected spans in the bytevectors BV1 and BV2, return -1, 0 or +1.

The arguments START  and PAST must be valid slot  indexes satisfying the
conditions:

  0 <= START <= PAST <= number of slots

otherwise the  behaviour of the  functions is undefined.   The functions
compare a  span in  the data  area of the  bytevectors starting  at slot
index START, included, and ending at slot index PAST, excluded.

 return the following code:

 0, if the following condition is true:

    (PAST1 - START1) == (PAST2 - START2)

  and all the values in the slots are equal slot by slot.

+1, if the following condition is true:

    (PAST1 - START1) > (PAST2 - START2)

  or, while  visiting the  slots from  START to PAST,  BV1 holds  a slot
  value that is greater than the corresponding slot value in BV2.

-1, if the following condition is true:

   (PAST1 - START1) < (PAST2 - START2)

  or, while  visiting the  slots from  START to PAST,  BV1 holds  a slot
  value that is less than the corresponding slot value in BV2.")

(cl-macrolet
    ((mmec--def (FUNCSTEM)
		(let* ((FUNCNAME (mmec-sformat "mmec-bytevector-%s-6" FUNCSTEM)))
		  `(cl-defgeneric ,FUNCNAME (bv1 start1 past1 bv2 start2 past2)
		     "Compare the selected spans in the bytevectors BV1 and BV2: return true or false.

The meaning of  the arguments BV1, START1, PAST1, BV2,  START2, PAST2 is
the    same    as   in    the    call    to   the    generic    function
`mmec-bytevector-compare-6'."))))

  (mmec--def equal)
  (mmec--def less)
  (mmec--def greater)
  (mmec--def leq)
  (mmec--def geq))


;;;; bytevector objects: comparison specialised methods

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((BV-TYPE		(mmec-sformat "mmec-%s-bytevector"		TYPESTEM))
		       (CFUNC-COMPARE	(mmec-sformat "mmec-c-%s-bytevector-compare"	TYPESTEM))
		       (CFUNC-EQUAL	(mmec-sformat "mmec-c-%s-bytevector-equal"	TYPESTEM))
		       (CFUNC-LESS	(mmec-sformat "mmec-c-%s-bytevector-less"	TYPESTEM))
		       (CFUNC-GREATER	(mmec-sformat "mmec-c-%s-bytevector-greater"	TYPESTEM))
		       (CFUNC-LEQ	(mmec-sformat "mmec-c-%s-bytevector-leq"	TYPESTEM))
		       (CFUNC-GEQ	(mmec-sformat "mmec-c-%s-bytevector-geq"	TYPESTEM)))
		  `(progn
		     (cl-defmethod mmec-bytevector-compare-6 ((bv1 ,BV-TYPE) (start1 integer) (past1 integer)
							      (bv2 ,BV-TYPE) (start2 integer) (past2 integer))
		       "Compare the selected spans in the bytevectors BV1 and BV2, return -1, 0 or +1.

The arguments START  and PAST must be valid slot  indexes satisfying the
conditions:

  0 <= START <= PAST <= number of slots

otherwise the  behaviour of the  functions is undefined.   The functions
compare a  span in  the data  area of the  bytevectors starting  at slot
index START, included, and ending at slot index PAST, excluded.

Return the following code:

 0, if the following condition is true:

    (PAST1 - START1) == (PAST2 - START2)

  and all the values in the slots are equal slot by slot.

+1, if the following condition is true:

    (PAST1 - START1) > (PAST2 - START2)

  or, while  visiting the  slots from  START to PAST,  BV1 holds  a slot
  value that is greater than the corresponding slot value in BV2.

-1, if the following condition is true:

   (PAST1 - START1) < (PAST2 - START2)

  or, while  visiting the  slots from  START to PAST,  BV1 holds  a slot
  value that is less than the corresponding slot value in BV2."
		       (cl-assert (<= 0 start1 past1 (mmec-bytevector-number-of-slots bv1)))
		       (cl-assert (<= 0 start2 past2 (mmec-bytevector-number-of-slots bv2)))
		       (,CFUNC-COMPARE (mmec--extract-obj ,BV-TYPE bv1) start1 past1
				       (mmec--extract-obj ,BV-TYPE bv2) start2 past2))

		     (cl-defmethod mmec-bytevector-equal-6 ((bv1 ,BV-TYPE) (start1 integer) (past1 integer)
							    (bv2 ,BV-TYPE) (start2 integer) (past2 integer))
		       "Compare the selected spans in the bytevectors BV1 and BV2: return true or false."
		       (cl-assert (<= 0 start1 past1 (mmec-bytevector-number-of-slots bv1)))
		       (cl-assert (<= 0 start2 past2 (mmec-bytevector-number-of-slots bv2)))
		       (,CFUNC-EQUAL (mmec--extract-obj ,BV-TYPE bv1) start1 past1
				     (mmec--extract-obj ,BV-TYPE bv2) start2 past2))

		     (cl-defmethod mmec-bytevector-less-6 ((bv1 ,BV-TYPE) (start1 integer) (past1 integer)
							   (bv2 ,BV-TYPE) (start2 integer) (past2 integer))
		       "Compare the selected spans in the bytevectors BV1 and BV2: return true or false."
		       (cl-assert (<= 0 start1 past1 (mmec-bytevector-number-of-slots bv1)))
		       (cl-assert (<= 0 start2 past2 (mmec-bytevector-number-of-slots bv2)))
		       (,CFUNC-LESS (mmec--extract-obj ,BV-TYPE bv1) start1 past1
				    (mmec--extract-obj ,BV-TYPE bv2) start2 past2))

		     (cl-defmethod mmec-bytevector-greater-6 ((bv1 ,BV-TYPE) (start1 integer) (past1 integer)
							      (bv2 ,BV-TYPE) (start2 integer) (past2 integer))
		       "Compare the selected spans in the bytevectors BV1 and BV2: return true or false."
		       (cl-assert (<= 0 start1 past1 (mmec-bytevector-number-of-slots bv1)))
		       (cl-assert (<= 0 start2 past2 (mmec-bytevector-number-of-slots bv2)))
		       (,CFUNC-GREATER (mmec--extract-obj ,BV-TYPE bv1) start1 past1
				       (mmec--extract-obj ,BV-TYPE bv2) start2 past2))

		     (cl-defmethod mmec-bytevector-leq-6 ((bv1 ,BV-TYPE) (start1 integer) (past1 integer)
							  (bv2 ,BV-TYPE) (start2 integer) (past2 integer))
		       "Compare the selected spans in the bytevectors BV1 and BV2: return true or false."
		       (cl-assert (<= 0 start1 past1 (mmec-bytevector-number-of-slots bv1)))
		       (cl-assert (<= 0 start2 past2 (mmec-bytevector-number-of-slots bv2)))
		       (,CFUNC-LEQ (mmec--extract-obj ,BV-TYPE bv1) start1 past1
				   (mmec--extract-obj ,BV-TYPE bv2) start2 past2))

		     (cl-defmethod mmec-bytevector-geq-6 ((bv1 ,BV-TYPE) (start1 integer) (past1 integer)
							  (bv2 ,BV-TYPE) (start2 integer) (past2 integer))
		       "Compare the selected spans in the bytevectors BV1 and BV2: return true or false."
		       (cl-assert (<= 0 start1 past1 (mmec-bytevector-number-of-slots bv1)))
		       (cl-assert (<= 0 start2 past2 (mmec-bytevector-number-of-slots bv2)))
		       (,CFUNC-GEQ (mmec--extract-obj ,BV-TYPE bv1) start1 past1
				   (mmec--extract-obj ,BV-TYPE bv2) start2 past2))
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
  (mmec--def sintmax)
  (mmec--def uintmax)
  (mmec--def ssize)
  (mmec--def usize)
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


;;;; bytevector objects: conversion to/from list

(cl-defgeneric mmec-bytevector-to-list (bv)
  "Convert a bytevector object into a list of its elements.")

(cl-defmethod mmec-bytevector-to-list ((bv mmec-bytevector))
  (cl-loop for i from 0 to (mmec-bytevector-last-slot-index bv)
	   collect (mmec-bytevector-ref bv i)))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (BVTYPE		(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (CONSTRUCTOR	BVTYPE)
		       (FUNC		(mmec-sformat "mmec-%s-bytevector-from-list" TYPESTEM))
		       (DOCSTRING	(format "Convert a list object into a bytevector object of type `%s'." BVTYPE)))
		  `(progn
		     (cl-defgeneric ,FUNC (ELL)
		       ,DOCSTRING)
		     (cl-defmethod  ,FUNC ((ELL list))
		       ,DOCSTRING
		       (cl-loop with bv = (,CONSTRUCTOR (length ELL))
				for elt in ELL
				for i from 0
				do (mmec-bytevector-set bv i (,NUMTYPE elt))
				finally (return bv)))
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
  (mmec--def sintmax)
  (mmec--def uintmax)
  (mmec--def ssize)
  (mmec--def usize)
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


;;;; bytevector objects: conversion to/from vector

(cl-defgeneric mmec-bytevector-to-vector (bv)
  "Convert a bytevector object into a vector of its elements.")

(cl-defmethod mmec-bytevector-to-vector ((bv mmec-bytevector))
  (cl-loop with vec = (make-vector (mmec-bytevector-number-of-slots bv) nil)
	   for i from 0 to (mmec-bytevector-last-slot-index bv)
	   do (setf (elt vec i) (mmec-bytevector-ref bv i))
	   finally (return vec)))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s"				TYPESTEM))
		       (BVTYPE		(mmec-sformat "mmec-%s-bytevector"		TYPESTEM))
		       (FUNC		(mmec-sformat "mmec-%s-bytevector-from-vector"	TYPESTEM))
		       (CONSTRUCTOR	BVTYPE)
		       (DOCSTRING	(format "Convert a vector object into a bytevector object of type `%s'." BVTYPE)))
		  `(progn
		     (cl-defgeneric ,FUNC (VEC)
		       ,DOCSTRING)
		     (cl-defmethod  ,FUNC ((VEC vector))
		       ,DOCSTRING
		       (cl-loop with bv = (,CONSTRUCTOR (length VEC))
				for elt across VEC
		     		for i from 0
		     		do (mmec-bytevector-set bv i (,NUMTYPE elt))
				finally (return bv)))
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
  (mmec--def sintmax)
  (mmec--def uintmax)
  (mmec--def ssize)
  (mmec--def usize)
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


;;;; bytevector objects: subsequence

(cl-defun mmec-subbytevector (bv &key (start 0) (past (mmec-bytevector-number-of-slots bv)))
  "Extract a subsequence from the bytevector object BV.

The arguments START and PAST must be integers satisfying the conditions:

  0 <= start <= past <= number of slots
"
  (mmec-subbytevector-3 bv start past))

(cl-defgeneric mmec-subbytevector-3 (bv start past)
  "Extract a subsequence from the bytevector object BV.

The arguments START and PAST must be integers satisfying the conditions:

  0 <= start <= past <= number of slots
")

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((BVTYPE (mmec-sformat "mmec-%s-bytevector" TYPESTEM)))
		  `(cl-defmethod mmec-subbytevector-3 ((bv ,BVTYPE) (start integer) (past integer))
		     "Extract a subsequence from the bytevector object BV.

The arguments START and PAST must be integers satisfying the conditions:

  0 <= start <= past <= number of slots
"
		     (cl-assert (<= 0 start past (mmec-bytevector-number-of-slots bv)))
		     (mmec--make ,BVTYPE
				 :number-of-slots	(- past start)
				 :slot-size		(mmec-bytevector-slot-size       bv)
				 :signed-p		(mmec-bytevector-signed-p        bv)
				 :obj			(mmec-c-subbytevector (mmec-bytevector-obj bv) start past)
				 :number-of-allocated-bytes (* (mmec-bytevector-slot-size bv) (- past start)))))))
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
  (mmec--def sintmax)
  (mmec--def uintmax)
  (mmec--def ssize)
  (mmec--def usize)
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

(provide 'mmec-bytevector-objects)

;;; end of file
