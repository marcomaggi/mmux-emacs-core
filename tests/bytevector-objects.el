;;; bytevector-objects.el --- dynamic module test

;; Copyright (C) 2020 by Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>

;; This program is  free software; you can redistribute  it and/or modify it under the  terms of the
;; GNU General Public License as published by the  Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied  warranty of MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.
;;
;; You should have  received a copy of the  GNU General Public License along with  this program.  If
;; not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mmec)


;;;; helpers

(defmacro my--unsupported-type-error (TYPE UNSUPPORTED-TYPE INIT)
  `(should (condition-case nil
	       (,TYPE (,UNSUPPORTED-TYPE ,INIT))
	     ((mmec-error-unsupported-init-type)
	      t)
	     (t nil))))

(defmacro my--init-argument-does-not-fit (TYPE INIT)
  `(should (condition-case nil
	       (,TYPE ,INIT)
	     ((mmec-error-value-out-of-range)
	      t)
	     (t nil))))


;;;; bytevector makers

(ert-deftest mmec-uint8-bytevector ()
  "Build a `mmec-uint8-bytevector' object."
  (should	(mmec-bytevector-p			(make-mmec-uint8-bytevector 123)))
  (should	(mmec-bytevector-p			(mmec-uint8-bytevector 123)))
  (should	(mmec-integer-bytevector-p		(mmec-uint8-bytevector 123)))
  (should (not	(mmec-floating-point-bytevector-p	(mmec-uint8-bytevector 123))))
  (should	(mmec-uint8-bytevector-p		(mmec-uint8-bytevector 123)))
  (should (not	(mmec-uint16-bytevector-p		(mmec-uint8-bytevector 123))))
  (should (not	(mmec-uint32-bytevector-p		(mmec-uint8-bytevector 123))))
  (should (not	(mmec-uint64-bytevector-p		(mmec-uint8-bytevector 123))))
  (should (not	(mmec-sint8-bytevector-p		(mmec-uint8-bytevector 123))))
  (should (not	(mmec-sint16-bytevector-p		(mmec-uint8-bytevector 123))))
  (should (not	(mmec-sint32-bytevector-p		(mmec-uint8-bytevector 123))))
  (should (not	(mmec-sint64-bytevector-p		(mmec-uint8-bytevector 123))))
  (should (not	(mmec-float-bytevector-p		(mmec-uint8-bytevector 123))))
  (should (not	(mmec-double-bytevector-p		(mmec-uint8-bytevector 123))))
  (should (not	(mmec-ldouble-bytevector-p		(mmec-uint8-bytevector 123)))))

(ert-deftest mmec-sint8-bytevector ()
  "Build a `mmec-sint8-bytevector' object."
  (should	(mmec-bytevector-p			(make-mmec-sint8-bytevector 123)))
  (should	(mmec-bytevector-p			(mmec-sint8-bytevector 123)))
  (should	(mmec-integer-bytevector-p		(mmec-sint8-bytevector 123)))
  (should (not	(mmec-floating-point-bytevector-p	(mmec-sint8-bytevector 123))))
  (should (not	(mmec-uint8-bytevector-p		(mmec-sint8-bytevector 123))))
  (should (not	(mmec-uint16-bytevector-p		(mmec-sint8-bytevector 123))))
  (should (not	(mmec-uint32-bytevector-p		(mmec-sint8-bytevector 123))))
  (should (not	(mmec-uint64-bytevector-p		(mmec-sint8-bytevector 123))))
  (should 	(mmec-sint8-bytevector-p		(mmec-sint8-bytevector 123)))
  (should (not	(mmec-sint16-bytevector-p		(mmec-sint8-bytevector 123))))
  (should (not	(mmec-sint32-bytevector-p		(mmec-sint8-bytevector 123))))
  (should (not	(mmec-sint64-bytevector-p		(mmec-sint8-bytevector 123))))
  (should (not	(mmec-float-bytevector-p		(mmec-sint8-bytevector 123))))
  (should (not	(mmec-double-bytevector-p		(mmec-sint8-bytevector 123))))
  (should (not	(mmec-ldouble-bytevector-p		(mmec-sint8-bytevector 123)))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-uint16-bytevector ()
  "Build a `mmec-uint16-bytevector' object."
  (should	(mmec-bytevector-p			(mmec-uint16-bytevector 123)))
  (should	(mmec-integer-bytevector-p		(mmec-uint16-bytevector 123)))
  (should (not	(mmec-floating-point-bytevector-p	(mmec-uint16-bytevector 123))))
  (should (not	(mmec-uint8-bytevector-p		(mmec-uint16-bytevector 123))))
  (should 	(mmec-uint16-bytevector-p		(mmec-uint16-bytevector 123)))
  (should (not	(mmec-uint32-bytevector-p		(mmec-uint16-bytevector 123))))
  (should (not	(mmec-uint64-bytevector-p		(mmec-uint16-bytevector 123))))
  (should (not	(mmec-sint8-bytevector-p		(mmec-uint16-bytevector 123))))
  (should (not	(mmec-sint16-bytevector-p		(mmec-uint16-bytevector 123))))
  (should (not	(mmec-sint32-bytevector-p		(mmec-uint16-bytevector 123))))
  (should (not	(mmec-sint64-bytevector-p		(mmec-uint16-bytevector 123))))
  (should (not	(mmec-float-bytevector-p		(mmec-uint16-bytevector 123))))
  (should (not	(mmec-double-bytevector-p		(mmec-uint16-bytevector 123))))
  (should (not	(mmec-ldouble-bytevector-p		(mmec-uint16-bytevector 123)))))

(ert-deftest mmec-sint16-bytevector ()
  "Build a `mmec-sint16-bytevector' object."
  (should	(mmec-bytevector-p			(mmec-sint16-bytevector 123)))
  (should	(mmec-integer-bytevector-p		(mmec-sint16-bytevector 123)))
  (should (not	(mmec-floating-point-bytevector-p	(mmec-sint16-bytevector 123))))
  (should (not	(mmec-uint8-bytevector-p		(mmec-sint16-bytevector 123))))
  (should (not	(mmec-uint16-bytevector-p		(mmec-sint16-bytevector 123))))
  (should (not	(mmec-uint32-bytevector-p		(mmec-sint16-bytevector 123))))
  (should (not	(mmec-uint64-bytevector-p		(mmec-sint16-bytevector 123))))
  (should (not	(mmec-sint8-bytevector-p		(mmec-sint16-bytevector 123))))
  (should 	(mmec-sint16-bytevector-p		(mmec-sint16-bytevector 123)))
  (should (not	(mmec-sint32-bytevector-p		(mmec-sint16-bytevector 123))))
  (should (not	(mmec-sint64-bytevector-p		(mmec-sint16-bytevector 123))))
  (should (not	(mmec-float-bytevector-p		(mmec-sint16-bytevector 123))))
  (should (not	(mmec-double-bytevector-p		(mmec-sint16-bytevector 123))))
  (should (not	(mmec-ldouble-bytevector-p		(mmec-sint16-bytevector 123)))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-uint32-bytevector ()
  "Build a `mmec-uint32-bytevector' object."
  (should	(mmec-bytevector-p			(mmec-uint32-bytevector 123)))
  (should	(mmec-integer-bytevector-p		(mmec-uint32-bytevector 123)))
  (should (not	(mmec-floating-point-bytevector-p	(mmec-uint32-bytevector 123))))
  (should (not	(mmec-uint8-bytevector-p		(mmec-uint32-bytevector 123))))
  (should (not	(mmec-uint16-bytevector-p		(mmec-uint32-bytevector 123))))
  (should 	(mmec-uint32-bytevector-p		(mmec-uint32-bytevector 123)))
  (should (not	(mmec-uint64-bytevector-p		(mmec-uint32-bytevector 123))))
  (should (not	(mmec-sint8-bytevector-p		(mmec-uint32-bytevector 123))))
  (should (not	(mmec-sint16-bytevector-p		(mmec-uint32-bytevector 123))))
  (should (not	(mmec-sint32-bytevector-p		(mmec-uint32-bytevector 123))))
  (should (not	(mmec-sint64-bytevector-p		(mmec-uint32-bytevector 123))))
  (should (not	(mmec-float-bytevector-p		(mmec-uint32-bytevector 123))))
  (should (not	(mmec-double-bytevector-p		(mmec-uint32-bytevector 123))))
  (should (not	(mmec-ldouble-bytevector-p		(mmec-uint32-bytevector 123)))))

(ert-deftest mmec-sint32-bytevector ()
  "Build a `mmec-sint32-bytevector' object."
  (should	(mmec-bytevector-p			(mmec-sint32-bytevector 123)))
  (should	(mmec-integer-bytevector-p		(mmec-sint32-bytevector 123)))
  (should (not	(mmec-floating-point-bytevector-p	(mmec-sint32-bytevector 123))))
  (should (not	(mmec-uint8-bytevector-p		(mmec-sint32-bytevector 123))))
  (should (not	(mmec-uint16-bytevector-p		(mmec-sint32-bytevector 123))))
  (should (not	(mmec-uint32-bytevector-p		(mmec-sint32-bytevector 123))))
  (should (not	(mmec-uint64-bytevector-p		(mmec-sint32-bytevector 123))))
  (should (not 	(mmec-sint8-bytevector-p		(mmec-sint32-bytevector 123))))
  (should (not	(mmec-sint16-bytevector-p		(mmec-sint32-bytevector 123))))
  (should 	(mmec-sint32-bytevector-p		(mmec-sint32-bytevector 123)))
  (should (not	(mmec-sint64-bytevector-p		(mmec-sint32-bytevector 123))))
  (should (not	(mmec-float-bytevector-p		(mmec-sint32-bytevector 123))))
  (should (not	(mmec-double-bytevector-p		(mmec-sint32-bytevector 123))))
  (should (not	(mmec-ldouble-bytevector-p		(mmec-sint32-bytevector 123)))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-uint64-bytevector ()
  "Build a `mmec-uint64-bytevector' object."
  (should	(mmec-bytevector-p			(mmec-uint64-bytevector 123)))
  (should	(mmec-integer-bytevector-p		(mmec-uint64-bytevector 123)))
  (should (not	(mmec-floating-point-bytevector-p	(mmec-uint64-bytevector 123))))
  (should (not	(mmec-uint8-bytevector-p		(mmec-uint64-bytevector 123))))
  (should (not	(mmec-uint16-bytevector-p		(mmec-uint64-bytevector 123))))
  (should (not 	(mmec-uint32-bytevector-p		(mmec-uint64-bytevector 123))))
  (should 	(mmec-uint64-bytevector-p		(mmec-uint64-bytevector 123)))
  (should (not	(mmec-sint8-bytevector-p		(mmec-uint64-bytevector 123))))
  (should (not	(mmec-sint16-bytevector-p		(mmec-uint64-bytevector 123))))
  (should (not	(mmec-sint32-bytevector-p		(mmec-uint64-bytevector 123))))
  (should (not	(mmec-sint64-bytevector-p		(mmec-uint64-bytevector 123))))
  (should (not	(mmec-float-bytevector-p		(mmec-uint64-bytevector 123))))
  (should (not	(mmec-double-bytevector-p		(mmec-uint64-bytevector 123))))
  (should (not	(mmec-ldouble-bytevector-p		(mmec-uint64-bytevector 123)))))

(ert-deftest mmec-sint64-bytevector ()
  "Build a `mmec-642-bytevector' object."
  (should	(mmec-bytevector-p			(mmec-sint64-bytevector 123)))
  (should	(mmec-integer-bytevector-p		(mmec-sint64-bytevector 123)))
  (should (not	(mmec-floating-point-bytevector-p	(mmec-sint64-bytevector 123))))
  (should (not	(mmec-uint8-bytevector-p		(mmec-sint64-bytevector 123))))
  (should (not	(mmec-uint16-bytevector-p		(mmec-sint64-bytevector 123))))
  (should (not	(mmec-uint32-bytevector-p		(mmec-sint64-bytevector 123))))
  (should (not	(mmec-uint64-bytevector-p		(mmec-sint64-bytevector 123))))
  (should (not 	(mmec-sint8-bytevector-p		(mmec-sint64-bytevector 123))))
  (should (not	(mmec-sint16-bytevector-p		(mmec-sint64-bytevector 123))))
  (should (not 	(mmec-sint32-bytevector-p		(mmec-sint64-bytevector 123))))
  (should 	(mmec-sint64-bytevector-p		(mmec-sint64-bytevector 123))))
  (should (not	(mmec-float-bytevector-p		(mmec-sint64-bytevector 123))))
  (should (not	(mmec-double-bytevector-p		(mmec-sint64-bytevector 123))))
  (should (not	(mmec-ldouble-bytevector-p		(mmec-sint64-bytevector 123))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-floating-point-bytevector ()
  "Build a `mmec-float-bytevector' object."
  (should	(mmec-bytevector-p			(mmec-float-bytevector 123)))
  (should (not	(mmec-integer-bytevector-p		(mmec-float-bytevector 123))))
  (should 	(mmec-floating-point-bytevector-p	(mmec-float-bytevector 123)))
  (should (not	(mmec-uint8-bytevector-p		(mmec-float-bytevector 123))))
  (should (not	(mmec-uint16-bytevector-p		(mmec-float-bytevector 123))))
  (should (not	(mmec-uint32-bytevector-p		(mmec-float-bytevector 123))))
  (should (not	(mmec-uint64-bytevector-p		(mmec-float-bytevector 123))))
  (should (not	(mmec-sint8-bytevector-p		(mmec-float-bytevector 123))))
  (should (not	(mmec-sint16-bytevector-p		(mmec-float-bytevector 123))))
  (should (not	(mmec-sint32-bytevector-p		(mmec-float-bytevector 123))))
  (should (not	(mmec-sint64-bytevector-p		(mmec-float-bytevector 123))))
  (should 	(mmec-float-bytevector-p		(mmec-float-bytevector 123)))
  (should (not	(mmec-double-bytevector-p		(mmec-float-bytevector 123))))
  (should (not	(mmec-ldouble-bytevector-p		(mmec-float-bytevector 123)))))

(ert-deftest mmec-double-bytevector ()
  "Build a `mmec-double-bytevector' object."
  (should	(mmec-bytevector-p			(mmec-double-bytevector 123)))
  (should (not	(mmec-integer-bytevector-p		(mmec-double-bytevector 123))))
  (should 	(mmec-floating-point-bytevector-p	(mmec-double-bytevector 123)))
  (should (not	(mmec-uint8-bytevector-p		(mmec-double-bytevector 123))))
  (should (not	(mmec-uint16-bytevector-p		(mmec-double-bytevector 123))))
  (should (not	(mmec-uint32-bytevector-p		(mmec-double-bytevector 123))))
  (should (not	(mmec-uint64-bytevector-p		(mmec-double-bytevector 123))))
  (should (not	(mmec-sint8-bytevector-p		(mmec-double-bytevector 123))))
  (should (not	(mmec-sint16-bytevector-p		(mmec-double-bytevector 123))))
  (should (not	(mmec-sint32-bytevector-p		(mmec-double-bytevector 123))))
  (should (not	(mmec-sint64-bytevector-p		(mmec-double-bytevector 123))))
  (should (not	(mmec-float-bytevector-p		(mmec-double-bytevector 123))))
  (should 	(mmec-double-bytevector-p		(mmec-double-bytevector 123)))
  (should (not	(mmec-ldouble-bytevector-p		(mmec-double-bytevector 123)))))

(ert-deftest mmec-ldouble-bytevector ()
  "Build a `mmec-ldouble-bytevector' object."
  (should	(mmec-bytevector-p			(mmec-ldouble-bytevector 123)))
  (should (not	(mmec-integer-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should 	(mmec-floating-point-bytevector-p	(mmec-ldouble-bytevector 123)))
  (should (not	(mmec-uint8-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should (not	(mmec-uint16-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should (not	(mmec-uint32-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should (not	(mmec-uint64-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should (not	(mmec-sint8-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should (not	(mmec-sint16-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should (not	(mmec-sint32-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should (not	(mmec-sint64-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should (not	(mmec-float-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should (not	(mmec-double-bytevector-p		(mmec-ldouble-bytevector 123))))
  (should 	(mmec-ldouble-bytevector-p		(mmec-ldouble-bytevector 123))))


;;;; bytevector objects: getters and setters

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME	(mmec-sformat "mmec-%s-bytevector-test" TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (BVTYPE		(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (DOCSTRING	(format "Test setters and getters for a `%s' object." BVTYPE)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     (let ((bv	(,BVTYPE 10)))
		       (dotimes (i 10)
			 (mmec-bytevector-set bv i (,NUMTYPE (+ 10 i))))
		       (dotimes (i 10)
			 (should (mmec= (,NUMTYPE (+ 10 i))
					(mmec-bytevector-ref bv i)))))))))
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


;;;; slots inspection

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME	(mmec-sformat "mmec-%s-bytevector-slots-test" TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (BVTYPE		(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (SLOTSIZE	(mmec-sformat "mmec-sizeof-%s" TYPESTEM))
		       (ISSIGNED	(mmec-sformat "mmec-%s-is-signed" TYPESTEM))
		       (DOCSTRING	(format "Test inspecting the slots number and size of a `%s' object." BVTYPE)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     (let* ((len	10)
			    (bv		(,BVTYPE len)))
		       (should (= 10 (mmec-bytevector-number-of-slots bv)))
		       (should (= ,SLOTSIZE (mmec-bytevector-slot-size bv)))
		       (should (= (* 10 ,SLOTSIZE) (mmec-bytevector-number-of-allocated-bytes bv)))
		       (should (equal ,ISSIGNED (mmec-bytevector-signed-p bv)))
		       ;; emptyness
		       (should      (mmec-bytevector-empty-p (,BVTYPE 0)))
		       (should (not (mmec-bytevector-empty-p (,BVTYPE 10))))
		       (should      (mmec-bytevector-not-empty-p (,BVTYPE 10)))
		       (should (not (mmec-bytevector-not-empty-p (,BVTYPE 0))))
		       ;; slot index validation
		       (should (not (mmec-bytevector-valid-slot-index-p bv -1)))
		       (should      (mmec-bytevector-valid-slot-index-p bv 0))
		       (should      (mmec-bytevector-valid-slot-index-p bv (1- len)))
		       (should (not (mmec-bytevector-valid-slot-index-p bv len)))
		       ;; past slot index validation
		       (should (not (mmec-bytevector-valid-past-slot-index-p bv -1)))
		       (should      (mmec-bytevector-valid-past-slot-index-p bv 0))
		       (should      (mmec-bytevector-valid-past-slot-index-p bv (1- len)))
		       (should      (mmec-bytevector-valid-past-slot-index-p bv len))
		       (should (not (mmec-bytevector-valid-past-slot-index-p bv (1+ len))))
		       )))))
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


;;;; bytevector inspection functions

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME	(mmec-sformat "mmec-%s-bytevector-last-slot-index" TYPESTEM))
		       (BVTYPE		(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (DOCSTRING	(format "Test retrieval of the last slot index of a `mmec-%s-bytevector' object." TYPESTEM)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     (should (equal  0 (mmec-bytevector-last-slot-index (,BVTYPE 1))))
		     (should (equal 12 (mmec-bytevector-last-slot-index (,BVTYPE 13))))
		     (should (condition-case exc
				 (mmec-bytevector-last-slot-index (,BVTYPE 0))
			       (mmec-error-bytevector-is-empty	t)
			       (t				nil)))))))
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


;;;; printing

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME		(mmec-sformat "mmec-%s-print-test" TYPESTEM))
		       (BVTYPE			(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (NUMTYPE			(mmec-sformat "mmec-%s" TYPESTEM))
		       (DOCSTRING		(format "Test printing bytevectors of type `%s'." NUMTYPE))
		       (RESULT			(format "#s(%s 0 1 2 3 4 5)" BVTYPE)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     (let ((bv (,BVTYPE 6)))
		       (cl-loop for i from 0 to (mmec-bytevector-last-slot-index bv)
				do (mmec-bytevector-set bv i (,NUMTYPE i)))
		       (should (equal ,RESULT (cl-prin1-to-string bv))))))))
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
  (mmec--def ssize)
  (mmec--def usize)
  (mmec--def sintmax)
  (mmec--def uintmax)
  (mmec--def ptrdiff)
  (mmec--def sint8)
  (mmec--def uint8)
  (mmec--def sint16)
  (mmec--def uint16)
  (mmec--def sint32)
  (mmec--def uint32)
  (mmec--def sint64)
  (mmec--def uint64))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME		(mmec-sformat "mmec-%s-print-test" TYPESTEM))
		       (BVTYPE			(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (NUMTYPE			(mmec-sformat "mmec-%s" TYPESTEM))
		       (DOCSTRING		(format "Test printing bytevectors of type `%s'." NUMTYPE))
		       (RESULT			(format "#s(%s 0 1 2 3 4 5)" BVTYPE)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     (let ((bv (,BVTYPE 6)))
		       (cl-loop for i from 0 to (mmec-bytevector-last-slot-index bv)
				do (mmec-bytevector-set bv i (,NUMTYPE i)))
		       (should (equal ,RESULT (cl-prin1-to-string bv))))))))
  (mmec--def float)
  (mmec--def double)
  (mmec--def ldouble))


;;;; conversion to/from list

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME	(mmec-sformat "mmec-%s-bytevector-to/from-list" TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (BVTYPE		(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (DOCSTRING	(format "Test conversion of `mmec-%s-bytevector' objects to/from list." TYPESTEM))
		       (BVTOLIST	(mmec-sformat "mmec-bytevector-to-list" TYPESTEM))
		       (BVFROMLIST	(mmec-sformat "mmec-%s-bytevector-from-list" TYPESTEM)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     ;;Convert a bytevector to a list.
		     (should (let ((bv (,BVTYPE 6)))
			       (cl-loop for i from 0 to (mmec-bytevector-last-slot-index bv)
					do (mmec-bytevector-set bv i (,NUMTYPE i)))
			       (cl-loop for i in '(0 1 2 3 4 5)
					for j in (,BVTOLIST bv)
					always (mmec= (,NUMTYPE i) j))))
		     ;;Build a bytevector from a list.
		     (should (mmec-bytevector-equal (let ((result.bv (,BVTYPE 6)))
						      (cl-loop for i from 0 to (mmec-bytevector-last-slot-index result.bv)
				   			       do (mmec-bytevector-set result.bv i (,NUMTYPE i)))
						      result.bv)
						    (,BVFROMLIST '(0 1 2 3 4 5))))
		     (when nil
		       (mmec-debug-print (quote ,TYPESTEM) (,BVTOLIST (,BVFROMLIST '(0 1 2 3 4 5)))))))))
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


;;;; conversion to/from vector

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME	(mmec-sformat "mmec-%s-bytevector-to/from-vector" TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (BVTYPE		(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (DOCSTRING	(format "Test conversion of `mmec-%s-bytevector' objects to/from vector." TYPESTEM))
		       (BVTOVECTOR	(mmec-sformat "mmec-bytevector-to-vector" TYPESTEM))
		       (BVFROMVECTOR	(mmec-sformat "mmec-%s-bytevector-from-vector" TYPESTEM)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     ;;Convert a bytevector to a vector.
		     (should (let ((bv (,BVTYPE 6)))
			       (cl-loop for i from 0 to (mmec-bytevector-last-slot-index bv)
					do (mmec-bytevector-set bv i (,NUMTYPE i)))
			       (cl-loop for i in '[0 1 2 3 4 5]
					for j in (,BVTOVECTOR bv)
					always (mmec= (,NUMTYPE i) j))))
		     ;;Build a bytevector from a vector.
		     (should (mmec-bytevector-equal (let ((result.bv (,BVTYPE 6)))
						      (cl-loop for i from 0 to (mmec-bytevector-last-slot-index result.bv)
				   			       do (mmec-bytevector-set result.bv i (,NUMTYPE i)))
						      result.bv)
						    (,BVFROMVECTOR '[0 1 2 3 4 5])))
		     (when nil
		       (mmec-debug-print (quote ,TYPESTEM) (,BVTOVECTOR (,BVFROMVECTOR '[0 1 2 3 4 5]))))))))
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


;;;; comparison functions

(cl-macrolet
    ((mmec--compare	(TYPESTEM RV VEC1 VEC2)
			(let* ((FUNC		(mmec-sformat "mmec-bytevector-compare" TYPESTEM))
			       (BVFROMVECTOR	(mmec-sformat "mmec-%s-bytevector-from-vector" TYPESTEM)))
			  `(should (equal ,RV (,FUNC (,BVFROMVECTOR ,VEC1) (,BVFROMVECTOR ,VEC2))))))

     (mmec--equal	(TYPESTEM RV VEC1 VEC2)
			(let* ((FUNC		(mmec-sformat "mmec-bytevector-equal" TYPESTEM))
			       (BVFROMVECTOR	(mmec-sformat "mmec-%s-bytevector-from-vector" TYPESTEM)))
			  `(should (equal ,RV (,FUNC (,BVFROMVECTOR ,VEC1) (,BVFROMVECTOR ,VEC2))))))

     (mmec--less	(TYPESTEM RV VEC1 VEC2)
			(let* ((FUNC		(mmec-sformat "mmec-bytevector-less" TYPESTEM))
			       (BVFROMVECTOR	(mmec-sformat "mmec-%s-bytevector-from-vector" TYPESTEM)))
			  `(should (equal ,RV (,FUNC (,BVFROMVECTOR ,VEC1) (,BVFROMVECTOR ,VEC2))))))

     (mmec--greater	(TYPESTEM RV VEC1 VEC2)
			(let* ((FUNC		(mmec-sformat "mmec-bytevector-greater" TYPESTEM))
			       (BVFROMVECTOR	(mmec-sformat "mmec-%s-bytevector-from-vector" TYPESTEM)))
			  `(should (equal ,RV (,FUNC (,BVFROMVECTOR ,VEC1) (,BVFROMVECTOR ,VEC2))))))

     (mmec--leq		(TYPESTEM RV VEC1 VEC2)
			(let* ((FUNC		(mmec-sformat "mmec-bytevector-leq" TYPESTEM))
			       (BVFROMVECTOR	(mmec-sformat "mmec-%s-bytevector-from-vector" TYPESTEM)))
			  `(should (equal ,RV (,FUNC (,BVFROMVECTOR ,VEC1) (,BVFROMVECTOR ,VEC2))))))

     (mmec--geq		(TYPESTEM RV VEC1 VEC2)
			(let* ((FUNC		(mmec-sformat "mmec-bytevector-geq" TYPESTEM))
			       (BVFROMVECTOR	(mmec-sformat "mmec-%s-bytevector-from-vector" TYPESTEM)))
			  `(should (equal ,RV (,FUNC (,BVFROMVECTOR ,VEC1) (,BVFROMVECTOR ,VEC2))))))

     (mmec--def		(TYPESTEM)
			(let* ((TSTNAME-COMPARE	(mmec-sformat "mmec-%s-bytevector-comparison/compare" TYPESTEM))
			       (TESTNAME-EQUAL	(mmec-sformat "mmec-%s-bytevector-comparison/equal" TYPESTEM))
			       (TESTNAME-LESS	(mmec-sformat "mmec-%s-bytevector-comparison/less" TYPESTEM))
			       (TSTNAME-GREATER	(mmec-sformat "mmec-%s-bytevector-comparison/greater" TYPESTEM))
			       (TESTNAME-LEQ	(mmec-sformat "mmec-%s-bytevector-comparison/leq" TYPESTEM))
			       (TESTNAME-GEQ	(mmec-sformat "mmec-%s-bytevector-comparison/geq" TYPESTEM))
			       (DOCSTRING	(format "Test comparison between `mmec-%s-bytevector' objects." TYPESTEM)))
			  `(progn
			     (ert-deftest ,TSTNAME-COMPARE ()
			       ,DOCSTRING
			       (mmec--compare	,TYPESTEM  0 [0 1 2] [0 1 2])
			       (mmec--compare	,TYPESTEM -1 [0 1]   [0 1 2]) ;shorter first
			       (mmec--compare	,TYPESTEM +1 [0 1 2] [0 1])   ;shorter second
			       (mmec--compare	,TYPESTEM -1 [0 1 2] [0 9 2])
			       (mmec--compare	,TYPESTEM +1 [0 9 2] [0 1 2]))
			     (ert-deftest ,TESTNAME-EQUAL ()
			       ,DOCSTRING
			       (mmec--equal	,TYPESTEM   t [0 1 2] [0 1 2])
			       (mmec--equal	,TYPESTEM nil [0 1]   [0 1 2]) ;shorter first
			       (mmec--equal	,TYPESTEM nil [0 1 2] [0 1])   ;shorter second
			       (mmec--equal	,TYPESTEM nil [0 1 2] [0 9 2])
			       (mmec--equal	,TYPESTEM nil [0 9 2] [0 1 2]))
			     (ert-deftest ,TESTNAME-LESS ()
			       ,DOCSTRING
			       (mmec--less	,TYPESTEM nil [0 1 2] [0 1 2])
			       (mmec--less	,TYPESTEM   t [0 1]   [0 1 2]) ;shorter first
			       (mmec--less	,TYPESTEM nil [0 1 2] [0 1])   ;shorter second
			       (mmec--less	,TYPESTEM   t [0 1 2] [0 9 2])
			       (mmec--less	,TYPESTEM nil [0 9 2] [0 1 2]))
			     (ert-deftest ,TSTNAME-GREATER ()
			       ,DOCSTRING
			       (mmec--greater	,TYPESTEM nil [0 1 2] [0 1 2])
			       (mmec--greater	,TYPESTEM nil [0 1]   [0 1 2]) ;shorter first
			       (mmec--greater	,TYPESTEM   t [0 1 2] [0 1])   ;shorter second
			       (mmec--greater	,TYPESTEM nil [0 1 2] [0 9 2])
			       (mmec--greater	,TYPESTEM   t [0 9 2] [0 1 2]))
			     (ert-deftest ,TESTNAME-LEQ ()
			       ,DOCSTRING
			       (mmec--leq	,TYPESTEM   t [0 1 2] [0 1 2])
			       (mmec--leq	,TYPESTEM   t [0 1]   [0 1 2]) ;shorter first
			       (mmec--leq	,TYPESTEM nil [0 1 2] [0 1])   ;shorter second
			       (mmec--leq	,TYPESTEM   t [0 1 2] [0 9 2])
			       (mmec--leq	,TYPESTEM nil [0 9 2] [0 1 2]))
			     (ert-deftest ,TESTNAME-GEQ ()
			       ,DOCSTRING
			       (mmec--geq	,TYPESTEM   t [0 1 2] [0 1 2])
			       (mmec--geq	,TYPESTEM nil [0 1]   [0 1 2]) ;shorter first
			       (mmec--geq	,TYPESTEM   t [0 1 2] [0 1])   ;shorter second
			       (mmec--geq	,TYPESTEM nil [0 1 2] [0 9 2])
			       (mmec--geq	,TYPESTEM   t [0 9 2] [0 1 2])))
			  )))
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


;;;; subbytevectors

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME	(mmec-sformat "mmec-%s-subbytevector" TYPESTEM))
		       (BVTYPE		(mmec-sformat "mmec-%s-bytevector" TYPESTEM))
		       (DOCSTRING	(format "Extract subsequences from a `%s' object." BVTYPE))
		       (BVFROMVECTOR	(mmec-sformat "mmec-%s-bytevector-from-vector" TYPESTEM)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     (should (mmec-bytevector-equal
		     	      (mmec-subbytevector (,BVFROMVECTOR [1 2 3 4 5 6]))
		     	      (,BVFROMVECTOR [1 2 3 4 5 6])))
		     (should (mmec-bytevector-equal
		     	      (let ((bv		(,BVFROMVECTOR [1 2 3 4 5 6]))
		     		    (start	0)
		     		    (past	6))
		     		(let ((sbv (mmec-subbytevector bv :start start :past past)))
		     		  ;;(cl-prin1 sbv)
		     		  sbv))
		     	      (,BVFROMVECTOR [1 2 3 4 5 6])))
		     (should (mmec-bytevector-equal
		     	      (let ((bv		(,BVFROMVECTOR [1 2 3 4 5 6]))
		     		    (start	2)
		     		    (past	5))
		     		(let ((sbv (mmec-subbytevector bv :start start :past past)))
		     		  ;;(cl-prin1 sbv)
		     		  sbv))
		     	      (,BVFROMVECTOR [3 4 5])))
		     (should (mmec-bytevector-equal
		     	      (let ((bv		(,BVFROMVECTOR [1 2 3 4 5 6]))
		     		    (start	3)
		     		    (past	3))
		     		(let ((sbv (mmec-subbytevector bv :start start :past past)))
		     		  ;;(cl-prin1 sbv)
		     		  sbv))
		     	      (,BVFROMVECTOR [])))
		     (should (condition-case exc
				 (let ((bv	(,BVFROMVECTOR [1 2 3 4 5 6]))
		     		       (start	-1)
		     		       (past	3))
				   (mmec-subbytevector bv :start start :past past))
			       (mmec-error-bytevector-span-start-out-of-range	t)
			       (t						nil)))
		     (should (condition-case exc
				 (let ((bv	(,BVFROMVECTOR [1 2 3 4 5 6]))
		     		       (start	1)
		     		       (past	99))
				   (mmec-subbytevector bv :start start :past past))
			       (mmec-error-bytevector-span-past-out-of-range	t)
			       (t						nil)))
		     (should (condition-case exc
		     		 (let ((bv	(,BVFROMVECTOR [1 2 3 4 5 6]))
		     		       (start	4)
		     		       (past	3))
		     		   (mmec-subbytevector bv :start start :past past))
		     	       (mmec-error-bytevector-invalid-span-limits	t)
		     	       (t						nil)))
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


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; bytevector-objects.el ends here
