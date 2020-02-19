;;; user-pointer-objects.el --- dynamic module test

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


;;;; number objects makers: mmec-char

(ert-deftest mmec-number-char ()
  "Build a `mmec-char' object."
  (should	(mmec-fits-char-p	123))
  (should (not	(mmec-fits-char-p	123000)))
  ;;
  (should	(mmec-char-p	(mmec-char 123)))
  (should	(mmec-char-p	(mmec-char 123.0)))
  (should	(mmec-char-p	(mmec-char (mmec-char		123))))
  (should	(mmec-char-p	(mmec-char (mmec-schar		123))))
  (should	(mmec-char-p	(mmec-char (mmec-sshrt		123))))
  (should	(mmec-char-p	(mmec-char (mmec-sint		123))))
  (should	(mmec-char-p	(mmec-char (mmec-slong		123))))
  (should	(mmec-char-p	(mmec-char (mmec-sllong		123))))
  (should	(mmec-char-p	(mmec-char (mmec-sintmax	123))))
  (should	(mmec-char-p	(mmec-char (mmec-ssize		123))))
  (should	(mmec-char-p	(mmec-char (mmec-ptrdiff	123))))
  ;;
  (my--unsupported-type-error mmec-char mmec-uchar	123)
  (my--unsupported-type-error mmec-char mmec-wchar	123)
  (my--unsupported-type-error mmec-char mmec-ushrt	123)
  (my--unsupported-type-error mmec-char mmec-uint	123)
  (my--unsupported-type-error mmec-char mmec-ulong	123)
  (my--unsupported-type-error mmec-char mmec-ullong	123)
  (my--unsupported-type-error mmec-char mmec-uintmax	123)
  (my--unsupported-type-error mmec-char mmec-usize	123)
  ;;
  (my--init-argument-does-not-fit mmec-char 1230)
  nil)

;;; --------------------------------------------------------------------

(ert-deftest mmec-number-float ()
  "Build a `mmec-float' object."
  (should	(mmec-fits-float-p	123))
  (should	(mmec-fits-float-p	12.3))
  ;;
  (should	(mmec-float-p	(mmec-float 123)))
  (should	(mmec-float-p	(mmec-float 12.3)))
  ;;
  (should	(mmec-float-p	(mmec-float (mmec-char		123))))
  (should	(mmec-float-p	(mmec-float (mmec-schar		123))))
  (should	(mmec-float-p	(mmec-float (mmec-uchar		123))))
  (should	(mmec-float-p	(mmec-float (mmec-sshrt		123))))
  (should	(mmec-float-p	(mmec-float (mmec-ushrt		123))))
  (should	(mmec-float-p	(mmec-float (mmec-sint		123))))
  (should	(mmec-float-p	(mmec-float (mmec-uint		123))))
  (should	(mmec-float-p	(mmec-float (mmec-slong		123))))
  (should	(mmec-float-p	(mmec-float (mmec-ulong		123))))
  (should	(mmec-float-p	(mmec-float (mmec-sllong	123))))
  (should	(mmec-float-p	(mmec-float (mmec-ullong	123))))
  (should	(mmec-float-p	(mmec-float (mmec-sintmax	123))))
  (should	(mmec-float-p	(mmec-float (mmec-uintmax	123))))
  (should	(mmec-float-p	(mmec-float (mmec-ssize		123))))
  (should	(mmec-float-p	(mmec-float (mmec-usize		123))))
  (should	(mmec-float-p	(mmec-float (mmec-ptrdiff	123))))
  ;;
  (my--init-argument-does-not-fit mmec-float 12.30)
  t)


;;;; bytevector makers

(ert-deftest mmec-uint8-bytevector ()
  "Build a `mmec-uint8-bytevector' object."
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


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; test.el ends here
