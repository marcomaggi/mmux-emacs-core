;;; number-objects.el --- dynamic module test

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


;;;; equality tests

(defmacro mmux-core-test--equality--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality."))
	 (MAKER		(if (eq TYPE 'integer)
			    'identity
			  TYPE)))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should	(mmec= (,MAKER 1) (,MAKER 1)))
	 (should (not	(mmec= (,MAKER 5) (,MAKER 17))))
	 ;; Compare with a signed integer.
	 (should	(mmec= (,MAKER 1) (mmec-sint 1)))
	 (should (not	(mmec= (,MAKER 5) (mmec-sint 17))))
	 (should	(mmec= (mmec-sint 1) (,MAKER 1)))
	 (should (not	(mmec= (mmec-sint 5) (,MAKER 17))))
	 ))))

(defmacro mmux-core-test--equality--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should	(mmec= (,TYPE 1) (,TYPE 1)))
	 (should (not	(mmec= (,TYPE 5) (,TYPE 17))))
	 ;; Compare with an unsigned integer.
	 (should	(mmec= (,TYPE 1) (mmec-uint 1)))
	 (should (not	(mmec= (,TYPE 5) (mmec-uint 17))))
	 (should	(mmec= (mmec-uint 1) (,TYPE 1)))
	 (should (not	(mmec= (mmec-uint 5) (,TYPE 17))))
	 ))))

(defmacro mmux-core-test--floating-point--equal-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality."))a)
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should	(mmec= (,TYPE 1.0) (,TYPE 1.0)))
	 (should (not	(mmec= (,TYPE 5.0) (,TYPE 17.0))))
	 ;; Compare with a `float'.
	 (should	(mmec= (,TYPE 1.0) 1.0))
	 (should (not	(mmec= (,TYPE 5.0) 17.0)))
	 (should	(mmec= 1.0 (,TYPE 1.0)))
	 (should (not	(mmec= 5.0 (,TYPE 17.0))))
	 ;; Compare with a `mmec-float'.
	 (should	(mmec= (,TYPE 1.0) (mmec-float 1.0)))
	 (should (not	(mmec= (,TYPE 5.0) (mmec-float 17.0))))
	 (should	(mmec= (mmec-float 1.0) (,TYPE 1.0)))
	 (should (not	(mmec= (mmec-float 5.0) (,TYPE 17.0))))
	 ;; Compare with a `mmec-ldouble'.
	 (should	(mmec= (,TYPE 1.0) (mmec-ldouble 1.0)))
	 (should (not	(mmec= (,TYPE 5.0) (mmec-ldouble 17.0))))
	 (should	(mmec= (mmec-ldouble 1.0) (,TYPE 1.0)))
	 (should (not	(mmec= (mmec-ldouble 5.0) (,TYPE 17.0))))
	 ))))

(mmux-core-test--equality--integers-signed	integer)
(mmux-core-test--equality--integers-signed	mmec-char)
(mmux-core-test--equality--integers-unsigned	mmec-uchar)
(mmux-core-test--equality--integers-signed	mmec-schar)
(mmux-core-test--equality--integers-unsigned	mmec-wchar)
(mmux-core-test--equality--integers-signed	mmec-sshrt)
(mmux-core-test--equality--integers-unsigned	mmec-ushrt)
(mmux-core-test--equality--integers-signed	mmec-sint)
(mmux-core-test--equality--integers-unsigned	mmec-uint)
(mmux-core-test--equality--integers-signed	mmec-slong)
(mmux-core-test--equality--integers-unsigned	mmec-ulong)
(mmux-core-test--equality--integers-signed	mmec-sllong)
(mmux-core-test--equality--integers-unsigned	mmec-ullong)
(mmux-core-test--equality--integers-signed	mmec-sint8)
(mmux-core-test--equality--integers-unsigned	mmec-uint8)
(mmux-core-test--equality--integers-signed	mmec-sint16)
(mmux-core-test--equality--integers-unsigned	mmec-uint16)
(mmux-core-test--equality--integers-signed	mmec-sint32)
(mmux-core-test--equality--integers-unsigned	mmec-uint32)
(mmux-core-test--equality--integers-signed	mmec-sint64)
(mmux-core-test--equality--integers-unsigned	mmec-uint64)
(mmux-core-test--equality--integers-signed	mmec-ssize)
(mmux-core-test--equality--integers-unsigned	mmec-usize)
(mmux-core-test--equality--integers-signed	mmec-sintmax)
(mmux-core-test--equality--integers-unsigned	mmec-uintmax)
(mmux-core-test--equality--integers-signed	mmec-ptrdiff)

(mmux-core-test--floating-point--equal-tests	float)
(mmux-core-test--floating-point--equal-tests	mmec-float)
(mmux-core-test--floating-point--equal-tests	mmec-double)
(mmux-core-test--floating-point--equal-tests	mmec-ldouble)


;;;; not-equality tests

(defmacro mmux-core-test--not-equal--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "not-equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for not-equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not	(mmec/= (,TYPE 1) (,TYPE 1))))
	 (should 	(mmec/= (,TYPE 5) (,TYPE 17)))
	 ;; Compare with a signed integer.
	 (should (not	(mmec/= (,TYPE 1) (mmec-sint 1))))
	 (should 	(mmec/= (,TYPE 5) (mmec-sint 17)))
	 (should (not	(mmec/= (mmec-sint 1) (,TYPE 1))))
	 (should 	(mmec/= (mmec-sint 5) (,TYPE 17)))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
	 ;; (should (not	(mmec/= (,TYPE 1) (mmec-uint 1))))
	 ;; (should 	(mmec/= (,TYPE 5) (mmec-uint 17)))
	 ;; (should (not	(mmec/= (mmec-uint 1) (,TYPE 1))))
	 ;; (should 	(mmec/= (mmec-uint 5) (,TYPE 17)))
	 ))))

(defmacro mmux-core-test--not-equal--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "not-equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for not-equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not	(mmec/= (,TYPE 1) (,TYPE 1))))
	 (should 	(mmec/= (,TYPE 5) (,TYPE 17)))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
	 ;; (should (not	(mmec/= (,TYPE 1) (mmec-sint 1))))
	 ;; (should 	(mmec/= (,TYPE 5) (mmec-sint 17)))
	 ;; (should (not	(mmec/= (mmec-sint 1) (,TYPE 1))))
	 ;; (should 	(mmec/= (mmec-sint 5) (,TYPE 17)))
	 ;; Compare with an unsigned integer.
	 (should (not	(mmec/= (,TYPE 1) (mmec-uint 1))))
	 (should 	(mmec/= (,TYPE 5) (mmec-uint 17)))
	 (should (not	(mmec/= (mmec-uint 1) (,TYPE 1))))
	 (should 	(mmec/= (mmec-uint 5) (,TYPE 17)))
	 ))))

(defmacro mmux-core-test--floating-point--not-equal-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for not-equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not	(mmec/= (,TYPE 1.0) (,TYPE 1.0))))
	 (should 	(mmec/= (,TYPE 5.0) (,TYPE 17.0)))
	 ;; Compare with a float.
	 (should (not	(mmec/= (,TYPE 1.0) 1.0)))
	 (should 	(mmec/= (,TYPE 5.0) 17.0))
	 (should (not	(mmec/= 1.0 (,TYPE 1.0))))
	 (should 	(mmec/= 5.0 (,TYPE 17.0)))
	 ;; Compare with a mmec-float.
	 (should (not	(mmec/= (,TYPE 1.0) (mmec-float 1.0))))
	 (should 	(mmec/= (,TYPE 5.0) (mmec-float 17.0)))
	 (should (not	(mmec/= (mmec-float 1.0) (,TYPE 1.0))))
	 (should 	(mmec/= (mmec-float 5.0) (,TYPE 17.0)))
	 ;; Compare with a mmec-ldouble.
	 (should (not	(mmec/= (,TYPE 1.0) (mmec-ldouble 1.0))))
	 (should 	(mmec/= (,TYPE 5.0) (mmec-ldouble 17.0)))
	 (should (not	(mmec/= (mmec-ldouble 1.0) (,TYPE 1.0))))
	 (should 	(mmec/= (mmec-ldouble 5.0) (,TYPE 17.0)))
	 ))))

(ert-deftest not-equality-integer ()
  "Compare objects of type `integer' for not-equality."
  (should (not	(mmec/= 1 1)))
  (should 	(mmec/= 1 2)))

(ert-deftest not-equality-float ()
  "Compare objects of type `float' for not-equality."
  (should (not	(mmec/= 1.0 1.0)))
  (should 	(mmec/= 1.0 2.0)))

(mmux-core-test--not-equal--integers-signed	mmec-char)
(mmux-core-test--not-equal--integers-unsigned	mmec-uchar)
(mmux-core-test--not-equal--integers-signed	mmec-schar)
(mmux-core-test--not-equal--integers-unsigned	mmec-wchar)
(mmux-core-test--not-equal--integers-signed	mmec-sshrt)
(mmux-core-test--not-equal--integers-unsigned	mmec-ushrt)
(mmux-core-test--not-equal--integers-signed	mmec-sint)
(mmux-core-test--not-equal--integers-unsigned	mmec-uint)
(mmux-core-test--not-equal--integers-signed	mmec-slong)
(mmux-core-test--not-equal--integers-unsigned	mmec-ulong)
(mmux-core-test--not-equal--integers-signed	mmec-sllong)
(mmux-core-test--not-equal--integers-unsigned	mmec-ullong)
(mmux-core-test--not-equal--integers-signed	mmec-sint8)
(mmux-core-test--not-equal--integers-unsigned	mmec-uint8)
(mmux-core-test--not-equal--integers-signed	mmec-sint16)
(mmux-core-test--not-equal--integers-unsigned	mmec-uint16)
(mmux-core-test--not-equal--integers-signed	mmec-sint32)
(mmux-core-test--not-equal--integers-unsigned	mmec-uint32)
(mmux-core-test--not-equal--integers-signed	mmec-sint64)
(mmux-core-test--not-equal--integers-unsigned	mmec-uint64)
(mmux-core-test--not-equal--integers-signed	mmec-ssize)
(mmux-core-test--not-equal--integers-unsigned	mmec-usize)
(mmux-core-test--not-equal--integers-signed	mmec-sintmax)
(mmux-core-test--not-equal--integers-unsigned	mmec-uintmax)
(mmux-core-test--not-equal--integers-signed	mmec-ptrdiff)

(mmux-core-test--floating-point--not-equal-tests float)
(mmux-core-test--floating-point--not-equal-tests mmec-float)
(mmux-core-test--floating-point--not-equal-tests mmec-ldouble)


;;;; less-than tests

(defmacro mmux-core-test--less-than--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec< (,TYPE 1) (,TYPE 2)))
	 (should (not	(mmec< (,TYPE 1) (,TYPE 1))))
	 (should (not	(mmec< (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 (should 	(mmec< (,TYPE 1) (mmec-sint 2)))
	 (should (not	(mmec< (,TYPE 1) (mmec-sint 1))))
	 (should (not	(mmec< (,TYPE 2) (mmec-sint 1))))
	 (should 	(mmec< (mmec-sint 1) (,TYPE 2)))
	 (should (not	(mmec< (mmec-sint 1) (,TYPE 1))))
	 (should (not	(mmec< (mmec-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (mmec< (,TYPE 1) (mmec-uint 2)))
         ;; (should (not   (mmec< (,TYPE 1) (mmec-uint 1))))
         ;; (should (not   (mmec< (,TYPE 2) (mmec-uint 1))))
         ;; (should        (mmec< (mmec-uint 1) (,TYPE 2)))
         ;; (should (not   (mmec< (mmec-uint 1) (,TYPE 1))))
         ;; (should (not   (mmec< (mmec-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--less-than--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec< (,TYPE 1) (,TYPE 2)))
	 (should (not	(mmec< (,TYPE 1) (,TYPE 1))))
	 (should (not	(mmec< (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (mmec< (,TYPE 1) (mmec-sint 2)))
         ;; (should (not   (mmec< (,TYPE 1) (mmec-sint 1))))
         ;; (should (not   (mmec< (,TYPE 2) (mmec-sint 1))))
         ;; (should        (mmec< (mmec-sint 1) (,TYPE 2)))
         ;; (should (not   (mmec< (mmec-sint 1) (,TYPE 1))))
         ;; (should (not   (mmec< (mmec-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 (should 	(mmec< (,TYPE 1) (mmec-uint 2)))
	 (should (not	(mmec< (,TYPE 1) (mmec-uint 1))))
	 (should (not	(mmec< (,TYPE 2) (mmec-uint 1))))
	 (should 	(mmec< (mmec-uint 1) (,TYPE 2)))
	 (should (not	(mmec< (mmec-uint 1) (,TYPE 1))))
	 (should (not	(mmec< (mmec-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--floating-point--less-than-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec< (,TYPE 1.0) (,TYPE 2.0)))
	 (should (not	(mmec< (,TYPE 1.0) (,TYPE 1.0))))
	 (should (not	(mmec< (,TYPE 2.0) (,TYPE 1.0))))
	 ;; Compare with a float.
	 (should 	(mmec< (,TYPE 1.0) 2.0))
	 (should (not	(mmec< (,TYPE 1.0) 1.0)))
	 (should (not	(mmec< (,TYPE 2.0) 1.0)))
	 (should 	(mmec< 1.0 (,TYPE 2.0)))
	 (should (not	(mmec< 1.0 (,TYPE 1.0))))
	 (should (not	(mmec< 2.0 (,TYPE 1.0))))
	 ;; Compare with a mmec-float.
	 (should 	(mmec< (,TYPE 1.0) (mmec-float 2.0)))
	 (should (not	(mmec< (,TYPE 1.0) (mmec-float 1.0))))
	 (should (not	(mmec< (,TYPE 2.0) (mmec-float 1.0))))
	 (should 	(mmec< (mmec-float 1.0) (,TYPE 2.0)))
	 (should (not	(mmec< (mmec-float 1.0) (,TYPE 1.0))))
	 (should (not	(mmec< (mmec-float 2.0) (,TYPE 1.0))))
	 ;; Compare with a mmec-ldouble.
	 (should 	(mmec< (,TYPE 1.0) (mmec-ldouble 2.0)))
	 (should (not	(mmec< (,TYPE 1.0) (mmec-ldouble 1.0))))
	 (should (not	(mmec< (,TYPE 2.0) (mmec-ldouble 1.0))))
	 (should 	(mmec< (mmec-ldouble 1.0) (,TYPE 2.0)))
	 (should (not	(mmec< (mmec-ldouble 1.0) (,TYPE 1.0))))
	 (should (not	(mmec< (mmec-ldouble 2.0) (,TYPE 1.0))))
	 ))))

(ert-deftest less-than-integer ()
  "Compare objects of type `integer' for equality."
  (should 	(mmec< 1 2))
  (should (not	(mmec< 1 1)))
  (should (not	(mmec< 2 1))))

(ert-deftest less-than-float ()
  "Compare objects of type `float' for equality."
  (should 	(mmec< 1.0 2.0))
  (should (not	(mmec< 1.0 1.0)))
  (should (not	(mmec< 2.0 1.0))))

(mmux-core-test--less-than--integers-signed	mmec-char)
(mmux-core-test--less-than--integers-unsigned	mmec-uchar)
(mmux-core-test--less-than--integers-signed	mmec-schar)
(mmux-core-test--less-than--integers-unsigned	mmec-wchar)
(mmux-core-test--less-than--integers-signed	mmec-sshrt)
(mmux-core-test--less-than--integers-unsigned	mmec-ushrt)
(mmux-core-test--less-than--integers-signed	mmec-sint)
(mmux-core-test--less-than--integers-unsigned	mmec-uint)
(mmux-core-test--less-than--integers-signed	mmec-slong)
(mmux-core-test--less-than--integers-unsigned	mmec-ulong)
(mmux-core-test--less-than--integers-signed	mmec-sllong)
(mmux-core-test--less-than--integers-unsigned	mmec-ullong)
(mmux-core-test--less-than--integers-signed	mmec-sint8)
(mmux-core-test--less-than--integers-unsigned	mmec-uint8)
(mmux-core-test--less-than--integers-signed	mmec-sint16)
(mmux-core-test--less-than--integers-unsigned	mmec-uint16)
(mmux-core-test--less-than--integers-signed	mmec-sint32)
(mmux-core-test--less-than--integers-unsigned	mmec-uint32)
(mmux-core-test--less-than--integers-signed	mmec-sint64)
(mmux-core-test--less-than--integers-unsigned	mmec-uint64)
(mmux-core-test--less-than--integers-signed	mmec-ssize)
(mmux-core-test--less-than--integers-unsigned	mmec-usize)
(mmux-core-test--less-than--integers-signed	mmec-sintmax)
(mmux-core-test--less-than--integers-unsigned	mmec-uintmax)
(mmux-core-test--less-than--integers-signed	mmec-ptrdiff)

(mmux-core-test--floating-point--less-than-tests float)
(mmux-core-test--floating-point--less-than-tests mmec-float)
(mmux-core-test--floating-point--less-than-tests mmec-ldouble)


;;;; greater-than tests

(defmacro mmux-core-test--greater-than--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec> (,TYPE 1) (,TYPE 2))))
	 (should (not	(mmec> (,TYPE 1) (,TYPE 1))))
	 (should 	(mmec> (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
	 (should (not 	(mmec> (,TYPE 1) (mmec-sint 2))))
	 (should (not	(mmec> (,TYPE 1) (mmec-sint 1))))
	 (should 	(mmec> (,TYPE 2) (mmec-sint 1)))
	 (should (not	(mmec> (mmec-sint 1) (,TYPE 2))))
	 (should (not	(mmec> (mmec-sint 1) (,TYPE 1))))
	 (should 	(mmec> (mmec-sint 2) (,TYPE 1)))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should (not   (mmec> (,TYPE 1) (mmec-uint 2))))
         ;; (should (not   (mmec> (,TYPE 1) (mmec-uint 1))))
         ;; (should        (mmec> (,TYPE 2) (mmec-uint 1)))
         ;; (should (not   (mmec> (mmec-uint 1) (,TYPE 2))))
         ;; (should (not   (mmec> (mmec-uint 1) (,TYPE 1))))
         ;; (should        (mmec> (mmec-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--greater-than--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec> (,TYPE 1) (,TYPE 2))))
	 (should (not	(mmec> (,TYPE 1) (,TYPE 1))))
	 (should 	(mmec> (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
         ;; (should (not   (mmec> (,TYPE 1) (mmec-sint 2))))
         ;; (should (not   (mmec> (,TYPE 1) (mmec-sint 1))))
         ;; (should        (mmec> (,TYPE 2) (mmec-sint 1)))
         ;; (should (not   (mmec> (mmec-sint 1) (,TYPE 2))))
         ;; (should (not   (mmec> (mmec-sint 1) (,TYPE 1))))
         ;; (should        (mmec> (mmec-sint 2) (,TYPE 1)))
	 ;;
	 ;; Compare with an unsigned integer.
	 ;;
	 (should (not	(mmec> (,TYPE 1) (mmec-uint 2))))
	 (should (not	(mmec> (,TYPE 1) (mmec-uint 1))))
	 (should	(mmec> (,TYPE 2) (mmec-uint 1)))
	 (should (not	(mmec> (mmec-uint 1) (,TYPE 2))))
	 (should (not	(mmec> (mmec-uint 1) (,TYPE 1))))
	 (should	(mmec> (mmec-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--floating-point--greater-than-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec> (,TYPE 1.0) (,TYPE 2.0))))
	 (should (not	(mmec> (,TYPE 1.0) (,TYPE 1.0))))
	 (should 	(mmec> (,TYPE 2.0) (,TYPE 1.0)))
	 ;; Compare with a float.
	 (should (not 	(mmec> (,TYPE 1.0) 2.0)))
	 (should (not	(mmec> (,TYPE 1.0) 1.0)))
	 (should 	(mmec> (,TYPE 2.0) 1.0))
	 (should (not 	(mmec> 1.0 (,TYPE 2.0))))
	 (should (not	(mmec> 1.0 (,TYPE 1.0))))
	 (should 	(mmec> 2.0 (,TYPE 1.0)))
	 ;; Compare with a mmec-float.
	 (should (not 	(mmec> (,TYPE 1.0) (mmec-float 2.0))))
	 (should (not	(mmec> (,TYPE 1.0) (mmec-float 1.0))))
	 (should 	(mmec> (,TYPE 2.0) (mmec-float 1.0)))
	 (should (not 	(mmec> (mmec-float 1.0) (,TYPE 2.0))))
	 (should (not	(mmec> (mmec-float 1.0) (,TYPE 1.0))))
	 (should 	(mmec> (mmec-float 2.0) (,TYPE 1.0)))
	 ;; Compare with a mmec-ldouble.
	 (should (not 	(mmec> (,TYPE 1.0) (mmec-ldouble 2.0))))
	 (should (not	(mmec> (,TYPE 1.0) (mmec-ldouble 1.0))))
	 (should 	(mmec> (,TYPE 2.0) (mmec-ldouble 1.0)))
	 (should (not 	(mmec> (mmec-ldouble 1.0) (,TYPE 2.0))))
	 (should (not	(mmec> (mmec-ldouble 1.0) (,TYPE 1.0))))
	 (should 	(mmec> (mmec-ldouble 2.0) (,TYPE 1.0)))
	 ))))

(ert-deftest greater-than-integer ()
  "Compare objects of type `integer' for equality."
  (should (not 	(mmec> 1 2)))
  (should (not	(mmec> 1 1)))
  (should 	(mmec> 2 1)))

(ert-deftest greater-than-float ()
  "Compare objects of type `float' for equality."
  (should (not 	(mmec> 1.0 2.0)))
  (should (not	(mmec> 1.0 1.0)))
  (should 	(mmec> 2.0 1.0)))

(mmux-core-test--greater-than--integers-signed		mmec-char)
(mmux-core-test--greater-than--integers-unsigned	mmec-uchar)
(mmux-core-test--greater-than--integers-signed		mmec-schar)
(mmux-core-test--greater-than--integers-unsigned	mmec-wchar)
(mmux-core-test--greater-than--integers-signed		mmec-sshrt)
(mmux-core-test--greater-than--integers-unsigned	mmec-ushrt)
(mmux-core-test--greater-than--integers-signed		mmec-sint)
(mmux-core-test--greater-than--integers-unsigned	mmec-uint)
(mmux-core-test--greater-than--integers-signed		mmec-slong)
(mmux-core-test--greater-than--integers-unsigned	mmec-ulong)
(mmux-core-test--greater-than--integers-signed		mmec-sllong)
(mmux-core-test--greater-than--integers-unsigned	mmec-ullong)
(mmux-core-test--greater-than--integers-signed		mmec-sint8)
(mmux-core-test--greater-than--integers-unsigned	mmec-uint8)
(mmux-core-test--greater-than--integers-signed		mmec-sint16)
(mmux-core-test--greater-than--integers-unsigned	mmec-uint16)
(mmux-core-test--greater-than--integers-signed		mmec-sint32)
(mmux-core-test--greater-than--integers-unsigned	mmec-uint32)
(mmux-core-test--greater-than--integers-signed		mmec-sint64)
(mmux-core-test--greater-than--integers-unsigned	mmec-uint64)
(mmux-core-test--greater-than--integers-signed		mmec-ssize)
(mmux-core-test--greater-than--integers-unsigned	mmec-usize)
(mmux-core-test--greater-than--integers-signed		mmec-sintmax)
(mmux-core-test--greater-than--integers-unsigned	mmec-uintmax)
(mmux-core-test--greater-than--integers-signed		mmec-ptrdiff)

(mmux-core-test--floating-point--greater-than-tests float)
(mmux-core-test--floating-point--greater-than-tests mmec-float)
(mmux-core-test--floating-point--greater-than-tests mmec-ldouble)


;;;; less-than or equal-to tests

(defmacro mmux-core-test--less-than-or-equal-to--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec<= (,TYPE 1) (,TYPE 2)))
	 (should 	(mmec<= (,TYPE 1) (,TYPE 1)))
	 (should (not	(mmec<= (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 (should 	(mmec<= (,TYPE 1) (mmec-sint 2)))
	 (should 	(mmec<= (,TYPE 1) (mmec-sint 1)))
	 (should (not	(mmec<= (,TYPE 2) (mmec-sint 1))))
	 (should 	(mmec<= (mmec-sint 1) (,TYPE 2)))
	 (should 	(mmec<= (mmec-sint 1) (,TYPE 1)))
	 (should (not	(mmec<= (mmec-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (mmec<= (,TYPE 1) (mmec-uint 2)))
         ;; (should        (mmec<= (,TYPE 1) (mmec-uint 1)))
         ;; (should (not   (mmec<= (,TYPE 2) (mmec-uint 1))))
         ;; (should        (mmec<= (mmec-uint 1) (,TYPE 2)))
         ;; (should        (mmec<= (mmec-uint 1) (,TYPE 1)))
         ;; (should (not   (mmec<= (mmec-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--less-than-or-equal-to--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec<= (,TYPE 1) (,TYPE 2)))
	 (should 	(mmec<= (,TYPE 1) (,TYPE 1)))
	 (should (not	(mmec<= (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (mmec<= (,TYPE 1) (mmec-sint 2)))
         ;; (should        (mmec<= (,TYPE 1) (mmec-sint 1)))
         ;; (should (not   (mmec<= (,TYPE 2) (mmec-sint 1))))
         ;; (should        (mmec<= (mmec-sint 1) (,TYPE 2)))
         ;; (should        (mmec<= (mmec-sint 1) (,TYPE 1)))
         ;; (should (not   (mmec<= (mmec-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 (should 	(mmec<= (,TYPE 1) (mmec-uint 2)))
	 (should 	(mmec<= (,TYPE 1) (mmec-uint 1)))
	 (should (not	(mmec<= (,TYPE 2) (mmec-uint 1))))
	 (should 	(mmec<= (mmec-uint 1) (,TYPE 2)))
	 (should 	(mmec<= (mmec-uint 1) (,TYPE 1)))
	 (should (not	(mmec<= (mmec-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--floating-point--less-than-or-equal-to-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec<= (,TYPE 1.0) (,TYPE 2.0)))
	 (should 	(mmec<= (,TYPE 1.0) (,TYPE 1.0)))
	 (should (not	(mmec<= (,TYPE 2.0) (,TYPE 1.0))))
	 ;; Compare with a float.
	 (should 	(mmec<= (,TYPE 1.0) 2.0))
	 (should 	(mmec<= (,TYPE 1.0) 1.0))
	 (should (not	(mmec<= (,TYPE 2.0) 1.0)))
	 (should 	(mmec<= 1.0 (,TYPE 2.0)))
	 (should 	(mmec<= 1.0 (,TYPE 1.0)))
	 (should (not	(mmec<= 2.0 (,TYPE 1.0))))
	 ;; Compare with a mmec-float.
	 (should 	(mmec<= (,TYPE 1.0) (mmec-float 2.0)))
	 (should 	(mmec<= (,TYPE 1.0) (mmec-float 1.0)))
	 (should (not	(mmec<= (,TYPE 2.0) (mmec-float 1.0))))
	 (should 	(mmec<= (mmec-float 1.0) (,TYPE 2.0)))
	 (should 	(mmec<= (mmec-float 1.0) (,TYPE 1.0)))
	 (should (not	(mmec<= (mmec-float 2.0) (,TYPE 1.0))))
	 ;; Compare with a mmec-ldouble.
	 (should 	(mmec<= (,TYPE 1.0) (mmec-ldouble 2.0)))
	 (should 	(mmec<= (,TYPE 1.0) (mmec-ldouble 1.0)))
	 (should (not	(mmec<= (,TYPE 2.0) (mmec-ldouble 1.0))))
	 (should 	(mmec<= (mmec-ldouble 1.0) (,TYPE 2.0)))
	 (should 	(mmec<= (mmec-ldouble 1.0) (,TYPE 1.0)))
	 (should (not	(mmec<= (mmec-ldouble 2.0) (,TYPE 1.0))))
	 ))))

(ert-deftest less-than-or-equal-to-integer ()
  "Compare objects of type `integer' for equality."
  (should 	(mmec<= 1 2))
  (should 	(mmec<= 1 1))
  (should (not	(mmec<= 2 1))))

(ert-deftest less-than-or-equal-to-float ()
  "Compare objects of type `float' for equality."
  (should 	(mmec<= 1.0 2.0))
  (should 	(mmec<= 1.0 1.0))
  (should (not	(mmec<= 2.0 1.0))))

(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-char)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uchar)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-schar)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-wchar)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sshrt)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-ushrt)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sint)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uint)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-slong)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-ulong)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sllong)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-ullong)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sint8)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uint8)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sint16)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uint16)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sint32)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uint32)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sint64)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uint64)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-ssize)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-usize)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sintmax)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uintmax)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-ptrdiff)

(mmux-core-test--floating-point--less-than-or-equal-to-tests float)
(mmux-core-test--floating-point--less-than-or-equal-to-tests mmec-float)
(mmux-core-test--floating-point--less-than-or-equal-to-tests mmec-ldouble)


;;;; greater-than or equal-to tests

(defmacro mmux-core-test--greater-than-or-equal-to--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec>= (,TYPE 1) (,TYPE 2))))
	 (should 	(mmec>= (,TYPE 1) (,TYPE 1)))
	 (should 	(mmec>= (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
	 (should (not 	(mmec>= (,TYPE 1) (mmec-sint 2))))
	 (should 	(mmec>= (,TYPE 1) (mmec-sint 1)))
	 (should 	(mmec>= (,TYPE 2) (mmec-sint 1)))
	 (should (not	(mmec>= (mmec-sint 1) (,TYPE 2))))
	 (should 	(mmec>= (mmec-sint 1) (,TYPE 1)))
	 (should 	(mmec>= (mmec-sint 2) (,TYPE 1)))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should (not   (mmec>= (,TYPE 1) (mmec-uint 2))))
         ;; (should        (mmec>= (,TYPE 1) (mmec-uint 1)))
         ;; (should        (mmec>= (,TYPE 2) (mmec-uint 1)))
         ;; (should (not   (mmec>= (mmec-uint 1) (,TYPE 2))))
         ;; (should        (mmec>= (mmec-uint 1) (,TYPE 1)))
         ;; (should        (mmec>= (mmec-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--greater-than-or-equal-to--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec>= (,TYPE 1) (,TYPE 2))))
	 (should 	(mmec>= (,TYPE 1) (,TYPE 1)))
	 (should 	(mmec>= (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should (not   (mmec>= (,TYPE 1) (mmec-sint 2))))
         ;; (should        (mmec>= (,TYPE 1) (mmec-sint 1)))
         ;; (should        (mmec>= (,TYPE 2) (mmec-sint 1)))
         ;; (should (not   (mmec>= (mmec-sint 1) (,TYPE 2))))
         ;; (should        (mmec>= (mmec-sint 1) (,TYPE 1)))
         ;; (should        (mmec>= (mmec-sint 2) (,TYPE 1)))
	 ;;
	 ;; Compare with an unsigned integer.
	 (should (not	(mmec>= (,TYPE 1) (mmec-uint 2))))
	 (should 	(mmec>= (,TYPE 1) (mmec-uint 1)))
	 (should 	(mmec>= (,TYPE 2) (mmec-uint 1)))
	 (should (not 	(mmec>= (mmec-uint 1) (,TYPE 2))))
	 (should 	(mmec>= (mmec-uint 1) (,TYPE 1)))
	 (should 	(mmec>= (mmec-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--floating-point--greater-than-or-equal-to-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec>= (,TYPE 1.0) (,TYPE 2.0))))
	 (should 	(mmec>= (,TYPE 1.0) (,TYPE 1.0)))
	 (should 	(mmec>= (,TYPE 2.0) (,TYPE 1.0)))
	 ;; Compare with a float.
	 (should (not 	(mmec>= (,TYPE 1.0) 2.0)))
	 (should 	(mmec>= (,TYPE 1.0) 1.0))
	 (should 	(mmec>= (,TYPE 2.0) 1.0))
	 (should (not 	(mmec>= 1.0 (,TYPE 2.0))))
	 (should 	(mmec>= 1.0 (,TYPE 1.0)))
	 (should 	(mmec>= 2.0 (,TYPE 1.0)))
	 ;; Compare with a mmec-float.
	 (should (not 	(mmec>= (,TYPE 1.0) (mmec-float 2.0))))
	 (should 	(mmec>= (,TYPE 1.0) (mmec-float 1.0)))
	 (should 	(mmec>= (,TYPE 2.0) (mmec-float 1.0)))
	 (should (not 	(mmec>= (mmec-float 1.0) (,TYPE 2.0))))
	 (should 	(mmec>= (mmec-float 1.0) (,TYPE 1.0)))
	 (should 	(mmec>= (mmec-float 2.0) (,TYPE 1.0)))
	 ;; Compare with a mmec-ldouble.
	 (should (not 	(mmec>= (,TYPE 1.0) (mmec-ldouble 2.0))))
	 (should 	(mmec>= (,TYPE 1.0) (mmec-ldouble 1.0)))
	 (should 	(mmec>= (,TYPE 2.0) (mmec-ldouble 1.0)))
	 (should (not 	(mmec>= (mmec-ldouble 1.0) (,TYPE 2.0))))
	 (should 	(mmec>= (mmec-ldouble 1.0) (,TYPE 1.0)))
	 (should 	(mmec>= (mmec-ldouble 2.0) (,TYPE 1.0)))
	 ))))

(ert-deftest greater-than-or-equal-to-integer ()
  "Compare objects of type `integer' for equality."
  (should (not 	(mmec>= 1 2)))
  (should 	(mmec>= 1 1))
  (should 	(mmec>= 2 1)))

(ert-deftest greater-than-or-equal-to-float ()
  "Compare objects of type `float' for equality."
  (should (not 	(mmec>= 1.0 2.0)))
  (should 	(mmec>= 1.0 1.0))
  (should 	(mmec>= 2.0 1.0)))

(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-char)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uchar)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-schar)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-wchar)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sshrt)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-ushrt)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sint)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uint)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-slong)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-ulong)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sllong)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-ullong)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sint8)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uint8)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sint16)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uint16)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sint32)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uint32)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sint64)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uint64)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-ssize)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-usize)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sintmax)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uintmax)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-ptrdiff)

(mmux-core-test--floating-point--greater-than-or-equal-to-tests float)
(mmux-core-test--floating-point--greater-than-or-equal-to-tests mmec-float)
(mmux-core-test--floating-point--greater-than-or-equal-to-tests mmec-ldouble)


;;;; mixed integer floating-point equality

(defmacro mmux-core-test--mixed-integer--equal-tests (FTYPE)
  (let* ((FTYPE.str	(symbol-name FTYPE))
	 (TESTNAME	(intern (concat "mixed-equality-integer-" FTYPE.str))))
    `(progn
       (ert-deftest ,TESTNAME ()
	 (should	(mmec= 1   (,FTYPE 1.0)))
	 (should	(mmec= (,FTYPE 1.0) 1))
	 (should (not	(mmec= 5   (,FTYPE 17.0))))
	 (should (not	(mmec= (,FTYPE 5.0) 17)))
	 ))))

(mmux-core-test--mixed-integer--equal-tests		mmec-float)
(mmux-core-test--mixed-integer--equal-tests		float)
(mmux-core-test--mixed-integer--equal-tests		mmec-ldouble)

;;; --------------------------------------------------------------------

(defmacro mmux-core-test--mixed--equal-tests (ITYPE FTYPE)
  (let* ((ITYPE.str	(symbol-name ITYPE))
	 (FTYPE.str	(symbol-name FTYPE))
	 (TESTNAME	(intern (concat "mixed-equality-" ITYPE.str "-" FTYPE.str))))
    `(progn
       (ert-deftest ,TESTNAME ()
	 (should	(mmec= (,ITYPE 1)   (,FTYPE 1.0)))
	 (should	(mmec= (,FTYPE 1.0) (,ITYPE 1)))
	 (should (not	(mmec= (,ITYPE 5)   (,FTYPE 17.0))))
	 (should (not	(mmec= (,FTYPE 5.0) (,ITYPE 17))))
	 ))))

(mmux-core-test--mixed--equal-tests mmec-sint	mmec-float)
(mmux-core-test--mixed--equal-tests mmec-uint	mmec-float)

(mmux-core-test--mixed--equal-tests mmec-sint	float)
(mmux-core-test--mixed--equal-tests mmec-uint	float)

(mmux-core-test--mixed--equal-tests mmec-sint	mmec-ldouble)
(mmux-core-test--mixed--equal-tests mmec-uint	mmec-ldouble)


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


;;;; number objects: printing

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME	(mmec-sformat "mmec-%s-print-test"	TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s"			TYPESTEM))
		       (DOCSTRING	(format "Test printing numbers of type `%s'." NUMTYPE))
		       (RESULT		(format "#s(%s 123)" NUMTYPE)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     (should (equal ,RESULT (cl-prin1-to-string (,NUMTYPE 123))))))))
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
		(let* ((TESTNAME	(mmec-sformat "mmec-%s-print-test"	TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s"			TYPESTEM))
		       (DOCSTRING	(format "Test printing numbers of type `%s'." NUMTYPE))
		       (RESULT		(format "#s(%s 123)" NUMTYPE)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     (should (equal ,RESULT (cl-prin1-to-string (,NUMTYPE 123.0))))))))
  (mmec--def float)
  (mmec--def double)
  (mmec--def ldouble))


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; number-objects.el ends here
