;;; ctypes-comparison.el --- dynamic module test

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
(require 'cc-core)


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
	 (should	(cc= (,MAKER 1) (,MAKER 1)))
	 (should (not	(cc= (,MAKER 5) (,MAKER 17))))
	 ;; Compare with a signed integer.
	 (should	(cc= (,MAKER 1) (cc-sint 1)))
	 (should (not	(cc= (,MAKER 5) (cc-sint 17))))
	 (should	(cc= (cc-sint 1) (,MAKER 1)))
	 (should (not	(cc= (cc-sint 5) (,MAKER 17))))
	 ))))

(defmacro mmux-core-test--equality--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should	(cc= (,TYPE 1) (,TYPE 1)))
	 (should (not	(cc= (,TYPE 5) (,TYPE 17))))
	 ;; Compare with an unsigned integer.
	 (should	(cc= (,TYPE 1) (cc-uint 1)))
	 (should (not	(cc= (,TYPE 5) (cc-uint 17))))
	 (should	(cc= (cc-uint 1) (,TYPE 1)))
	 (should (not	(cc= (cc-uint 5) (,TYPE 17))))
	 ))))

(defmacro mmux-core-test--floating-point--equal-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality."))a)
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should	(cc= (,TYPE 1.0) (,TYPE 1.0)))
	 (should (not	(cc= (,TYPE 5.0) (,TYPE 17.0))))
	 ;; Compare with a `float'.
	 (should	(cc= (,TYPE 1.0) 1.0))
	 (should (not	(cc= (,TYPE 5.0) 17.0)))
	 (should	(cc= 1.0 (,TYPE 1.0)))
	 (should (not	(cc= 5.0 (,TYPE 17.0))))
	 ;; Compare with a `cc-float'.
	 (should	(cc= (,TYPE 1.0) (cc-float 1.0)))
	 (should (not	(cc= (,TYPE 5.0) (cc-float 17.0))))
	 (should	(cc= (cc-float 1.0) (,TYPE 1.0)))
	 (should (not	(cc= (cc-float 5.0) (,TYPE 17.0))))
	 ;; Compare with a `cc-ldouble'.
	 (should	(cc= (,TYPE 1.0) (cc-ldouble 1.0)))
	 (should (not	(cc= (,TYPE 5.0) (cc-ldouble 17.0))))
	 (should	(cc= (cc-ldouble 1.0) (,TYPE 1.0)))
	 (should (not	(cc= (cc-ldouble 5.0) (,TYPE 17.0))))
	 ))))

(mmux-core-test--equality--integers-signed	integer)
(mmux-core-test--equality--integers-signed	cc-char)
(mmux-core-test--equality--integers-unsigned	cc-uchar)
(mmux-core-test--equality--integers-signed	cc-schar)
(mmux-core-test--equality--integers-unsigned	cc-wchar)
(mmux-core-test--equality--integers-signed	cc-sshrt)
(mmux-core-test--equality--integers-unsigned	cc-ushrt)
(mmux-core-test--equality--integers-signed	cc-sint)
(mmux-core-test--equality--integers-unsigned	cc-uint)
(mmux-core-test--equality--integers-signed	cc-slong)
(mmux-core-test--equality--integers-unsigned	cc-ulong)
(mmux-core-test--equality--integers-signed	cc-sllong)
(mmux-core-test--equality--integers-unsigned	cc-ullong)
(mmux-core-test--equality--integers-signed	cc-sint8)
(mmux-core-test--equality--integers-unsigned	cc-uint8)
(mmux-core-test--equality--integers-signed	cc-sint16)
(mmux-core-test--equality--integers-unsigned	cc-uint16)
(mmux-core-test--equality--integers-signed	cc-sint32)
(mmux-core-test--equality--integers-unsigned	cc-uint32)
(mmux-core-test--equality--integers-signed	cc-sint64)
(mmux-core-test--equality--integers-unsigned	cc-uint64)
(mmux-core-test--equality--integers-signed	cc-ssize)
(mmux-core-test--equality--integers-unsigned	cc-usize)
(mmux-core-test--equality--integers-signed	cc-sintmax)
(mmux-core-test--equality--integers-unsigned	cc-uintmax)
(mmux-core-test--equality--integers-signed	cc-ptrdiff)

(mmux-core-test--floating-point--equal-tests	float)
(mmux-core-test--floating-point--equal-tests	cc-float)
(mmux-core-test--floating-point--equal-tests	cc-double)
(mmux-core-test--floating-point--equal-tests	cc-ldouble)


;;;; not-equality tests

(defmacro mmux-core-test--not-equal--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "not-equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for not-equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not	(cc/= (,TYPE 1) (,TYPE 1))))
	 (should 	(cc/= (,TYPE 5) (,TYPE 17)))
	 ;; Compare with a signed integer.
	 (should (not	(cc/= (,TYPE 1) (cc-sint 1))))
	 (should 	(cc/= (,TYPE 5) (cc-sint 17)))
	 (should (not	(cc/= (cc-sint 1) (,TYPE 1))))
	 (should 	(cc/= (cc-sint 5) (,TYPE 17)))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
	 ;; (should (not	(cc/= (,TYPE 1) (cc-uint 1))))
	 ;; (should 	(cc/= (,TYPE 5) (cc-uint 17)))
	 ;; (should (not	(cc/= (cc-uint 1) (,TYPE 1))))
	 ;; (should 	(cc/= (cc-uint 5) (,TYPE 17)))
	 ))))

(defmacro mmux-core-test--not-equal--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "not-equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for not-equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not	(cc/= (,TYPE 1) (,TYPE 1))))
	 (should 	(cc/= (,TYPE 5) (,TYPE 17)))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
	 ;; (should (not	(cc/= (,TYPE 1) (cc-sint 1))))
	 ;; (should 	(cc/= (,TYPE 5) (cc-sint 17)))
	 ;; (should (not	(cc/= (cc-sint 1) (,TYPE 1))))
	 ;; (should 	(cc/= (cc-sint 5) (,TYPE 17)))
	 ;; Compare with an unsigned integer.
	 (should (not	(cc/= (,TYPE 1) (cc-uint 1))))
	 (should 	(cc/= (,TYPE 5) (cc-uint 17)))
	 (should (not	(cc/= (cc-uint 1) (,TYPE 1))))
	 (should 	(cc/= (cc-uint 5) (,TYPE 17)))
	 ))))

(defmacro mmux-core-test--floating-point--not-equal-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for not-equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not	(cc/= (,TYPE 1.0) (,TYPE 1.0))))
	 (should 	(cc/= (,TYPE 5.0) (,TYPE 17.0)))
	 ;; Compare with a float.
	 (should (not	(cc/= (,TYPE 1.0) 1.0)))
	 (should 	(cc/= (,TYPE 5.0) 17.0))
	 (should (not	(cc/= 1.0 (,TYPE 1.0))))
	 (should 	(cc/= 5.0 (,TYPE 17.0)))
	 ;; Compare with a cc-float.
	 (should (not	(cc/= (,TYPE 1.0) (cc-float 1.0))))
	 (should 	(cc/= (,TYPE 5.0) (cc-float 17.0)))
	 (should (not	(cc/= (cc-float 1.0) (,TYPE 1.0))))
	 (should 	(cc/= (cc-float 5.0) (,TYPE 17.0)))
	 ;; Compare with a cc-ldouble.
	 (should (not	(cc/= (,TYPE 1.0) (cc-ldouble 1.0))))
	 (should 	(cc/= (,TYPE 5.0) (cc-ldouble 17.0)))
	 (should (not	(cc/= (cc-ldouble 1.0) (,TYPE 1.0))))
	 (should 	(cc/= (cc-ldouble 5.0) (,TYPE 17.0)))
	 ))))

(ert-deftest not-equality-integer ()
  "Compare objects of type `integer' for not-equality."
  (should (not	(cc/= 1 1)))
  (should 	(cc/= 1 2)))

(ert-deftest not-equality-float ()
  "Compare objects of type `float' for not-equality."
  (should (not	(cc/= 1.0 1.0)))
  (should 	(cc/= 1.0 2.0)))

(mmux-core-test--not-equal--integers-signed	cc-char)
(mmux-core-test--not-equal--integers-unsigned	cc-uchar)
(mmux-core-test--not-equal--integers-signed	cc-schar)
(mmux-core-test--not-equal--integers-unsigned	cc-wchar)
(mmux-core-test--not-equal--integers-signed	cc-sshrt)
(mmux-core-test--not-equal--integers-unsigned	cc-ushrt)
(mmux-core-test--not-equal--integers-signed	cc-sint)
(mmux-core-test--not-equal--integers-unsigned	cc-uint)
(mmux-core-test--not-equal--integers-signed	cc-slong)
(mmux-core-test--not-equal--integers-unsigned	cc-ulong)
(mmux-core-test--not-equal--integers-signed	cc-sllong)
(mmux-core-test--not-equal--integers-unsigned	cc-ullong)
(mmux-core-test--not-equal--integers-signed	cc-sint8)
(mmux-core-test--not-equal--integers-unsigned	cc-uint8)
(mmux-core-test--not-equal--integers-signed	cc-sint16)
(mmux-core-test--not-equal--integers-unsigned	cc-uint16)
(mmux-core-test--not-equal--integers-signed	cc-sint32)
(mmux-core-test--not-equal--integers-unsigned	cc-uint32)
(mmux-core-test--not-equal--integers-signed	cc-sint64)
(mmux-core-test--not-equal--integers-unsigned	cc-uint64)
(mmux-core-test--not-equal--integers-signed	cc-ssize)
(mmux-core-test--not-equal--integers-unsigned	cc-usize)
(mmux-core-test--not-equal--integers-signed	cc-sintmax)
(mmux-core-test--not-equal--integers-unsigned	cc-uintmax)
(mmux-core-test--not-equal--integers-signed	cc-ptrdiff)

(mmux-core-test--floating-point--not-equal-tests float)
(mmux-core-test--floating-point--not-equal-tests cc-float)
(mmux-core-test--floating-point--not-equal-tests cc-ldouble)


;;;; less-than tests

(defmacro mmux-core-test--less-than--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(cc< (,TYPE 1) (,TYPE 2)))
	 (should (not	(cc< (,TYPE 1) (,TYPE 1))))
	 (should (not	(cc< (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 (should 	(cc< (,TYPE 1) (cc-sint 2)))
	 (should (not	(cc< (,TYPE 1) (cc-sint 1))))
	 (should (not	(cc< (,TYPE 2) (cc-sint 1))))
	 (should 	(cc< (cc-sint 1) (,TYPE 2)))
	 (should (not	(cc< (cc-sint 1) (,TYPE 1))))
	 (should (not	(cc< (cc-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (cc< (,TYPE 1) (cc-uint 2)))
         ;; (should (not   (cc< (,TYPE 1) (cc-uint 1))))
         ;; (should (not   (cc< (,TYPE 2) (cc-uint 1))))
         ;; (should        (cc< (cc-uint 1) (,TYPE 2)))
         ;; (should (not   (cc< (cc-uint 1) (,TYPE 1))))
         ;; (should (not   (cc< (cc-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--less-than--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(cc< (,TYPE 1) (,TYPE 2)))
	 (should (not	(cc< (,TYPE 1) (,TYPE 1))))
	 (should (not	(cc< (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (cc< (,TYPE 1) (cc-sint 2)))
         ;; (should (not   (cc< (,TYPE 1) (cc-sint 1))))
         ;; (should (not   (cc< (,TYPE 2) (cc-sint 1))))
         ;; (should        (cc< (cc-sint 1) (,TYPE 2)))
         ;; (should (not   (cc< (cc-sint 1) (,TYPE 1))))
         ;; (should (not   (cc< (cc-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 (should 	(cc< (,TYPE 1) (cc-uint 2)))
	 (should (not	(cc< (,TYPE 1) (cc-uint 1))))
	 (should (not	(cc< (,TYPE 2) (cc-uint 1))))
	 (should 	(cc< (cc-uint 1) (,TYPE 2)))
	 (should (not	(cc< (cc-uint 1) (,TYPE 1))))
	 (should (not	(cc< (cc-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--floating-point--less-than-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(cc< (,TYPE 1.0) (,TYPE 2.0)))
	 (should (not	(cc< (,TYPE 1.0) (,TYPE 1.0))))
	 (should (not	(cc< (,TYPE 2.0) (,TYPE 1.0))))
	 ;; Compare with a float.
	 (should 	(cc< (,TYPE 1.0) 2.0))
	 (should (not	(cc< (,TYPE 1.0) 1.0)))
	 (should (not	(cc< (,TYPE 2.0) 1.0)))
	 (should 	(cc< 1.0 (,TYPE 2.0)))
	 (should (not	(cc< 1.0 (,TYPE 1.0))))
	 (should (not	(cc< 2.0 (,TYPE 1.0))))
	 ;; Compare with a cc-float.
	 (should 	(cc< (,TYPE 1.0) (cc-float 2.0)))
	 (should (not	(cc< (,TYPE 1.0) (cc-float 1.0))))
	 (should (not	(cc< (,TYPE 2.0) (cc-float 1.0))))
	 (should 	(cc< (cc-float 1.0) (,TYPE 2.0)))
	 (should (not	(cc< (cc-float 1.0) (,TYPE 1.0))))
	 (should (not	(cc< (cc-float 2.0) (,TYPE 1.0))))
	 ;; Compare with a cc-ldouble.
	 (should 	(cc< (,TYPE 1.0) (cc-ldouble 2.0)))
	 (should (not	(cc< (,TYPE 1.0) (cc-ldouble 1.0))))
	 (should (not	(cc< (,TYPE 2.0) (cc-ldouble 1.0))))
	 (should 	(cc< (cc-ldouble 1.0) (,TYPE 2.0)))
	 (should (not	(cc< (cc-ldouble 1.0) (,TYPE 1.0))))
	 (should (not	(cc< (cc-ldouble 2.0) (,TYPE 1.0))))
	 ))))

(ert-deftest less-than-integer ()
  "Compare objects of type `integer' for equality."
  (should 	(cc< 1 2))
  (should (not	(cc< 1 1)))
  (should (not	(cc< 2 1))))

(ert-deftest less-than-float ()
  "Compare objects of type `float' for equality."
  (should 	(cc< 1.0 2.0))
  (should (not	(cc< 1.0 1.0)))
  (should (not	(cc< 2.0 1.0))))

(mmux-core-test--less-than--integers-signed	cc-char)
(mmux-core-test--less-than--integers-unsigned	cc-uchar)
(mmux-core-test--less-than--integers-signed	cc-schar)
(mmux-core-test--less-than--integers-unsigned	cc-wchar)
(mmux-core-test--less-than--integers-signed	cc-sshrt)
(mmux-core-test--less-than--integers-unsigned	cc-ushrt)
(mmux-core-test--less-than--integers-signed	cc-sint)
(mmux-core-test--less-than--integers-unsigned	cc-uint)
(mmux-core-test--less-than--integers-signed	cc-slong)
(mmux-core-test--less-than--integers-unsigned	cc-ulong)
(mmux-core-test--less-than--integers-signed	cc-sllong)
(mmux-core-test--less-than--integers-unsigned	cc-ullong)
(mmux-core-test--less-than--integers-signed	cc-sint8)
(mmux-core-test--less-than--integers-unsigned	cc-uint8)
(mmux-core-test--less-than--integers-signed	cc-sint16)
(mmux-core-test--less-than--integers-unsigned	cc-uint16)
(mmux-core-test--less-than--integers-signed	cc-sint32)
(mmux-core-test--less-than--integers-unsigned	cc-uint32)
(mmux-core-test--less-than--integers-signed	cc-sint64)
(mmux-core-test--less-than--integers-unsigned	cc-uint64)
(mmux-core-test--less-than--integers-signed	cc-ssize)
(mmux-core-test--less-than--integers-unsigned	cc-usize)
(mmux-core-test--less-than--integers-signed	cc-sintmax)
(mmux-core-test--less-than--integers-unsigned	cc-uintmax)
(mmux-core-test--less-than--integers-signed	cc-ptrdiff)

(mmux-core-test--floating-point--less-than-tests float)
(mmux-core-test--floating-point--less-than-tests cc-float)
(mmux-core-test--floating-point--less-than-tests cc-ldouble)


;;;; greater-than tests

(defmacro mmux-core-test--greater-than--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(cc> (,TYPE 1) (,TYPE 2))))
	 (should (not	(cc> (,TYPE 1) (,TYPE 1))))
	 (should 	(cc> (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
	 (should (not 	(cc> (,TYPE 1) (cc-sint 2))))
	 (should (not	(cc> (,TYPE 1) (cc-sint 1))))
	 (should 	(cc> (,TYPE 2) (cc-sint 1)))
	 (should (not	(cc> (cc-sint 1) (,TYPE 2))))
	 (should (not	(cc> (cc-sint 1) (,TYPE 1))))
	 (should 	(cc> (cc-sint 2) (,TYPE 1)))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should (not   (cc> (,TYPE 1) (cc-uint 2))))
         ;; (should (not   (cc> (,TYPE 1) (cc-uint 1))))
         ;; (should        (cc> (,TYPE 2) (cc-uint 1)))
         ;; (should (not   (cc> (cc-uint 1) (,TYPE 2))))
         ;; (should (not   (cc> (cc-uint 1) (,TYPE 1))))
         ;; (should        (cc> (cc-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--greater-than--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(cc> (,TYPE 1) (,TYPE 2))))
	 (should (not	(cc> (,TYPE 1) (,TYPE 1))))
	 (should 	(cc> (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
         ;; (should (not   (cc> (,TYPE 1) (cc-sint 2))))
         ;; (should (not   (cc> (,TYPE 1) (cc-sint 1))))
         ;; (should        (cc> (,TYPE 2) (cc-sint 1)))
         ;; (should (not   (cc> (cc-sint 1) (,TYPE 2))))
         ;; (should (not   (cc> (cc-sint 1) (,TYPE 1))))
         ;; (should        (cc> (cc-sint 2) (,TYPE 1)))
	 ;;
	 ;; Compare with an unsigned integer.
	 ;;
	 (should (not	(cc> (,TYPE 1) (cc-uint 2))))
	 (should (not	(cc> (,TYPE 1) (cc-uint 1))))
	 (should	(cc> (,TYPE 2) (cc-uint 1)))
	 (should (not	(cc> (cc-uint 1) (,TYPE 2))))
	 (should (not	(cc> (cc-uint 1) (,TYPE 1))))
	 (should	(cc> (cc-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--floating-point--greater-than-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(cc> (,TYPE 1.0) (,TYPE 2.0))))
	 (should (not	(cc> (,TYPE 1.0) (,TYPE 1.0))))
	 (should 	(cc> (,TYPE 2.0) (,TYPE 1.0)))
	 ;; Compare with a float.
	 (should (not 	(cc> (,TYPE 1.0) 2.0)))
	 (should (not	(cc> (,TYPE 1.0) 1.0)))
	 (should 	(cc> (,TYPE 2.0) 1.0))
	 (should (not 	(cc> 1.0 (,TYPE 2.0))))
	 (should (not	(cc> 1.0 (,TYPE 1.0))))
	 (should 	(cc> 2.0 (,TYPE 1.0)))
	 ;; Compare with a cc-float.
	 (should (not 	(cc> (,TYPE 1.0) (cc-float 2.0))))
	 (should (not	(cc> (,TYPE 1.0) (cc-float 1.0))))
	 (should 	(cc> (,TYPE 2.0) (cc-float 1.0)))
	 (should (not 	(cc> (cc-float 1.0) (,TYPE 2.0))))
	 (should (not	(cc> (cc-float 1.0) (,TYPE 1.0))))
	 (should 	(cc> (cc-float 2.0) (,TYPE 1.0)))
	 ;; Compare with a cc-ldouble.
	 (should (not 	(cc> (,TYPE 1.0) (cc-ldouble 2.0))))
	 (should (not	(cc> (,TYPE 1.0) (cc-ldouble 1.0))))
	 (should 	(cc> (,TYPE 2.0) (cc-ldouble 1.0)))
	 (should (not 	(cc> (cc-ldouble 1.0) (,TYPE 2.0))))
	 (should (not	(cc> (cc-ldouble 1.0) (,TYPE 1.0))))
	 (should 	(cc> (cc-ldouble 2.0) (,TYPE 1.0)))
	 ))))

(ert-deftest greater-than-integer ()
  "Compare objects of type `integer' for equality."
  (should (not 	(cc> 1 2)))
  (should (not	(cc> 1 1)))
  (should 	(cc> 2 1)))

(ert-deftest greater-than-float ()
  "Compare objects of type `float' for equality."
  (should (not 	(cc> 1.0 2.0)))
  (should (not	(cc> 1.0 1.0)))
  (should 	(cc> 2.0 1.0)))

(mmux-core-test--greater-than--integers-signed		cc-char)
(mmux-core-test--greater-than--integers-unsigned	cc-uchar)
(mmux-core-test--greater-than--integers-signed		cc-schar)
(mmux-core-test--greater-than--integers-unsigned	cc-wchar)
(mmux-core-test--greater-than--integers-signed		cc-sshrt)
(mmux-core-test--greater-than--integers-unsigned	cc-ushrt)
(mmux-core-test--greater-than--integers-signed		cc-sint)
(mmux-core-test--greater-than--integers-unsigned	cc-uint)
(mmux-core-test--greater-than--integers-signed		cc-slong)
(mmux-core-test--greater-than--integers-unsigned	cc-ulong)
(mmux-core-test--greater-than--integers-signed		cc-sllong)
(mmux-core-test--greater-than--integers-unsigned	cc-ullong)
(mmux-core-test--greater-than--integers-signed		cc-sint8)
(mmux-core-test--greater-than--integers-unsigned	cc-uint8)
(mmux-core-test--greater-than--integers-signed		cc-sint16)
(mmux-core-test--greater-than--integers-unsigned	cc-uint16)
(mmux-core-test--greater-than--integers-signed		cc-sint32)
(mmux-core-test--greater-than--integers-unsigned	cc-uint32)
(mmux-core-test--greater-than--integers-signed		cc-sint64)
(mmux-core-test--greater-than--integers-unsigned	cc-uint64)
(mmux-core-test--greater-than--integers-signed		cc-ssize)
(mmux-core-test--greater-than--integers-unsigned	cc-usize)
(mmux-core-test--greater-than--integers-signed		cc-sintmax)
(mmux-core-test--greater-than--integers-unsigned	cc-uintmax)
(mmux-core-test--greater-than--integers-signed		cc-ptrdiff)

(mmux-core-test--floating-point--greater-than-tests float)
(mmux-core-test--floating-point--greater-than-tests cc-float)
(mmux-core-test--floating-point--greater-than-tests cc-ldouble)


;;;; less-than or equal-to tests

(defmacro mmux-core-test--less-than-or-equal-to--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(cc<= (,TYPE 1) (,TYPE 2)))
	 (should 	(cc<= (,TYPE 1) (,TYPE 1)))
	 (should (not	(cc<= (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 (should 	(cc<= (,TYPE 1) (cc-sint 2)))
	 (should 	(cc<= (,TYPE 1) (cc-sint 1)))
	 (should (not	(cc<= (,TYPE 2) (cc-sint 1))))
	 (should 	(cc<= (cc-sint 1) (,TYPE 2)))
	 (should 	(cc<= (cc-sint 1) (,TYPE 1)))
	 (should (not	(cc<= (cc-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (cc<= (,TYPE 1) (cc-uint 2)))
         ;; (should        (cc<= (,TYPE 1) (cc-uint 1)))
         ;; (should (not   (cc<= (,TYPE 2) (cc-uint 1))))
         ;; (should        (cc<= (cc-uint 1) (,TYPE 2)))
         ;; (should        (cc<= (cc-uint 1) (,TYPE 1)))
         ;; (should (not   (cc<= (cc-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--less-than-or-equal-to--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(cc<= (,TYPE 1) (,TYPE 2)))
	 (should 	(cc<= (,TYPE 1) (,TYPE 1)))
	 (should (not	(cc<= (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (cc<= (,TYPE 1) (cc-sint 2)))
         ;; (should        (cc<= (,TYPE 1) (cc-sint 1)))
         ;; (should (not   (cc<= (,TYPE 2) (cc-sint 1))))
         ;; (should        (cc<= (cc-sint 1) (,TYPE 2)))
         ;; (should        (cc<= (cc-sint 1) (,TYPE 1)))
         ;; (should (not   (cc<= (cc-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 (should 	(cc<= (,TYPE 1) (cc-uint 2)))
	 (should 	(cc<= (,TYPE 1) (cc-uint 1)))
	 (should (not	(cc<= (,TYPE 2) (cc-uint 1))))
	 (should 	(cc<= (cc-uint 1) (,TYPE 2)))
	 (should 	(cc<= (cc-uint 1) (,TYPE 1)))
	 (should (not	(cc<= (cc-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--floating-point--less-than-or-equal-to-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "less-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(cc<= (,TYPE 1.0) (,TYPE 2.0)))
	 (should 	(cc<= (,TYPE 1.0) (,TYPE 1.0)))
	 (should (not	(cc<= (,TYPE 2.0) (,TYPE 1.0))))
	 ;; Compare with a float.
	 (should 	(cc<= (,TYPE 1.0) 2.0))
	 (should 	(cc<= (,TYPE 1.0) 1.0))
	 (should (not	(cc<= (,TYPE 2.0) 1.0)))
	 (should 	(cc<= 1.0 (,TYPE 2.0)))
	 (should 	(cc<= 1.0 (,TYPE 1.0)))
	 (should (not	(cc<= 2.0 (,TYPE 1.0))))
	 ;; Compare with a cc-float.
	 (should 	(cc<= (,TYPE 1.0) (cc-float 2.0)))
	 (should 	(cc<= (,TYPE 1.0) (cc-float 1.0)))
	 (should (not	(cc<= (,TYPE 2.0) (cc-float 1.0))))
	 (should 	(cc<= (cc-float 1.0) (,TYPE 2.0)))
	 (should 	(cc<= (cc-float 1.0) (,TYPE 1.0)))
	 (should (not	(cc<= (cc-float 2.0) (,TYPE 1.0))))
	 ;; Compare with a cc-ldouble.
	 (should 	(cc<= (,TYPE 1.0) (cc-ldouble 2.0)))
	 (should 	(cc<= (,TYPE 1.0) (cc-ldouble 1.0)))
	 (should (not	(cc<= (,TYPE 2.0) (cc-ldouble 1.0))))
	 (should 	(cc<= (cc-ldouble 1.0) (,TYPE 2.0)))
	 (should 	(cc<= (cc-ldouble 1.0) (,TYPE 1.0)))
	 (should (not	(cc<= (cc-ldouble 2.0) (,TYPE 1.0))))
	 ))))

(ert-deftest less-than-or-equal-to-integer ()
  "Compare objects of type `integer' for equality."
  (should 	(cc<= 1 2))
  (should 	(cc<= 1 1))
  (should (not	(cc<= 2 1))))

(ert-deftest less-than-or-equal-to-float ()
  "Compare objects of type `float' for equality."
  (should 	(cc<= 1.0 2.0))
  (should 	(cc<= 1.0 1.0))
  (should (not	(cc<= 2.0 1.0))))

(mmux-core-test--less-than-or-equal-to--integers-signed		cc-char)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-uchar)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-schar)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-wchar)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-sshrt)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-ushrt)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-sint)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-uint)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-slong)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-ulong)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-sllong)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-ullong)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-sint8)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-uint8)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-sint16)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-uint16)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-sint32)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-uint32)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-sint64)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-uint64)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-ssize)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-usize)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-sintmax)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	cc-uintmax)
(mmux-core-test--less-than-or-equal-to--integers-signed		cc-ptrdiff)

(mmux-core-test--floating-point--less-than-or-equal-to-tests float)
(mmux-core-test--floating-point--less-than-or-equal-to-tests cc-float)
(mmux-core-test--floating-point--less-than-or-equal-to-tests cc-ldouble)


;;;; greater-than or equal-to tests

(defmacro mmux-core-test--greater-than-or-equal-to--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(cc>= (,TYPE 1) (,TYPE 2))))
	 (should 	(cc>= (,TYPE 1) (,TYPE 1)))
	 (should 	(cc>= (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
	 (should (not 	(cc>= (,TYPE 1) (cc-sint 2))))
	 (should 	(cc>= (,TYPE 1) (cc-sint 1)))
	 (should 	(cc>= (,TYPE 2) (cc-sint 1)))
	 (should (not	(cc>= (cc-sint 1) (,TYPE 2))))
	 (should 	(cc>= (cc-sint 1) (,TYPE 1)))
	 (should 	(cc>= (cc-sint 2) (,TYPE 1)))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should (not   (cc>= (,TYPE 1) (cc-uint 2))))
         ;; (should        (cc>= (,TYPE 1) (cc-uint 1)))
         ;; (should        (cc>= (,TYPE 2) (cc-uint 1)))
         ;; (should (not   (cc>= (cc-uint 1) (,TYPE 2))))
         ;; (should        (cc>= (cc-uint 1) (,TYPE 1)))
         ;; (should        (cc>= (cc-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--greater-than-or-equal-to--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(cc>= (,TYPE 1) (,TYPE 2))))
	 (should 	(cc>= (,TYPE 1) (,TYPE 1)))
	 (should 	(cc>= (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should (not   (cc>= (,TYPE 1) (cc-sint 2))))
         ;; (should        (cc>= (,TYPE 1) (cc-sint 1)))
         ;; (should        (cc>= (,TYPE 2) (cc-sint 1)))
         ;; (should (not   (cc>= (cc-sint 1) (,TYPE 2))))
         ;; (should        (cc>= (cc-sint 1) (,TYPE 1)))
         ;; (should        (cc>= (cc-sint 2) (,TYPE 1)))
	 ;;
	 ;; Compare with an unsigned integer.
	 (should (not	(cc>= (,TYPE 1) (cc-uint 2))))
	 (should 	(cc>= (,TYPE 1) (cc-uint 1)))
	 (should 	(cc>= (,TYPE 2) (cc-uint 1)))
	 (should (not 	(cc>= (cc-uint 1) (,TYPE 2))))
	 (should 	(cc>= (cc-uint 1) (,TYPE 1)))
	 (should 	(cc>= (cc-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--floating-point--greater-than-or-equal-to-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "greater-than-or-equal-to-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(cc>= (,TYPE 1.0) (,TYPE 2.0))))
	 (should 	(cc>= (,TYPE 1.0) (,TYPE 1.0)))
	 (should 	(cc>= (,TYPE 2.0) (,TYPE 1.0)))
	 ;; Compare with a float.
	 (should (not 	(cc>= (,TYPE 1.0) 2.0)))
	 (should 	(cc>= (,TYPE 1.0) 1.0))
	 (should 	(cc>= (,TYPE 2.0) 1.0))
	 (should (not 	(cc>= 1.0 (,TYPE 2.0))))
	 (should 	(cc>= 1.0 (,TYPE 1.0)))
	 (should 	(cc>= 2.0 (,TYPE 1.0)))
	 ;; Compare with a cc-float.
	 (should (not 	(cc>= (,TYPE 1.0) (cc-float 2.0))))
	 (should 	(cc>= (,TYPE 1.0) (cc-float 1.0)))
	 (should 	(cc>= (,TYPE 2.0) (cc-float 1.0)))
	 (should (not 	(cc>= (cc-float 1.0) (,TYPE 2.0))))
	 (should 	(cc>= (cc-float 1.0) (,TYPE 1.0)))
	 (should 	(cc>= (cc-float 2.0) (,TYPE 1.0)))
	 ;; Compare with a cc-ldouble.
	 (should (not 	(cc>= (,TYPE 1.0) (cc-ldouble 2.0))))
	 (should 	(cc>= (,TYPE 1.0) (cc-ldouble 1.0)))
	 (should 	(cc>= (,TYPE 2.0) (cc-ldouble 1.0)))
	 (should (not 	(cc>= (cc-ldouble 1.0) (,TYPE 2.0))))
	 (should 	(cc>= (cc-ldouble 1.0) (,TYPE 1.0)))
	 (should 	(cc>= (cc-ldouble 2.0) (,TYPE 1.0)))
	 ))))

(ert-deftest greater-than-or-equal-to-integer ()
  "Compare objects of type `integer' for equality."
  (should (not 	(cc>= 1 2)))
  (should 	(cc>= 1 1))
  (should 	(cc>= 2 1)))

(ert-deftest greater-than-or-equal-to-float ()
  "Compare objects of type `float' for equality."
  (should (not 	(cc>= 1.0 2.0)))
  (should 	(cc>= 1.0 1.0))
  (should 	(cc>= 2.0 1.0)))

(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-char)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-uchar)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-schar)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-wchar)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-sshrt)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-ushrt)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-sint)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-uint)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-slong)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-ulong)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-sllong)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-ullong)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-sint8)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-uint8)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-sint16)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-uint16)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-sint32)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-uint32)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-sint64)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-uint64)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-ssize)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-usize)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-sintmax)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	cc-uintmax)
(mmux-core-test--greater-than-or-equal-to--integers-signed	cc-ptrdiff)

(mmux-core-test--floating-point--greater-than-or-equal-to-tests float)
(mmux-core-test--floating-point--greater-than-or-equal-to-tests cc-float)
(mmux-core-test--floating-point--greater-than-or-equal-to-tests cc-ldouble)


;;;; mixed integer floating-point equality

(defmacro mmux-core-test--mixed-integer--equal-tests (FTYPE)
  (let* ((FTYPE.str	(symbol-name FTYPE))
	 (TESTNAME	(intern (concat "mixed-equality-integer-" FTYPE.str))))
    `(progn
       (ert-deftest ,TESTNAME ()
	 (should	(cc= 1   (,FTYPE 1.0)))
	 (should	(cc= (,FTYPE 1.0) 1))
	 (should (not	(cc= 5   (,FTYPE 17.0))))
	 (should (not	(cc= (,FTYPE 5.0) 17)))
	 ))))

(mmux-core-test--mixed-integer--equal-tests		cc-float)
(mmux-core-test--mixed-integer--equal-tests		float)
(mmux-core-test--mixed-integer--equal-tests		cc-ldouble)

;;; --------------------------------------------------------------------

(defmacro mmux-core-test--mixed--equal-tests (ITYPE FTYPE)
  (let* ((ITYPE.str	(symbol-name ITYPE))
	 (FTYPE.str	(symbol-name FTYPE))
	 (TESTNAME	(intern (concat "mixed-equality-" ITYPE.str "-" FTYPE.str))))
    `(progn
       (ert-deftest ,TESTNAME ()
	 (should	(cc= (,ITYPE 1)   (,FTYPE 1.0)))
	 (should	(cc= (,FTYPE 1.0) (,ITYPE 1)))
	 (should (not	(cc= (,ITYPE 5)   (,FTYPE 17.0))))
	 (should (not	(cc= (,FTYPE 5.0) (,ITYPE 17))))
	 ))))

(mmux-core-test--mixed--equal-tests cc-sint	cc-float)
(mmux-core-test--mixed--equal-tests cc-uint	cc-float)

(mmux-core-test--mixed--equal-tests cc-sint	float)
(mmux-core-test--mixed--equal-tests cc-uint	float)

(mmux-core-test--mixed--equal-tests cc-sint	cc-ldouble)
(mmux-core-test--mixed--equal-tests cc-uint	cc-ldouble)


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; ctypes-comparison.el ends here
