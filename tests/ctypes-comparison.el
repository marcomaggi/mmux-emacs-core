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

(defmacro mmux-core-test--integers--equal-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should	(cc= (,TYPE 1) (,TYPE 1)))
	 (should (not	(cc= (,TYPE 5) (,TYPE 17))))
	 ;; Compare with a signed integer.
	 (should	(cc= (,TYPE 1) (cc-signed-int 1)))
	 (should (not	(cc= (,TYPE 5) (cc-signed-int 17))))
	 (should	(cc= (cc-signed-int 1) (,TYPE 1)))
	 (should (not	(cc= (cc-signed-int 5) (,TYPE 17))))
	 ;; Compare with an unsigned integer.
	 (should	(cc= (,TYPE 1) (cc-unsigned-int 1)))
	 (should (not	(cc= (,TYPE 5) (cc-unsigned-int 17))))
	 (should	(cc= (cc-unsigned-int 1) (,TYPE 1)))
	 (should (not	(cc= (cc-unsigned-int 5) (,TYPE 17))))
	 ))))

(defmacro mmux-core-test--floating-point--equal-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(intern (concat "equality-" TYPE.str)))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should	(cc= (,TYPE 1.0) (,TYPE 1.0)))
	 (should (not	(cc= (,TYPE 5.0) (,TYPE 17.0))))
	 ;; Compare with a float.
	 (should	(cc= (,TYPE 1.0) 1.0))
	 (should (not	(cc= (,TYPE 5.0) 17.0)))
	 (should	(cc= 1.0 (,TYPE 1.0)))
	 (should (not	(cc= 5.0 (,TYPE 17.0))))
	 ;; Compare with a cc-float.
	 (should	(cc= (,TYPE 1.0) (cc-float 1.0)))
	 (should (not	(cc= (,TYPE 5.0) (cc-float 17.0))))
	 (should	(cc= (cc-float 1.0) (,TYPE 1.0)))
	 (should (not	(cc= (cc-float 5.0) (,TYPE 17.0))))
	 ;; Compare with a cc-long-double.
	 (should	(cc= (,TYPE 1.0) (cc-long-double 1.0)))
	 (should (not	(cc= (,TYPE 5.0) (cc-long-double 17.0))))
	 (should	(cc= (cc-long-double 1.0) (,TYPE 1.0)))
	 (should (not	(cc= (cc-long-double 5.0) (,TYPE 17.0))))
	 ))))

(ert-deftest equality-integer ()
  "Compare objects of type `integer' for equality."
  (should	(cc= 1 1))
  (should (not	(cc= 1 2))))

(ert-deftest equality-float ()
  "Compare objects of type `float' for equality."
  (should	(cc= 1.0 1.0))
  (should (not	(cc= 1.0 2.0))))

(mmux-core-test--integers--equal-tests cc-char)
(mmux-core-test--integers--equal-tests cc-uchar)
(mmux-core-test--integers--equal-tests cc-schar)
(mmux-core-test--integers--equal-tests cc-wchar)
(mmux-core-test--integers--equal-tests cc-signed-short-int)
(mmux-core-test--integers--equal-tests cc-unsigned-short-int)
(mmux-core-test--integers--equal-tests cc-signed-int)
(mmux-core-test--integers--equal-tests cc-unsigned-int)
(mmux-core-test--integers--equal-tests cc-signed-long-int)
(mmux-core-test--integers--equal-tests cc-unsigned-long-int)
(mmux-core-test--integers--equal-tests cc-signed-long-long-int)
(mmux-core-test--integers--equal-tests cc-unsigned-long-long-int)
(mmux-core-test--integers--equal-tests cc-sint8)
(mmux-core-test--integers--equal-tests cc-uint8)
(mmux-core-test--integers--equal-tests cc-sint16)
(mmux-core-test--integers--equal-tests cc-uint16)
(mmux-core-test--integers--equal-tests cc-sint32)
(mmux-core-test--integers--equal-tests cc-uint32)
(mmux-core-test--integers--equal-tests cc-sint64)
(mmux-core-test--integers--equal-tests cc-uint64)
(mmux-core-test--integers--equal-tests cc-ssize)
(mmux-core-test--integers--equal-tests cc-usize)
(mmux-core-test--integers--equal-tests cc-sintmax)
(mmux-core-test--integers--equal-tests cc-uintmax)
(mmux-core-test--integers--equal-tests cc-ptrdiff)

(mmux-core-test--floating-point--equal-tests float)
(mmux-core-test--floating-point--equal-tests cc-float)
(mmux-core-test--floating-point--equal-tests cc-long-double)


;;;; not-equality tests

(defmacro mmux-core-test--integers--not-equal-tests (TYPE)
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
	 (should (not	(cc/= (,TYPE 1) (cc-signed-int 1))))
	 (should 	(cc/= (,TYPE 5) (cc-signed-int 17)))
	 (should (not	(cc/= (cc-signed-int 1) (,TYPE 1))))
	 (should 	(cc/= (cc-signed-int 5) (,TYPE 17)))
	 ;; Compare with an unsigned integer.
	 (should (not	(cc/= (,TYPE 1) (cc-unsigned-int 1))))
	 (should 	(cc/= (,TYPE 5) (cc-unsigned-int 17)))
	 (should (not	(cc/= (cc-unsigned-int 1) (,TYPE 1))))
	 (should 	(cc/= (cc-unsigned-int 5) (,TYPE 17)))
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
	 ;; Compare with a cc-long-double.
	 (should (not	(cc/= (,TYPE 1.0) (cc-long-double 1.0))))
	 (should 	(cc/= (,TYPE 5.0) (cc-long-double 17.0)))
	 (should (not	(cc/= (cc-long-double 1.0) (,TYPE 1.0))))
	 (should 	(cc/= (cc-long-double 5.0) (,TYPE 17.0)))
	 ))))

(ert-deftest not-equality-integer ()
  "Compare objects of type `integer' for not-equality."
  (should (not	(cc/= 1 1)))
  (should 	(cc/= 1 2)))

(ert-deftest not-equality-float ()
  "Compare objects of type `float' for not-equality."
  (should (not	(cc/= 1.0 1.0)))
  (should 	(cc/= 1.0 2.0)))

(mmux-core-test--integers--not-equal-tests cc-char)
(mmux-core-test--integers--not-equal-tests cc-uchar)
(mmux-core-test--integers--not-equal-tests cc-schar)
(mmux-core-test--integers--not-equal-tests cc-wchar)
(mmux-core-test--integers--not-equal-tests cc-signed-short-int)
(mmux-core-test--integers--not-equal-tests cc-unsigned-short-int)
(mmux-core-test--integers--not-equal-tests cc-signed-int)
(mmux-core-test--integers--not-equal-tests cc-unsigned-int)
(mmux-core-test--integers--not-equal-tests cc-signed-long-int)
(mmux-core-test--integers--not-equal-tests cc-unsigned-long-int)
(mmux-core-test--integers--not-equal-tests cc-signed-long-long-int)
(mmux-core-test--integers--not-equal-tests cc-unsigned-long-long-int)
(mmux-core-test--integers--not-equal-tests cc-sint8)
(mmux-core-test--integers--not-equal-tests cc-uint8)
(mmux-core-test--integers--not-equal-tests cc-sint16)
(mmux-core-test--integers--not-equal-tests cc-uint16)
(mmux-core-test--integers--not-equal-tests cc-sint32)
(mmux-core-test--integers--not-equal-tests cc-uint32)
(mmux-core-test--integers--not-equal-tests cc-sint64)
(mmux-core-test--integers--not-equal-tests cc-uint64)
(mmux-core-test--integers--not-equal-tests cc-ssize)
(mmux-core-test--integers--not-equal-tests cc-usize)
(mmux-core-test--integers--not-equal-tests cc-sintmax)
(mmux-core-test--integers--not-equal-tests cc-uintmax)
(mmux-core-test--integers--not-equal-tests cc-ptrdiff)

(mmux-core-test--floating-point--not-equal-tests float)
(mmux-core-test--floating-point--not-equal-tests cc-float)
(mmux-core-test--floating-point--not-equal-tests cc-long-double)


;;;; less-than tests

(defmacro mmux-core-test--integers--less-than-tests (TYPE)
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
	 (should 	(cc< (,TYPE 1) (cc-signed-int 2)))
	 (should (not	(cc< (,TYPE 1) (cc-signed-int 1))))
	 (should (not	(cc< (,TYPE 2) (cc-signed-int 1))))
	 (should 	(cc< (cc-signed-int 1) (,TYPE 2)))
	 (should (not	(cc< (cc-signed-int 1) (,TYPE 1))))
	 (should (not	(cc< (cc-signed-int 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 (should 	(cc< (,TYPE 1) (cc-unsigned-int 2)))
	 (should (not	(cc< (,TYPE 1) (cc-unsigned-int 1))))
	 (should (not	(cc< (,TYPE 2) (cc-unsigned-int 1))))
	 (should 	(cc< (cc-unsigned-int 1) (,TYPE 2)))
	 (should (not	(cc< (cc-unsigned-int 1) (,TYPE 1))))
	 (should (not	(cc< (cc-unsigned-int 2) (,TYPE 1))))
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
	 ;; Compare with a cc-long-double.
	 (should 	(cc< (,TYPE 1.0) (cc-long-double 2.0)))
	 (should (not	(cc< (,TYPE 1.0) (cc-long-double 1.0))))
	 (should (not	(cc< (,TYPE 2.0) (cc-long-double 1.0))))
	 (should 	(cc< (cc-long-double 1.0) (,TYPE 2.0)))
	 (should (not	(cc< (cc-long-double 1.0) (,TYPE 1.0))))
	 (should (not	(cc< (cc-long-double 2.0) (,TYPE 1.0))))
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

(mmux-core-test--integers--less-than-tests cc-char)
(mmux-core-test--integers--less-than-tests cc-uchar)
(mmux-core-test--integers--less-than-tests cc-schar)
(mmux-core-test--integers--less-than-tests cc-wchar)
(mmux-core-test--integers--less-than-tests cc-signed-short-int)
(mmux-core-test--integers--less-than-tests cc-unsigned-short-int)
(mmux-core-test--integers--less-than-tests cc-signed-int)
(mmux-core-test--integers--less-than-tests cc-unsigned-int)
(mmux-core-test--integers--less-than-tests cc-signed-long-int)
(mmux-core-test--integers--less-than-tests cc-unsigned-long-int)
(mmux-core-test--integers--less-than-tests cc-signed-long-long-int)
(mmux-core-test--integers--less-than-tests cc-unsigned-long-long-int)
(mmux-core-test--integers--less-than-tests cc-sint8)
(mmux-core-test--integers--less-than-tests cc-uint8)
(mmux-core-test--integers--less-than-tests cc-sint16)
(mmux-core-test--integers--less-than-tests cc-uint16)
(mmux-core-test--integers--less-than-tests cc-sint32)
(mmux-core-test--integers--less-than-tests cc-uint32)
(mmux-core-test--integers--less-than-tests cc-sint64)
(mmux-core-test--integers--less-than-tests cc-uint64)
(mmux-core-test--integers--less-than-tests cc-ssize)
(mmux-core-test--integers--less-than-tests cc-usize)
(mmux-core-test--integers--less-than-tests cc-sintmax)
(mmux-core-test--integers--less-than-tests cc-uintmax)
(mmux-core-test--integers--less-than-tests cc-ptrdiff)

(mmux-core-test--floating-point--less-than-tests float)
(mmux-core-test--floating-point--less-than-tests cc-float)
(mmux-core-test--floating-point--less-than-tests cc-long-double)


;;;; greater-than tests

(defmacro mmux-core-test--integers--greater-than-tests (TYPE)
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
	 (should (not 	(cc> (,TYPE 1) (cc-signed-int 2))))
	 (should (not	(cc> (,TYPE 1) (cc-signed-int 1))))
	 (should 	(cc> (,TYPE 2) (cc-signed-int 1)))
	 (should (not	(cc> (cc-signed-int 1) (,TYPE 2))))
	 (should (not	(cc> (cc-signed-int 1) (,TYPE 1))))
	 (should 	(cc> (cc-signed-int 2) (,TYPE 1)))
	 ;; Compare with an unsigned integer.
	 (should (not	(cc> (,TYPE 1) (cc-unsigned-int 2))))
	 (should (not	(cc> (,TYPE 1) (cc-unsigned-int 1))))
	 (should 	(cc> (,TYPE 2) (cc-unsigned-int 1)))
	 (should (not 	(cc> (cc-unsigned-int 1) (,TYPE 2))))
	 (should (not	(cc> (cc-unsigned-int 1) (,TYPE 1))))
	 (should 	(cc> (cc-unsigned-int 2) (,TYPE 1)))
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
	 ;; Compare with a cc-long-double.
	 (should (not 	(cc> (,TYPE 1.0) (cc-long-double 2.0))))
	 (should (not	(cc> (,TYPE 1.0) (cc-long-double 1.0))))
	 (should 	(cc> (,TYPE 2.0) (cc-long-double 1.0)))
	 (should (not 	(cc> (cc-long-double 1.0) (,TYPE 2.0))))
	 (should (not	(cc> (cc-long-double 1.0) (,TYPE 1.0))))
	 (should 	(cc> (cc-long-double 2.0) (,TYPE 1.0)))
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

(mmux-core-test--integers--greater-than-tests cc-char)
(mmux-core-test--integers--greater-than-tests cc-uchar)
(mmux-core-test--integers--greater-than-tests cc-schar)
(mmux-core-test--integers--greater-than-tests cc-wchar)
(mmux-core-test--integers--greater-than-tests cc-signed-short-int)
(mmux-core-test--integers--greater-than-tests cc-unsigned-short-int)
(mmux-core-test--integers--greater-than-tests cc-signed-int)
(mmux-core-test--integers--greater-than-tests cc-unsigned-int)
(mmux-core-test--integers--greater-than-tests cc-signed-long-int)
(mmux-core-test--integers--greater-than-tests cc-unsigned-long-int)
(mmux-core-test--integers--greater-than-tests cc-signed-long-long-int)
(mmux-core-test--integers--greater-than-tests cc-unsigned-long-long-int)
(mmux-core-test--integers--greater-than-tests cc-sint8)
(mmux-core-test--integers--greater-than-tests cc-uint8)
(mmux-core-test--integers--greater-than-tests cc-sint16)
(mmux-core-test--integers--greater-than-tests cc-uint16)
(mmux-core-test--integers--greater-than-tests cc-sint32)
(mmux-core-test--integers--greater-than-tests cc-uint32)
(mmux-core-test--integers--greater-than-tests cc-sint64)
(mmux-core-test--integers--greater-than-tests cc-uint64)
(mmux-core-test--integers--greater-than-tests cc-ssize)
(mmux-core-test--integers--greater-than-tests cc-usize)
(mmux-core-test--integers--greater-than-tests cc-sintmax)
(mmux-core-test--integers--greater-than-tests cc-uintmax)
(mmux-core-test--integers--greater-than-tests cc-ptrdiff)

(mmux-core-test--floating-point--greater-than-tests float)
(mmux-core-test--floating-point--greater-than-tests cc-float)
(mmux-core-test--floating-point--greater-than-tests cc-long-double)


;;;; less-than or equal-to tests

(defmacro mmux-core-test--integers--less-than-or-equal-to-tests (TYPE)
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
	 (should 	(cc<= (,TYPE 1) (cc-signed-int 2)))
	 (should 	(cc<= (,TYPE 1) (cc-signed-int 1)))
	 (should (not	(cc<= (,TYPE 2) (cc-signed-int 1))))
	 (should 	(cc<= (cc-signed-int 1) (,TYPE 2)))
	 (should 	(cc<= (cc-signed-int 1) (,TYPE 1)))
	 (should (not	(cc<= (cc-signed-int 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 (should 	(cc<= (,TYPE 1) (cc-unsigned-int 2)))
	 (should 	(cc<= (,TYPE 1) (cc-unsigned-int 1)))
	 (should (not	(cc<= (,TYPE 2) (cc-unsigned-int 1))))
	 (should 	(cc<= (cc-unsigned-int 1) (,TYPE 2)))
	 (should 	(cc<= (cc-unsigned-int 1) (,TYPE 1)))
	 (should (not	(cc<= (cc-unsigned-int 2) (,TYPE 1))))
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
	 ;; Compare with a cc-long-double.
	 (should 	(cc<= (,TYPE 1.0) (cc-long-double 2.0)))
	 (should 	(cc<= (,TYPE 1.0) (cc-long-double 1.0)))
	 (should (not	(cc<= (,TYPE 2.0) (cc-long-double 1.0))))
	 (should 	(cc<= (cc-long-double 1.0) (,TYPE 2.0)))
	 (should 	(cc<= (cc-long-double 1.0) (,TYPE 1.0)))
	 (should (not	(cc<= (cc-long-double 2.0) (,TYPE 1.0))))
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

(mmux-core-test--integers--less-than-or-equal-to-tests cc-char)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-uchar)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-schar)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-wchar)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-signed-short-int)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-unsigned-short-int)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-signed-int)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-unsigned-int)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-signed-long-int)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-unsigned-long-int)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-signed-long-long-int)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-unsigned-long-long-int)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-sint8)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-uint8)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-sint16)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-uint16)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-sint32)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-uint32)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-sint64)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-uint64)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-ssize)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-usize)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-sintmax)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-uintmax)
(mmux-core-test--integers--less-than-or-equal-to-tests cc-ptrdiff)

(mmux-core-test--floating-point--less-than-or-equal-to-tests float)
(mmux-core-test--floating-point--less-than-or-equal-to-tests cc-float)
(mmux-core-test--floating-point--less-than-or-equal-to-tests cc-long-double)


;;;; greater-than or equal-to tests

(defmacro mmux-core-test--integers--greater-than-or-equal-to-tests (TYPE)
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
	 (should (not 	(cc>= (,TYPE 1) (cc-signed-int 2))))
	 (should 	(cc>= (,TYPE 1) (cc-signed-int 1)))
	 (should 	(cc>= (,TYPE 2) (cc-signed-int 1)))
	 (should (not	(cc>= (cc-signed-int 1) (,TYPE 2))))
	 (should 	(cc>= (cc-signed-int 1) (,TYPE 1)))
	 (should 	(cc>= (cc-signed-int 2) (,TYPE 1)))
	 ;; Compare with an unsigned integer.
	 (should (not	(cc>= (,TYPE 1) (cc-unsigned-int 2))))
	 (should 	(cc>= (,TYPE 1) (cc-unsigned-int 1)))
	 (should 	(cc>= (,TYPE 2) (cc-unsigned-int 1)))
	 (should (not 	(cc>= (cc-unsigned-int 1) (,TYPE 2))))
	 (should 	(cc>= (cc-unsigned-int 1) (,TYPE 1)))
	 (should 	(cc>= (cc-unsigned-int 2) (,TYPE 1)))
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
	 ;; Compare with a cc-long-double.
	 (should (not 	(cc>= (,TYPE 1.0) (cc-long-double 2.0))))
	 (should 	(cc>= (,TYPE 1.0) (cc-long-double 1.0)))
	 (should 	(cc>= (,TYPE 2.0) (cc-long-double 1.0)))
	 (should (not 	(cc>= (cc-long-double 1.0) (,TYPE 2.0))))
	 (should 	(cc>= (cc-long-double 1.0) (,TYPE 1.0)))
	 (should 	(cc>= (cc-long-double 2.0) (,TYPE 1.0)))
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

(mmux-core-test--integers--greater-than-or-equal-to-tests cc-char)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-uchar)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-schar)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-wchar)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-signed-short-int)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-unsigned-short-int)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-signed-int)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-unsigned-int)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-signed-long-int)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-unsigned-long-int)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-signed-long-long-int)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-unsigned-long-long-int)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-sint8)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-uint8)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-sint16)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-uint16)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-sint32)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-uint32)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-sint64)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-uint64)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-ssize)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-usize)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-sintmax)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-uintmax)
(mmux-core-test--integers--greater-than-or-equal-to-tests cc-ptrdiff)

(mmux-core-test--floating-point--greater-than-or-equal-to-tests float)
(mmux-core-test--floating-point--greater-than-or-equal-to-tests cc-float)
(mmux-core-test--floating-point--greater-than-or-equal-to-tests cc-long-double)


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
(mmux-core-test--mixed-integer--equal-tests		cc-long-double)

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

(mmux-core-test--mixed--equal-tests cc-signed-int	cc-float)
(mmux-core-test--mixed--equal-tests cc-unsigned-int	cc-float)

(mmux-core-test--mixed--equal-tests cc-signed-int	float)
(mmux-core-test--mixed--equal-tests cc-unsigned-int	float)

(mmux-core-test--mixed--equal-tests cc-signed-int	cc-long-double)
(mmux-core-test--mixed--equal-tests cc-unsigned-int	cc-long-double)


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;; ;;; test.el ends here
