;;; demo.el --- dynamic module test

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


;;;; demo tests

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
  ;; (mmec--def schar)
  ;; (mmec--def uchar)
  ;; (mmec--def wchar)
  ;; (mmec--def sshrt)
  ;;(mmec--def ushrt)
  (mmec--def sint)
  ;; (mmec--def uint)
  ;; (mmec--def slong)
  ;; (mmec--def ulong)
  ;; (mmec--def sllong)
  ;; (mmec--def ullong)
  ;; (mmec--def ssize)
  ;; (mmec--def usize)
  ;; (mmec--def sintmax)
  ;; (mmec--def uintmax)
  ;; (mmec--def ptrdiff)
  ;; (mmec--def sint8)
  ;; (mmec--def uint8)
  ;; (mmec--def sint16)
  ;; (mmec--def uint16)
  ;; (mmec--def sint32)
  ;; (mmec--def uint32)
  ;; (mmec--def sint64)
  ;; (mmec--def uint64)
  ;; (mmec--def float)
  ;;(mmec--def double)
  ;; (mmec--def ldouble)
  )


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;; ;;; test.el ends here
