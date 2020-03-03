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
    ((mmec--def (TYPESTEM ARGSTEM PROPERTY)
		(let* ((TESTNAME	(mmec-sformat "mmec-test-%s-constructors" TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (ARGTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (CONSTRUCTOR	NUMTYPE))
		  ;;(mmec-debug-print TYPESTEM ARGSTEM PROPERTY)
		  (cl-case PROPERTY
		    (fits		`(ert-deftest ,TESTNAME ()
					   (should (mmec-number-type-p char (,CONSTRUCTOR (mmec-limit-min ,ARGSTEM))))
					   (should (mmec-number-type-p char (,CONSTRUCTOR (mmec-limit-max ,ARGSTEM))))))
		    (no-fits		`(ert-deftest ,TESTNAME ()
					   (should (condition-case exc
						       (progn
							 (,CONSTRUCTOR (mmec-limit-min ,ARGSTEM))
							 (signal 'mmec-test-error (list "Expected exception: no-fits.")))
						     (mmec-error-value-out-of-range	t)))
					   (should (condition-case exc
						       (progn
							 (,CONSTRUCTOR (mmec-limit-max ,ARGSTEM))
							 (signal 'mmec-test-error (list "Expected exception: no-fits.")))
						     (mmec-error-value-out-of-range	t)))))
		    (unsupported	`(ert-deftest ,TESTNAME ()
					   (should (condition-case exc
						       (progn
							 (,CONSTRUCTOR (mmec-limit-min ,ARGSTEM))
							 (signal 'mmec-test-error (list "Expected exception: unsupported.")))
						     (mmec-error-unsupported-init-type	t)))))
		    (t
		     (signal 'mmec-error-invalid-argument (list 'mmec--def PROPERTY)))
		    ))))

  (cl-macrolet
      ((mmec--deftest (ARGSTEM PROPERTY) `(mmec--def char ,ARGSTEM ,PROPERTY)))
    ;;(mmec--deftest char		fits)
    (mmec--deftest schar	no-fits)
    ))

;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;; ;;; test.el ends here
