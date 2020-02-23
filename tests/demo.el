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

(require 'ert)
(require 'mmec)


;;;; demo tests

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
  (mmec--def char))


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;; ;;; test.el ends here
