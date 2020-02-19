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

(ert-deftest mmec-sint16-bytevector ()
  "Setters and getters for a `mmec-sint16-bytevector' object."
  (let ((bv	(mmec-sint16-bytevector 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-sint16 (+ -10 i))))
    (dotimes (i 10)
      (should (mmec= (mmec-sint16 (+ -10 i))
		     (mmec-bytevector-ref bv i))))))


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;; ;;; test.el ends here
