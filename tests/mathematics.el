;;; mathematics.el --- dynamic module test

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

(defun quasi-equal (X Y)
  (mmec< (mmec-sub X Y) 1e-3))


;;;; trigonometric functions

(ert-deftest mmec-float-sin ()
  "Test the trigonometric sine function."
  (should (quasi-equal (sin 0.23) (mmec-sin (mmec-double 0.23))))
  )


;;;; done

(garbage-collect)
(ert-run-tests-batch-and-exit)

;;; mathematics.el ends here
