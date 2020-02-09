;;; cc-basics.el --- basic definitions and module loading

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-09 06:37:26 marco>
;; Keywords: extensions

;; This file is part of MMUX Emacs Core.
;;
;; This program is  free software: you can redistribute  it and/or modify it under the  terms of the
;; GNU General Public License as published by the  Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied  warranty of MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.
;;
;; You should have  received a copy of the  GNU General Public License along with  this program.  If
;; not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:

;;

;;; Change Log:

;;

;;; Code:

(eval-when-compile
  (load "libmmux-emacs-core"))
(load "libmmux-emacs-core")
(require 'cl-lib)


;;;; error symbols

(define-error 'mmec-error
  "Error while executing a MMUX Emacs Core operation."
  'error)

;;; --------------------------------------------------------------------
;;; constructor errors

(define-error 'mmec-error-constructor
  "An error occurred while constructing an object."
  'mmec-error)

(define-error 'mmec-error-no-memory
  "Error allocating memory."
  'mmec-error-constructor)

(define-error 'mmec-error-instantiating-abstract-type
  "An attempt was performed to instantiate an abstract data type."
  'mmec-error-constructor)

(define-error 'mmec-error-unsupported-init-type
  "An argument given to an object constructor has an unsupported type."
  'mmec-error-constructor)

;;; --------------------------------------------------------------------
;;; range errors

(define-error 'mmec-error-value-out-of-range
  "A numeric object is out of range."
  'mmec-error)

(define-error 'mmec-error-index-out-of-range
  "Attempt to access the internal represenation of an object with an index out of range."
  'mmec-error-value-out-of-range)

(define-error 'mmec-error-bytevector-index-out-of-range
  "Attempt to access the internal represenation of a bytevector object with an index out of range."
  'mmec-error-index-out-of-range)

;;; --------------------------------------------------------------------
;;; operations errors

(define-error 'mmec-error-signed/unsigned-integer-comparison
  "Cannot compare a signed integer with an unsigned integer."
  'mmec-error)


;;;; helpers

(defun cc-debug-print (&rest args)
  (pp args 'external-debugging-output))


;;;; done

(provide 'cc-basics)

;;; cc-core.el ends here
