;;; cc-basics.el --- basic definitions and module loading

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-06 06:51:34 marco>
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

(define-error 'mmux-core-error
  "Error while executing a MMUX Emacs Core operation."
  'error)

(define-error 'mmux-core-no-memory-error
  "Error allocating memory."
  'mmux-core-error)

(define-error 'mmux-core-index-out-of-range
  "Attempt to access the internal represenation of an object with an index out of range."
  'mmux-core-error)

(define-error 'mmux-core-bytevector-index-out-of-range
  "Attempt to access the internal represenation of a bytevector object with an index out of range."
  'mmux-core-index-out-of-range)

(define-error 'mmux-core-instantiating-abstract-type
  "An attempt was performed to instantiate an abstract data type."
  'mmux-core-error)


;;;; helpers

(defmacro cc--define-self-object-maker-method (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (CC-MAKER	(intern (concat TYPE.str "--make")))
	 (OBJ-GETTER	(intern (concat TYPE.str "-obj")))
	 (DOCSTRING	(concat "Build and return a new instance of `" TYPE.str "'.")))
    `(cl-defmethod ,TYPE ((init ,TYPE))
       ,DOCSTRING
       (,CC-MAKER :obj (,OBJ-GETTER init)))))

(defun cc-debug-print (&rest args)
  (pp args 'external-debugging-output))


;;;; done

(provide 'cc-basics)

;;; cc-core.el ends here
