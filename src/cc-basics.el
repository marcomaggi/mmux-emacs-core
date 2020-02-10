;;; cc-basics.el --- basic definitions and module loading

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-10 08:43:56 marco>
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

(eval-and-compile
  (load "libmmux-emacs-core"))
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

(eval-and-compile
  (defun cc--prefixed-string-p (STRING)
    ;;Return true if STRING begins with the prefix "cc-".
    ;;
    (string= "cc-" (substring STRING 0 3)))

  (defun cc--strip-prefix-from-symbol-name (SYMBOL)
    ;;If SYMBOL's  name begins  with the  prefix "cc-": strip  it and  return the  resulting string.
    ;;Otherwise return SYMBOL's name itself.
    ;;
    (let ((STRING	(symbol-name SYMBOL)))
      (if (cc--prefixed-string-p STRING)
	  ;;Strip the prefix.
	  (substring STRING 3)
	STRING)))

  (defun cc--prepend-prefix-to-symbol-name (SYMBOL)
    ;;If SYMBOL's name begins with the prefix "cc-": return SYMBOL's name itself.  Otherwise prepend
    ;;the prefix to te name and return the resulting string.
    ;;
    ;;
    (let ((STRING	(symbol-name SYMBOL)))
      (if (cc--prefixed-string-p STRING)
	  STRING
	;;Prepend the prefix
	(concat (concat "cc-" STRING)))))
  )

(defmacro cc--make (TYPE-OR-STEM &rest ARGS)
  ;;Expand into the application of a struct constructor to the given arguments.  Example:
  ;;
  ;;  (cl-defstruct (cc-sint64 (:constructor cc-sint64--make))
  ;;    obj)
  ;;
  ;;  (cc--make sint64 :obj 123)
  ;;  ==> (cc-sint64--make :obj 123)
  ;;
  (let* ((TYPE.str	(cc--prepend-prefix-to-symbol-name TYPE-OR-STEM))
	 (CONSTRUCTOR	(intern (concat TYPE.str "--make"))))
    (cons CONSTRUCTOR ARGS)))

(defmacro cc--extract-obj (TYPE-OR-STEM VALUE)
  ;;Expand into the application of a struct slot getter to a struct instance.  Example:
  ;;
  ;;  (cl-defstruct cc-mine obj)
  ;;
  ;;  (cc--extract-obj mine (cc-mine :obj 123))
  ;;  ==> (cc-mine-obj (cc-mine :obj 123))
  ;;  => 123
  ;;
  ;;Many struct types  defined by this package use  the name "obj" for the slot  holding an internal
  ;;representation (often a user-pointer object).
  ;;
  (let* ((TYPE.str	(cc--prepend-prefix-to-symbol-name TYPE-OR-STEM))
	 (EXTRACTOR	(intern (concat TYPE.str "-obj"))))
    `(,EXTRACTOR ,VALUE)))

(defmacro cc--clang-constructor (TYPE-OR-STEM &rest ARGS)
  ;;Expand  into the  application of  the  C language  object  constructor to  the given  arguments.
  ;;Example:
  ;;
  ;;  (cc--clang-constructor sint32 123)
  ;;  ==> (mmux-core-c-make-sint32 123)
  ;;
  (let* ((STEM.str		(cc--strip-prefix-from-symbol-name TYPE-OR-STEM))
	 (CLANG-CONSTRUCTOR	(intern (concat "mmux-core-c-make-" STEM.str))))
    (cons CLANG-CONSTRUCTOR ARGS)))

(defmacro cc--clang-converter (FROMTYPE-OR-STEM TOTYPE-OR-STEM &rest ARGS)
  ;;Expand  into  the application  of  the  C language  object  converter  to the  given  arguments.
  ;;Examples:
  ;;
  ;;  (cc--clang-converter sint32 sint64 123)
  ;;  ==> (mmux-core-c-sint32-to-sint54 (cc-sint32 123))
  ;;
  ;;  (cc--clang-converter cc-float cc-ldouble 123)
  ;;  ==> (mmux-core-c-float-to-ldouble (cc-float 1.2))
  ;;
  (let* ((FROMSTEM.str		(cc--strip-prefix-from-symbol-name FROMTYPE-OR-STEM))
	 (TOSTEM.str		(cc--strip-prefix-from-symbol-name TOTYPE-OR-STEM))
	 (CLANG-CONVERTER	(intern (concat "mmux-core-c-" FROMSTEM.str "-to-" TOSTEM.str))))
    (cons CLANG-CONVERTER ARGS)))


;;;; done

(provide 'cc-basics)

;;; cc-core.el ends here
