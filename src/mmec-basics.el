;;; mmec-basics.el --- basic definitions and module loading

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-03-03 05:10:21 marco>
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
(require 'cl-print)


;;;; error symbols

(define-error 'mmec-error
  "Error while executing a MMUX Emacs Core operation."
  'error)

;;; --------------------------------------------------------------------
;;; features errors

(define-error 'mmec-error-unsupported-feature
  "The requested feature is not implemented."
  'mmec-error)

(define-error 'mmec-error-unimplemented-c-language-function
  "The requested C language function is not implemented by the underlying platform."
  'mmec-error)

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

;; bytevector errors

(define-error 'mmec-error-bytevector-constructor
  "An error occurred while building a bytevector object."
  'mmec-error-constructor)

(define-error 'mmec-error-bytevector-constructor-invalid-number-of-slots
  "An invalid number of slots was given as argument to a bytevector constructor."
  'mmec-error-bytevector-constructor)

(define-error 'mmec-error-bytevector-constructor-invalid-slot-size
  "An invalid slot size was given as argument to a bytevector constructor."
  'mmec-error-bytevector-constructor)

(define-error 'mmec-error-bytevector-constructor-size-too-big
  "The bytevector size resulting from the given slot size and number of slots is too big."
  'mmec-error-bytevector-constructor)

;;; --------------------------------------------------------------------
;;; range errors

(define-error 'mmec-error-value-out-of-range
  "A numeric object is out of range."
  'mmec-error)

(define-error 'mmec-error-index-out-of-range
  "Attempt to access the internal represenation of an object with an index out of range."
  'mmec-error-value-out-of-range)

;; bytevectors

(define-error 'mmec-error-bytevector-is-empty
  "Used to signal that a bytevector operand is empty.."
  'mmec-error-value-out-of-range)

(define-error 'mmec-error-bytevector-index-out-of-range
  "Attempt to access the internal represenation of a bytevector object with an index out of range."
  'mmec-error-index-out-of-range)

(define-error 'mmec-error-bytevector-span-start-out-of-range
  "The start index selected as slots span beginning in a bytevector is out of range."
  'mmec-error-bytevector-index-out-of-range)

(define-error 'mmec-error-bytevector-span-past-out-of-range
  "The past index selected as slots span ending in a bytevector is out of range."
  'mmec-error-bytevector-index-out-of-range)

(define-error 'mmec-error-bytevector-invalid-span-limits
  "The start and past indexes selected as slots span limits in a bytevector are invalid."
  'mmec-error-bytevector-index-out-of-range)

;;; --------------------------------------------------------------------
;;; operations errors

(define-error 'mmec-error-signed/unsigned-integer-comparison
  "Cannot compare a signed integer with an unsigned integer."
  'mmec-error)

;;; --------------------------------------------------------------------
;;; mathematics errors

(define-error 'mmec-error-overflow
  "Overflow while performing a mathematic operation."
  'mmec-error)

(define-error 'mmec-error-underflow
  "Underflow while performing a mathematic operation."
  'mmec-error)


;;;; helpers

(defun mmec-debug-print (&rest args)
  (pp args 'external-debugging-output))

(defun mmec-sformat (STRING &rest ARGS)
  (intern (apply 'format STRING ARGS)))

(eval-and-compile
  (defun mmec--prefixed-string-p (STRING)
    ;;Return true if STRING begins with the prefix "mmec-".
    ;;
    (and (stringp STRING)
	 (<= 5 (length STRING))
	 (string= "mmec-" (substring STRING 0 5))))

  (defun mmec--strip-prefix-from-symbol-name (SYMBOL)
    ;;If SYMBOL's  name begins  with the  prefix "mmec-": strip  it and  return the  resulting string.
    ;;Otherwise return SYMBOL's name itself.
    ;;
    (let ((STRING	(symbol-name SYMBOL)))
      (if (mmec--prefixed-string-p STRING)
	  ;;Strip the prefix.
	  (substring STRING 5)
	STRING)))

  (defun mmec--prepend-prefix-to-symbol-name (SYMBOL)
    ;;If SYMBOL's name begins with the prefix "mmec-": return SYMBOL's name itself.  Otherwise prepend
    ;;the prefix to te name and return the resulting string.
    ;;
    ;;
    (let ((STRING	(symbol-name SYMBOL)))
      (if (mmec--prefixed-string-p STRING)
	  STRING
	;;Prepend the prefix
	(concat (concat "mmec-" STRING)))))

  (defun mmec--type-elisp-constructor-name (TYPE-OR-STEM)
    (intern (concat (mmec--prepend-prefix-to-symbol-name TYPE-OR-STEM) "--make")))
  )

(defmacro mmec--make (TYPE-OR-STEM &rest ARGS)
  ;;Expand into the application of a struct constructor to the given arguments.  Example:
  ;;
  ;;  (cl-defstruct (mmec-sint64 (:constructor mmec-sint64--make))
  ;;    obj)
  ;;
  ;;  (mmec--make sint64 :obj 123)
  ;;  ==> (mmec-sint64--make :obj 123)
  ;;
  (cons (mmec--type-elisp-constructor-name TYPE-OR-STEM) ARGS))

(defmacro mmec--extract-obj (TYPE-OR-STEM VALUE)
  ;;Expand into the application of a struct slot getter to a struct instance.  Example:
  ;;
  ;;  (cl-defstruct mmec-mine obj)
  ;;
  ;;  (mmec--extract-obj mine (mmec-mine :obj 123))
  ;;  ==> (mmec-mine-obj (mmec-mine :obj 123))
  ;;  => 123
  ;;
  ;;Many struct types  defined by this package use  the name "obj" for the slot  holding an internal
  ;;representation (often a user-pointer object).
  ;;
  (let* ((TYPE.str	(mmec--prepend-prefix-to-symbol-name TYPE-OR-STEM))
	 (EXTRACTOR	(intern (concat TYPE.str "-obj"))))
    `(,EXTRACTOR ,VALUE)))


;;;; common macros enhancement

(defmacro mmec-defun (FUNCNAME &rest STUFF)
  "Wrapper for the macro `cl-defun' defined by MMUX Emacs Core.

Define  a function  like `cl-defun'  does with  the same  arguments.  In
addition: the  whole definition  is wrapped into  a `cl-symbol-macrolet'
form  defining the  special  constant `--func--'  which  expands to  the
quoted function name.

Argument  FUNCNAME  must  be  a  symbol representing  the  name  of  the
function.

Optional argument STUFF must be the meat of the function definition."
  (declare (indent defun))
  `(cl-symbol-macrolet ((--func-- (quote ,FUNCNAME)))
     (cl-defun ,FUNCNAME ,@STUFF)))

(defmacro mmec-defmethod (FUNCNAME &rest STUFF)
  "Wrapper for the macro `cl-defmethod' defined by MMUX Emacs Core.

Define a function like `cl-defmethod'  does with the same arguments.  In
addition: the  whole definition  is wrapped into  a `cl-symbol-macrolet'
form  defining the  special  constant `--func--'  which  expands to  the
quoted function name.

Argument FUNCNAME must be a symbol  representing the name of the generic
function.

Optional argument STUFF must be the meat of the method definition."
  (declare (indent defun))
  `(cl-symbol-macrolet ((--func-- (quote ,FUNCNAME)))
     (cl-defmethod ,FUNCNAME ,@STUFF)))


;;;; done

(provide 'mmec-basics)

;;; mmec-core.el ends here
