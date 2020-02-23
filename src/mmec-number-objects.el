;;; mmec-number-objects.el --- numeric type definitions for C language intefaces

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-23 21:27:50 marco>
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

(require 'mmec-basics)


;;;; basic numeric type definitions

;; Base type of all the custom number types defined by this module.
(cl-defstruct (mmec-number
	       (:constructor	mmec-number--make)))

;; Base type of all the custom exact integer types defined by this module.
(cl-defstruct (mmec-integer
	       (:include	mmec-number)
	       (:constructor	mmec-integer--make)))

;; Base type of all the custom exact signed integer number types defined by this module.
(cl-defstruct (mmec-signed-integer
	       (:include	mmec-integer)
	       (:constructor	mmec-signed-integer--make)))

;; Base type of all the custom exact unsigned integer number types defined by this module.
(cl-defstruct (mmec-unsigned-integer
	       (:include	mmec-integer)
	       (:constructor	mmec-unsigned-integer--make)))

;; Base type of all the custom floating-point number types defined by this module.
(cl-defstruct (mmec-floating-point
	       (:include	mmec-number)
	       (:constructor	mmec-floating-point--make)))

;;; --------------------------------------------------------------------

(cl-macrolet ((mmec--define-abstract-type-constructor
	       (TYPE)
	       `(defun ,TYPE (&rest args)
		  (signal 'mmux-core-instantiating-abstract-type (quote ,TYPE)))))
  (mmec--define-abstract-type-constructor mmec-number)
  (mmec--define-abstract-type-constructor mmec-integer)
  (mmec--define-abstract-type-constructor mmec-signed-integer)
  (mmec--define-abstract-type-constructor mmec-unsigned-integer)
  (mmec--define-abstract-type-constructor mmec-floating-point))


;;;; C language type wrappers: char

(cl-defstruct (mmec-char
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-char--make))
  obj)

(cl-defgeneric mmec-char (init)
  "Constructor for number objects of type `mmec-char'.

This  type  constructor  is  implemented as  a  generic  function.   The
argument INIT must be a number value.")

(cl-defmethod mmec-char ((init mmec-char))
  "Constructor for number objects of type `mmec-char'.

This is the copy constructor implemented as method.  This method creates
a   duplicate  of   the  INIT   value,  but   it  reuses   the  internal
representation (which is immutable)."
  (mmec-char--make :obj (mmec-char-obj init)))

(cl-defmethod mmec-char ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-char'.

This constructor normalises the initialisation  argument to an object of
type `mmec-sint64', then  it checks if the  range is valid: if  it is is
builds  an object  of type  `mmec-char', otherwise  is raised  the error
condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-char-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-char init)))
    (mmec-char--make :obj (mmec-c-make-integer-char-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-char ((init integer))
  "Constructor for number objects of type `mmec-char'.

This  constructor accepts  as initialisation  argument a  value of  type
`integer'.

This constructor normalises the initialisation  argument to an object of
type `mmec-sint64', then  it checks if the  range is valid: if  it is is
builds  an object  of type  `mmec-char', otherwise  it raises  the error
condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-char-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-char init)))
    (mmec-char--make :obj (mmec-c-make-integer-char-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-char ((init float))
  "Constructor for number objects of type `mmec-char'.

This  constructor accepts  as initialisation  argument a  value of  type
`float'.

This constructor normalises the initialisation  argument to an object of
type `mmec-sint64', then  it checks if the  range is valid: if  it is is
builds  an object  of type  `mmec-char', otherwise  it raises  the error
condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-char-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-char init)))
    (mmec-char--make :obj (mmec-c-make-integer-char-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-char ((init mmec-number))
  "Constructor for number objects of type `mmec-char'.

This constructor  method signals that the  given initialisation argument
is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-char init)))


;;;; C language type wrappers: signed char

(cl-defstruct (mmec-schar
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-schar--make))
  obj)

(cl-defgeneric mmec-schar (init)
  "Constructor for number objects of type `mmec-schar'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-schar ((init mmec-schar))
  "Constructor for number objects of type `mmec-schar'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-schar--make :obj (mmec-schar-obj init)))

(cl-defmethod mmec-schar ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-schar'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-schar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-schar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-schar init)))
    (mmec-schar--make :obj (mmec-c-make-integer-schar-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-schar ((init integer))
  "Constructor for number objects of type `mmec-schar'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-schar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-schar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-schar init)))
    (mmec-schar--make :obj (mmec-c-make-integer-schar-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-schar ((init float))
  "Constructor for number objects of type `mmec-schar'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-schar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-schar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-schar init)))
    (mmec-schar--make :obj (mmec-c-make-integer-schar-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-schar ((init mmec-number))
  "Constructor for number objects of type `mmec-schar'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-schar init)))


;;;; C language type wrappers: unsigned char

(cl-defstruct (mmec-uchar
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-uchar--make))
  obj)

(cl-defgeneric mmec-uchar (init)
  "Constructor for number objects of type `mmec-uchar'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-uchar ((init mmec-uchar))
  "Constructor for number objects of type `mmec-uchar'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-uchar--make :obj (mmec-uchar-obj init)))

(cl-defmethod mmec-uchar ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-uchar'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uchar',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uchar init)))
    (mmec-uchar--make :obj (mmec-c-make-integer-uchar-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uchar ((init integer))
  "Constructor for number objects of type `mmec-uchar'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uchar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uchar init)))
    (mmec-uchar--make :obj (mmec-c-make-integer-uchar-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uchar ((init float))
  "Constructor for number objects of type `mmec-uchar'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uchar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uchar init)))
    (mmec-uchar--make :obj (mmec-c-make-integer-uchar-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uchar ((init mmec-number))
  "Constructor for number objects of type `mmec-uchar'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-uchar init)))


;;;; C language type wrappers: wchar_t

(cl-defstruct (mmec-wchar
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-wchar--make))
  obj)

(cl-defgeneric mmec-wchar (init)
  "Constructor for number objects of type `mmec-wchar'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-wchar ((init mmec-wchar))
  "Constructor for number objects of type `mmec-wchar'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-wchar--make :obj (mmec-wchar-obj init)))

(cl-defmethod mmec-wchar ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-wchar'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-wchar',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-wchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-wchar init)))
    (mmec-wchar--make :obj (mmec-c-make-usrptr-wchar-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-wchar ((init integer))
  "Constructor for number objects of type `mmec-wchar'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uchar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-wchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-wchar init)))
    (mmec-wchar--make :obj (mmec-c-make-usrptr-wchar-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-wchar ((init float))
  "Constructor for number objects of type `mmec-wchar'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uchar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-wchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-wchar init)))
    (mmec-wchar--make :obj (mmec-c-make-usrptr-wchar-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-wchar ((init mmec-number))
  "Constructor for number objects of type `mmec-wchar'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-wchar init)))


;;;; C language type wrappers: signed short int

(cl-defstruct (mmec-sshrt
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-sshrt--make))
  obj)

(cl-defgeneric mmec-sshrt (init)
  "Constructor for number objects of type `mmec-sshrt'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-sshrt ((init mmec-sshrt))
  "Constructor for number objects of type `mmec-sshrt'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-sshrt--make :obj (mmec-sshrt-obj init)))

(cl-defmethod mmec-sshrt ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-sshrt'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sshrt',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sshrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sshrt init)))
    (mmec-sshrt--make :obj (mmec-c-make-integer-sshrt-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sshrt ((init integer))
  "Constructor for number objects of type `mmec-sshrt'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sshrt',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sshrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sshrt init)))
    (mmec-sshrt--make :obj (mmec-c-make-integer-sshrt-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sshrt ((init float))
  "Constructor for number objects of type `mmec-sshrt'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sshrt',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sshrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sshrt init)))
    (mmec-sshrt--make :obj (mmec-c-make-integer-sshrt-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sshrt ((init mmec-number))
  "Constructor for number objects of type `mmec-sshrt'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-sshrt init)))


;;;; C language type wrappers: unsigned short int

(cl-defstruct (mmec-ushrt
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-ushrt--make))
  obj)

(cl-defgeneric mmec-ushrt (init)
  "Constructor for number objects of type `mmec-ushrt'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-ushrt ((init mmec-ushrt))
  "Constructor for number objects of type `mmec-ushrt'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-ushrt--make :obj (mmec-ushrt-obj init)))

(cl-defmethod mmec-ushrt ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-ushrt'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ushrt',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-ushrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ushrt init)))
    (mmec-ushrt--make :obj (mmec-c-make-integer-ushrt-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-ushrt ((init integer))
  "Constructor for number objects of type `mmec-ushrt'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ushrt',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-ushrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ushrt init)))
    (mmec-ushrt--make :obj (mmec-c-make-integer-ushrt-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-ushrt ((init float))
  "Constructor for number objects of type `mmec-ushrt'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ushrt',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-ushrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ushrt init)))
    (mmec-ushrt--make :obj (mmec-c-make-integer-ushrt-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-ushrt ((init mmec-number))
  "Constructor for number objects of type `mmec-ushrt'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-ushrt init)))


;;;; C language type wrappers: signed int

(cl-defstruct (mmec-sint
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-sint--make))
  obj)

(cl-defgeneric mmec-sint (init)
  "Constructor for number objects of type `mmec-sint'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-sint ((init mmec-sint))
  "Constructor for number objects of type `mmec-sint'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-sint--make :obj (mmec-sint-obj init)))

(cl-defmethod mmec-sint ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-sint'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sint',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint init)))
    (mmec-sint--make :obj (mmec-c-make-usrptr-sint-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint ((init integer))
  "Constructor for number objects of type `mmec-sint'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sint',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint init)))
    (mmec-sint--make :obj (mmec-c-make-usrptr-sint-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint ((init float))
  "Constructor for number objects of type `mmec-sint'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sint',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint init)))
    (mmec-sint--make :obj (mmec-c-make-usrptr-sint-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint ((init mmec-number))
  "Constructor for number objects of type `mmec-sint'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-sint init)))


;;;; C language type wrappers: unsigned int

(cl-defstruct (mmec-uint
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-uint--make))
  obj)

(cl-defgeneric mmec-uint (init)
  "Constructor for number objects of type `mmec-uint'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-uint ((init mmec-uint))
  "Constructor for number objects of type `mmec-uint'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-uint--make :obj (mmec-uint-obj init)))

(cl-defmethod mmec-uint ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-uint'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uint',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint init)))
    (mmec-uint--make :obj (mmec-c-make-usrptr-uint-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint ((init integer))
  "Constructor for number objects of type `mmec-uint'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uint',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint init)))
    (mmec-uint--make :obj (mmec-c-make-usrptr-uint-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint ((init float))
  "Constructor for number objects of type `mmec-uint'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uint',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint init)))
    (mmec-uint--make :obj (mmec-c-make-usrptr-uint-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint ((init mmec-number))
  "Constructor for number objects of type `mmec-uint'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-uint init)))


;;;; C language type wrappers: signed long int

(cl-defstruct (mmec-slong
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-slong--make))
  obj)

(cl-defgeneric mmec-slong (init)
  "Constructor for number objects of type `mmec-slong'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-slong ((init mmec-slong))
  "Constructor for number objects of type `mmec-slong'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-slong--make :obj (mmec-slong-obj init)))

(cl-defmethod mmec-slong ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-slong'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-slong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-slong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-slong init)))
    (mmec-slong--make :obj (mmec-c-make-usrptr-slong-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-slong ((init integer))
  "Constructor for number objects of type `mmec-slong'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-slong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-slong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-slong init)))
    (mmec-slong--make :obj (mmec-c-make-usrptr-slong-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-slong ((init float))
  "Constructor for number objects of type `mmec-slong'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-slong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-slong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-slong init)))
    (mmec-slong--make :obj (mmec-c-make-usrptr-slong-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-slong ((init mmec-number))
  "Constructor for number objects of type `mmec-slong'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-slong init)))


;;;; C language type wrappers: unsigned long int

(cl-defstruct (mmec-ulong
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-ulong--make))
  obj)

(cl-defgeneric mmec-ulong (init)
  "Constructor for number objects of type `mmec-ulong'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-ulong ((init mmec-ulong))
  "Constructor for number objects of type `mmec-ulong'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-ulong--make :obj (mmec-ulong-obj init)))

(cl-defmethod mmec-ulong ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-ulong'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ulong',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-ulong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ulong init)))
    (mmec-ulong--make :obj (mmec-c-make-usrptr-ulong-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-ulong ((init integer))
  "Constructor for number objects of type `mmec-ulong'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ulong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-ulong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ulong init)))
    (mmec-ulong--make :obj (mmec-c-make-usrptr-ulong-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-ulong ((init float))
  "Constructor for number objects of type `mmec-ulong'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ulong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-ulong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ulong init)))
    (mmec-ulong--make :obj (mmec-c-make-usrptr-ulong-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-ulong ((init mmec-number))
  "Constructor for number objects of type `mmec-ulong'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-ulong init)))


;;;; C language type wrappers: signed long long int

(cl-defstruct (mmec-sllong
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-sllong--make))
  obj)

(cl-defgeneric mmec-sllong (init)
  "Constructor for number objects of type `mmec-sllong'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-sllong ((init mmec-sllong))
  "Constructor for number objects of type `mmec-sllong'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-sllong--make :obj (mmec-sllong-obj init)))

(cl-defmethod mmec-sllong ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-sllong'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sllong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sllong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sllong init)))
    (mmec-sllong--make :obj (mmec-c-make-usrptr-sllong-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sllong ((init integer))
  "Constructor for number objects of type `mmec-sllong'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sllong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sllong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sllong init)))
    (mmec-sllong--make :obj (mmec-c-make-usrptr-sllong-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sllong ((init float))
  "Constructor for number objects of type `mmec-sllong'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sllong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sllong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sllong init)))
    (mmec-sllong--make :obj (mmec-c-make-usrptr-sllong-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sllong ((init mmec-number))
  "Constructor for number objects of type `mmec-sllong'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-sllong init)))


;;;; C language type wrappers: unsigned long long int

(cl-defstruct (mmec-ullong
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-ullong--make))
  obj)

(cl-defgeneric mmec-ullong (init)
  "Constructor for number objects of type `mmec-ullong'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-ullong ((init mmec-ullong))
  "Constructor for number objects of type `mmec-ullong'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-ullong--make :obj (mmec-ullong-obj init)))

(cl-defmethod mmec-ullong ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-ullong'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ullong',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-ullong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ullong init)))
    (mmec-ullong--make :obj (mmec-c-make-usrptr-ullong-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-ullong ((init integer))
  "Constructor for number objects of type `mmec-ullong'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ullong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-ullong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ullong init)))
    (mmec-ullong--make :obj (mmec-c-make-usrptr-ullong-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-ullong ((init float))
  "Constructor for number objects of type `mmec-ullong'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ullong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-ullong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ullong init)))
    (mmec-ullong--make :obj (mmec-c-make-usrptr-ullong-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-ullong ((init mmec-number))
  "Constructor for number objects of type `mmec-ullong'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-ullong init)))


;;;; C language type wrappers: intmax_t

(cl-defstruct (mmec-sintmax
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-sintmax--make))
  obj)

(cl-defgeneric mmec-sintmax (init)
  "Constructor for number objects of type `mmec-sintmax'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-sintmax ((init mmec-sintmax))
  "Constructor for number objects of type `mmec-sintmax'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-sintmax--make :obj (mmec-sintmax-obj init)))

(cl-defmethod mmec-sintmax ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-sintmax'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sintmax',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sintmax init)))
    (mmec-sintmax--make :obj (mmec-c-make-usrptr-sintmax-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sintmax ((init integer))
  "Constructor for number objects of type `mmec-sintmax'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sintmax',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sintmax init)))
    (mmec-sintmax--make :obj (mmec-c-make-usrptr-sintmax-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sintmax ((init float))
  "Constructor for number objects of type `mmec-sintmax'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sintmax',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sintmax init)))
    (mmec-sintmax--make :obj (mmec-c-make-usrptr-sintmax-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sintmax ((init mmec-number))
  "Constructor for number objects of type `mmec-sintmax'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-sintmax init)))


;;;; C language type wrappers: uintmax_t

(cl-defstruct (mmec-uintmax
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-uintmax--make))
  obj)

(cl-defgeneric mmec-uintmax (init)
  "Constructor for number objects of type `mmec-uintmax'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-uintmax ((init mmec-uintmax))
  "Constructor for number objects of type `mmec-uintmax'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-uintmax--make :obj (mmec-uintmax-obj init)))

(cl-defmethod mmec-uintmax ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-uintmax'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uintmax',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uintmax init)))
    (mmec-uintmax--make :obj (mmec-c-make-usrptr-uintmax-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uintmax ((init integer))
  "Constructor for number objects of type `mmec-uintmax'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uintmax',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uintmax init)))
    (mmec-uintmax--make :obj (mmec-c-make-usrptr-uintmax-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uintmax ((init float))
  "Constructor for number objects of type `mmec-uintmax'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uintmax',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uintmax init)))
    (mmec-uintmax--make :obj (mmec-c-make-usrptr-uintmax-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uintmax ((init mmec-number))
  "Constructor for number objects of type `mmec-uintmax'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-uintmax init)))


;;;; C language type wrappers: ssize_t

(cl-defstruct (mmec-ssize
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-ssize--make))
  obj)

(cl-defgeneric mmec-ssize (init)
  "Constructor for number objects of type `mmec-ssize'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-ssize ((init mmec-ssize))
  "Constructor for number objects of type `mmec-ssize'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-ssize--make :obj (mmec-ssize-obj init)))

(cl-defmethod mmec-ssize ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-ssize'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ssize',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-ssize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ssize init)))
    (mmec-ssize--make :obj (mmec-c-make-usrptr-ssize-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-ssize ((init integer))
  "Constructor for number objects of type `mmec-ssize'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ssize',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-ssize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ssize init)))
    (mmec-ssize--make :obj (mmec-c-make-usrptr-ssize-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-ssize ((init float))
  "Constructor for number objects of type `mmec-ssize'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-ssize',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-ssize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ssize init)))
    (mmec-ssize--make :obj (mmec-c-make-usrptr-ssize-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-ssize ((init mmec-number))
  "Constructor for number objects of type `mmec-ssize'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-ssize init)))


;;;; C language type wrappers: size_t

(cl-defstruct (mmec-usize
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-usize--make))
  obj)

(cl-defgeneric mmec-usize (init)
  "Constructor for number objects of type `mmec-usize'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-usize ((init mmec-usize))
  "Constructor for number objects of type `mmec-usize'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-usize--make :obj (mmec-usize-obj init)))

(cl-defmethod mmec-usize ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-usize'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-usize',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-usize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-usize init)))
    (mmec-usize--make :obj (mmec-c-make-usrptr-usize-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-usize ((init integer))
  "Constructor for number objects of type `mmec-usize'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-usize',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-usize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-usize init)))
    (mmec-usize--make :obj (mmec-c-make-usrptr-usize-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-usize ((init float))
  "Constructor for number objects of type `mmec-usize'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-usize',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-usize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-usize init)))
    (mmec-usize--make :obj (mmec-c-make-usrptr-usize-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-usize ((init mmec-number))
  "Constructor for number objects of type `mmec-usize'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-usize init)))


;;;; C language type wrappers: ptrdiff_t

(cl-defstruct (mmec-ptrdiff
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-ptrdiff--make))
  obj)

(cl-defgeneric mmec-ptrdiff (init)
  "Constructor for number objects of type `mmec-ptrdiff'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-ptrdiff ((init mmec-ptrdiff))
  "Constructor for number objects of type `mmec-ptrdiff'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-ptrdiff--make :obj (mmec-ptrdiff-obj init)))

(cl-defmethod mmec-ptrdiff ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-ptrdiff'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it is  is builds an object of type  `mmec-ptrdiff', otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-ptrdiff-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ptrdiff init)))
    (mmec-ptrdiff--make :obj (mmec-c-make-usrptr-ptrdiff-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-ptrdiff ((init integer))
  "Constructor for number objects of type `mmec-ptrdiff'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it is  is builds an object of type  `mmec-ptrdiff', otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-ptrdiff-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ptrdiff init)))
    (mmec-ptrdiff--make :obj (mmec-c-make-usrptr-ptrdiff-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-ptrdiff ((init float))
  "Constructor for number objects of type `mmec-ptrdiff'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it is  is builds an object of type  `mmec-ptrdiff', otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-ptrdiff-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-ptrdiff init)))
    (mmec-ptrdiff--make :obj (mmec-c-make-usrptr-ptrdiff-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-ptrdiff ((init mmec-number))
  "Constructor for number objects of type `mmec-ptrdiff'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-ptrdiff init)))


;;;; C language type wrappers: int8_t

(cl-defstruct (mmec-sint8
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-sint8--make))
  obj)

(cl-defgeneric mmec-sint8 (init)
  "Constructor for number objects of type `mmec-sint8'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-sint8 ((init mmec-sint8))
  "Constructor for number objects of type `mmec-sint8'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-sint8--make :obj (mmec-sint8-obj init)))

(cl-defmethod mmec-sint8 ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-sint8'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sint8',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint8 init)))
    (mmec-sint8--make :obj (mmec-c-make-integer-sint8-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint8 ((init integer))
  "Constructor for number objects of type `mmec-sint8'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sint8',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint8 init)))
    (mmec-sint8--make :obj (mmec-c-make-integer-sint8-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint8 ((init float))
  "Constructor for number objects of type `mmec-sint8'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sint8',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint8 init)))
    (mmec-sint8--make :obj (mmec-c-make-integer-sint8-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint8 ((init mmec-number))
  "Constructor for number objects of type `mmec-sint8'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-sint8 init)))


;;;; C language type wrappers: uint8_t

(cl-defstruct (mmec-uint8
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-uint8--make))
  obj)

(cl-defgeneric mmec-uint8 (init)
  "Constructor for number objects of type `mmec-uint8'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-uint8 ((init mmec-uint8))
  "Constructor for number objects of type `mmec-uint8'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-uint8--make :obj (mmec-uint8-obj init)))

(cl-defmethod mmec-uint8 ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-uint8'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uint8',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint8 init)))
    (mmec-uint8--make :obj (mmec-c-make-integer-uint8-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint8 ((init integer))
  "Constructor for number objects of type `mmec-uint8'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uint8',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint8 init)))
    (mmec-uint8--make :obj (mmec-c-make-integer-uint8-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint8 ((init float))
  "Constructor for number objects of type `mmec-uint8'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uint8',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint8 init)))
    (mmec-uint8--make :obj (mmec-c-make-integer-uint8-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint8 ((init mmec-number))
  "Constructor for number objects of type `mmec-uint8'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-uint8 init)))


;;;; C language type wrappers: int16_t

(cl-defstruct (mmec-sint16
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-sint16--make))
  obj)

(cl-defgeneric mmec-sint16 (init)
  "Constructor for number objects of type `mmec-sint16'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-sint16 ((init mmec-sint16))
  "Constructor for number objects of type `mmec-sint16'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-sint16--make :obj (mmec-sint16-obj init)))

(cl-defmethod mmec-sint16 ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-sint16'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds an  object of type `mmec-sint16',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint16 init)))
    (mmec-sint16--make :obj (mmec-c-make-integer-sint16-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint16 ((init integer))
  "Constructor for number objects of type `mmec-sint16'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sint16',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint16 init)))
    (mmec-sint16--make :obj (mmec-c-make-integer-sint16-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint16 ((init float))
  "Constructor for number objects of type `mmec-sint16'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sint16',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint16 init)))
    (mmec-sint16--make :obj (mmec-c-make-integer-sint16-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint16 ((init mmec-number))
  "Constructor for number objects of type `mmec-sint16'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-sint16 init)))


;;;; C language type wrappers: uint16_t

(cl-defstruct (mmec-uint16
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-uint16--make))
  obj)

(cl-defgeneric mmec-uint16 (init)
  "Constructor for number objects of type `mmec-uint16'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-uint16 ((init mmec-uint16))
  "Constructor for number objects of type `mmec-uint16'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-uint16--make :obj (mmec-uint16-obj init)))

(cl-defmethod mmec-uint16 ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-uint16'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds an  object of type `mmec-uint16',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint16 init)))
    (mmec-uint16--make :obj (mmec-c-make-integer-uint16-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint16 ((init integer))
  "Constructor for number objects of type `mmec-uint16'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uint16',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint16 init)))
    (mmec-uint16--make :obj (mmec-c-make-integer-uint16-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint16 ((init float))
  "Constructor for number objects of type `mmec-uint16'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uint16',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint16 init)))
    (mmec-uint16--make :obj (mmec-c-make-integer-uint16-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint16 ((init mmec-number))
  "Constructor for number objects of type `mmec-uint16'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-uint16 init)))


;;;; C language type wrappers: int32_t

(cl-defstruct (mmec-sint32
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-sint32--make))
  obj)

(cl-defgeneric mmec-sint32 (init)
  "Constructor for number objects of type `mmec-sint32'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-sint32 ((init mmec-sint32))
  "Constructor for number objects of type `mmec-sint32'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-sint32--make :obj (mmec-sint32-obj init)))

(cl-defmethod mmec-sint32 ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-sint32'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds an  object of type `mmec-sint32',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint32 init)))
    (mmec-sint32--make :obj (mmec-c-make-usrptr-sint32-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint32 ((init integer))
  "Constructor for number objects of type `mmec-sint32'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sint32',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint32 init)))
    (mmec-sint32--make :obj (mmec-c-make-usrptr-sint32-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint32 ((init float))
  "Constructor for number objects of type `mmec-sint32'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-sint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-sint32',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-sint64 init)))
    (unless (mmec-fits-sint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-sint32 init)))
    (mmec-sint32--make :obj (mmec-c-make-usrptr-sint32-from-usrptr-sint64 (mmec-sint64-obj obj)))))

(cl-defmethod mmec-sint32 ((init mmec-number))
  "Constructor for number objects of type `mmec-sint32'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-sint32 init)))


;;;; C language type wrappers: uint32_t

(cl-defstruct (mmec-uint32
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-uint32--make))
  obj)

(cl-defgeneric mmec-uint32 (init)
  "Constructor for number objects of type `mmec-uint32'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-uint32 ((init mmec-uint32))
  "Constructor for number objects of type `mmec-uint32'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-uint32--make :obj (mmec-uint32-obj init)))

(cl-defmethod mmec-uint32 ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-uint32'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds an  object of type `mmec-uint32',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint32 init)))
    (mmec-uint32--make :obj (mmec-c-make-usrptr-uint32-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint32 ((init integer))
  "Constructor for number objects of type `mmec-uint32'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uint32',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint32 init)))
    (mmec-uint32--make :obj (mmec-c-make-usrptr-uint32-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint32 ((init float))
  "Constructor for number objects of type `mmec-uint32'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-uint64', then it  checks if the range is valid:
if it  is is builds  an object  of type `mmec-uint32',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-uint64 init)))
    (unless (mmec-fits-uint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-uint32 init)))
    (mmec-uint32--make :obj (mmec-c-make-usrptr-uint32-from-usrptr-uint64 (mmec-uint64-obj obj)))))

(cl-defmethod mmec-uint32 ((init mmec-number))
  "Constructor for number objects of type `mmec-uint32'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-uint32 init)))


;;;; C language type wrappers: sint64_t
;;
;;The custom number type  `mmec-sint64' is special because it is  used for normalised representation
;;of all the signed integer numbers, both built-in and custom.
;;

(cl-defstruct (mmec-sint64
	       (:include	mmec-signed-integer)
	       (:constructor	mmec-sint64--make))
  obj)

(cl-defgeneric mmec-sint64 (init)
  "Constructor for number objects of type `mmec-sint64'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-sint64 ((init mmec-sint64))
  "Constructor for number objects of type `mmec-sint64'.

The argument INIT is an instance  of type `mmec-sit64': this is the copy
constructor implemented as  method.  This method creates  a duplicate of
the elisp  object, but it  reuses the internal representation  (which is
immutable)."
  (mmec--make mmec-sint64 :obj (mmec--extract-obj mmec-sint64 init)))

(cl-defmethod mmec-sint64 ((init integer))
  "Constructor for number objects of type `mmec-sint64'.

The argument INIT must be a value of type `integer'."
  (mmec--make sint64 :obj (mmec-c-make-usrptr-sint64-from-elisp-integer init)))

(cl-defmethod mmec-sint64 ((init float))
  "Constructor for number objects of type `mmec-sint64'.

The argument INIT must be a  value of type `float'."
  (mmec--make sint64 :obj (mmec-c-make-usrptr-sint64-from-elisp-float init)))

(cl-defmethod mmec-sint64 ((init mmec-number))
  "Constructor for number objects of type `mmec-sint64'.

This constructor  method signals that the  given initialisation argument
is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-sint64 init)))

(cl-macrolet ((mmec--define-sint64-constructor-method-for-integer-intrep-init
	       (INIT-TYPE-OR-STEM)
	       "Expand into a `cl-defmethod' form defining a constructor method for `mmec-sint64' values.

The argument INIT-TYPE-OR-STEM must be a symbol representing a type name
or type stem name; this type must  be a signed integer with `integer' as
internal representation."
	       (let* ((INIT-TYPE	(intern (mmec--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
		      (DOCSTRING	(format "Constructor for number objects of type `mmec-sint64'.

The argument INIT must be an object of type `%s'." INIT-TYPE)))
		 `(cl-defmethod mmec-sint64 ((init ,INIT-TYPE))
		    ,DOCSTRING
		    (mmec--make sint64 :obj (mmec-c-make-usrptr-sint64-from-elisp-integer (mmec--extract-obj ,INIT-TYPE init)))))))
  (mmec--define-sint64-constructor-method-for-integer-intrep-init char)
  (mmec--define-sint64-constructor-method-for-integer-intrep-init schar)
  (mmec--define-sint64-constructor-method-for-integer-intrep-init sshrt)
  (mmec--define-sint64-constructor-method-for-integer-intrep-init sint8)
  (mmec--define-sint64-constructor-method-for-integer-intrep-init sint16))

(cl-macrolet
    ((mmec--define-sint64-constructor-method-for-usrptr-intrep-init
      (INIT-TYPE-OR-STEM)
      "Expand into a `cl-defmethod' form defining a constructor method for `mmec-sint64' values.

The argument INIT-TYPE-OR-STEM must be a symbol representing a type name
or  type  stem  name;  this  type  must  be  a  signed  integer  with  a
user-pointer object as internal representation."
      (let* ((INIT-TYPE		(intern (mmec--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
	     (CLANG-CONSTRUCTOR	(intern (format "mmec-c-make-usrptr-sint64-from-usrptr-%s" INIT-TYPE-OR-STEM)))
	     (DOCSTRING		(format "Constructor for number objects of type `mmec-sint64'.

The argument INIT must be an object of type `%s'." INIT-TYPE)))
	`(cl-defmethod mmec-sint64 ((init ,INIT-TYPE))
	   ,DOCSTRING
	   (mmec--make sint64 :obj (,CLANG-CONSTRUCTOR (mmec--extract-obj ,INIT-TYPE init)))))))
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init sint)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init slong)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init sllong)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init sintmax)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init ssize)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init sint32)
  (mmec--define-sint64-constructor-method-for-usrptr-intrep-init ptrdiff))


;;;; C language type wrappers: uint64_t
;;
;;The custom number type  `mmec-uint64' is special because it is  used for normalised representation
;;of all the unsigned integer numbers, both built-in and custom.
;;

(cl-defstruct (mmec-uint64
	       (:include	mmec-unsigned-integer)
	       (:constructor	mmec-uint64--make))
  obj)

(cl-defgeneric mmec-uint64 (init)
  "Constructor for number objects of type `mmec-uint64'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-uint64 ((init mmec-uint64))
  "Constructor for number objects of type `mmec-uint64'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec--make mmec-uint64 :obj (mmec--extract-obj mmec-uint64 init)))

(cl-defmethod mmec-uint64 ((init integer))
  "Constructor for number objects of type `mmec-uint64'.

This  constructor method  accepts  as  initialisation argument  a
value of the Emacs's built-in type `integer'."
  (when (> 0 init)
    (signal 'mmec-error-value-out-of-range (list 'mmec-uint64 init)))
  (mmec--make uint64 :obj (mmec-c-make-usrptr-uint64-from-elisp-integer init)))

(cl-defmethod mmec-uint64 ((init float))
  "Constructor for number objects of type `mmec-uint64'.

This  constructor method  accepts  as  initialisation argument  a
value  of the  Emacs's built-in  type `float'.   If the  value is
negative:   the   condition  `mmec-error-value-out-of-range'   is
raised."
  (when (> 0.0 init)
    (signal 'mmec-error-value-out-of-range (list 'mmec-uint64 init)))
  (mmec--make uint64 :obj (mmec-c-make-usrptr-uint64-from-elisp-float init)))

(cl-defmethod mmec-uint64 ((init mmec-number))
  "Constructor for number objects of type `mmec-uint64'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-uint64 init)))

(cl-macrolet ((mmec--define-uint64-constructor-method-for-integer-init
	       (INIT-TYPE-OR-STEM)
	       (let* ((INIT-TYPE	(intern (mmec--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
		      (DOCSTRING	(format "Constructor for number objects of type `mmec-uint64'.

This  constructor method  accepts as  initialisation argument  an
instance of type `%s'." INIT-TYPE)))
		 `(cl-defmethod mmec-uint64 ((init ,INIT-TYPE))
		    ,DOCSTRING
		    (mmec--make uint64 :obj (mmec-c-make-usrptr-uint64-from-elisp-integer (mmec--extract-obj ,INIT-TYPE init)))))))
  (mmec--define-uint64-constructor-method-for-integer-init uchar)
  (mmec--define-uint64-constructor-method-for-integer-init ushrt)
  (mmec--define-uint64-constructor-method-for-integer-init uint8)
  (mmec--define-uint64-constructor-method-for-integer-init uint16))

(cl-macrolet
    ((mmec--define-uint64-constructor-method-for-usrptr-init
      (INIT-TYPE-OR-STEM)
      (let* ((INIT-TYPE		(intern (mmec--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
	     (CLANG-CONSTRUCTOR	(intern (format "mmec-c-make-usrptr-uint64-from-usrptr-%s" INIT-TYPE-OR-STEM)))
	     (DOCSTRING		(format "Constructor for number objects of type `mmec-uint64'.

This  constructor method  accepts as  initialisation argument  an
instance of type `%s'." INIT-TYPE)))
	`(cl-defmethod mmec-uint64 ((init ,INIT-TYPE))
	   ,DOCSTRING
	   (mmec--make uint64 :obj (,CLANG-CONSTRUCTOR  (mmec--extract-obj ,INIT-TYPE init)))))))

  (mmec--define-uint64-constructor-method-for-usrptr-init wchar)
  (mmec--define-uint64-constructor-method-for-usrptr-init uint)
  (mmec--define-uint64-constructor-method-for-usrptr-init ulong)
  (mmec--define-uint64-constructor-method-for-usrptr-init ullong)
  (mmec--define-uint64-constructor-method-for-usrptr-init uintmax)
  (mmec--define-uint64-constructor-method-for-usrptr-init usize)
  (mmec--define-uint64-constructor-method-for-usrptr-init uint32))


;;;; C language type wrappers: float

(cl-defstruct (mmec-float
	       (:include	mmec-floating-point)
	       (:constructor	mmec-float--make))
  obj)

(cl-defgeneric mmec-float (init)
  "Constructor for number objects of type `mmec-float'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-float ((init mmec-float))
  "Constructor for number objects of type `mmec-float'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-float--make :obj (mmec-float-obj init)))

(cl-defmethod mmec-float ((init mmec-integer))
  "Constructor for number objects of type `mmec-float'.

The argument INIT is a value of type `mmec-integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-ldouble', then it checks if the range is valid:
if it  is is builds  an object  of type `mmec-float',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-ldouble init)))
    (unless (mmec-fits-float-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-float init)))
    (mmec-float--make :obj (mmec-c-make-usrptr-float-from-usrptr-ldouble (mmec-ldouble-obj obj)))))

(cl-defmethod mmec-float ((init integer))
  "Constructor for number objects of type `mmec-float'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-ldouble', then it checks if the range is valid:
if it  is is builds  an object  of type `mmec-float',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-ldouble init)))
    (unless (mmec-fits-float-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-float init)))
    (mmec-float--make :obj (mmec-c-make-usrptr-float-from-usrptr-ldouble (mmec-ldouble-obj obj)))))

(cl-defmethod mmec-float ((init float))
  "Constructor for number objects of type `mmec-float'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-ldouble', then it checks if the range is valid:
if it  is is builds  an object  of type `mmec-float',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-ldouble init)))
    (unless (mmec-fits-float-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-float init)))
    (mmec-float--make :obj (mmec-c-make-usrptr-float-from-usrptr-ldouble (mmec-ldouble-obj obj)))))

(cl-defmethod mmec-float ((init mmec-number))
  "Constructor for number objects of type `mmec-float'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-float init)))


;;;; C language type wrappers: double float

(cl-defstruct (mmec-double
	       (:include	mmec-floating-point)
	       (:constructor	mmec-double--make))
  obj)

(cl-defgeneric mmec-double (init)
  "Constructor for number objects of type `mmec-double'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-double ((init mmec-double))
  "Constructor for number objects of type `mmec-double'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  INIT value,  but  it reuses  the
internal representation (which is immutable)."
  (mmec-double--make :obj (mmec-double-obj init)))

(cl-defmethod mmec-double ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-double'.

This  constructor normalises  the initialisation  argument to  an
object of type `mmec-ldouble', then it checks if the range is valid:
if it  is is builds an  object of type `mmec-double',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-ldouble init)))
    (unless (mmec-fits-double-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-double init)))
    (mmec-double--make :obj (mmec-c-make-elisp-float-from-usrptr-ldouble (mmec-ldouble-obj obj)))))

(cl-defmethod mmec-double ((init integer))
  "Constructor for number objects of type `mmec-double'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This constructor normalises the initialisation  argument to an object of
type `mmec-ldouble', then it  checks if the range is valid:  if it is is
builds an  object of type  `mmec-double', otherwise it raises  the error
condition `mmec-error-value-out-of-range'."
  (let ((obj (mmec-ldouble init)))
    (unless (mmec-fits-double-p obj)
      (signal 'mmec-error-value-out-of-range (list 'mmec-double init)))
    (mmec-double--make :obj (mmec-c-make-elisp-float-from-usrptr-ldouble (mmec-ldouble-obj obj)))))

(cl-defmethod mmec-double ((init float))
  "Constructor for number objects of type `mmec-double'.

This constructor  accepts as  initialisation argument a  value of
type `float'."
  (mmec-double--make :obj init))

(cl-defmethod mmec-double ((init mmec-number))
  "Constructor for number objects of type `mmec-double'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-double init)))


;;;; C language type wrappers: long double float
;;
;;The custom number type `mmec-ldouble' is special  because it is used for normalised representation
;;of all the floating-point numbers, both built-in and custom.
;;

(cl-defstruct (mmec-ldouble
	       (:include	mmec-floating-point)
	       (:constructor	mmec-ldouble--make))
  obj)

(cl-defgeneric mmec-ldouble (init)
  "Constructor for number objects of type `mmec-ldouble'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod mmec-ldouble ((init mmec-ldouble))
  "Constructor for number objects of type `mmec-ldouble'.

This is the copy constructor  implemented as method.  This method
creates a duplicate of the INIT value, but it reuses the internal
representation (which is immutable)."
  (mmec--make mmec-ldouble :obj (mmec--extract-obj mmec-ldouble init)))

;;; --------------------------------------------------------------------

(cl-defmethod mmec-ldouble ((init integer))
  "Constructor for number objects of type `mmec-ldouble'.

This  constructor method  accepts  as  initialisation argument  a
value of the Emacs's built-in type `integer'.  The initialisation
value  is  normalised  to  an   instance  of  `float',  then  the
constructor is recursively applied to the normalised value."
  (mmec-ldouble (float init)))

(cl-defmethod mmec-ldouble ((init float))
  "Constructor for number objects of type `mmec-ldouble'.

This  constructor method  accepts  as  initialisation argument  a
value of the Emacs's built-in type `float'."
  (mmec--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-elisp-float init)))

;;; --------------------------------------------------------------------

(cl-defmethod mmec-ldouble ((init mmec-sint64))
  "Constructor for number objects of type `mmec-ldouble'.

This constructor  method initalises the returned  object with the
value from an instance of `mmec-sint64'"
  (mmec--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-usrptr-sint64 (mmec--extract-obj sint64 init))))

(cl-defmethod mmec-ldouble ((init mmec-uint64))
  "Constructor for number objects of type `mmec-ldouble'.

This constructor  method initalises the returned  object with the
value from an instance of `mmec-uint64'"
  (mmec--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-usrptr-uint64 (mmec--extract-obj uint64 init))))

;;; --------------------------------------------------------------------

(cl-defmethod mmec-ldouble ((init mmec-signed-integer))
  "Constructor for number objects of type `mmec-ldouble'.

This   constructor  method   normalises   any   number  of   type
`mmec-signed-integer'  to  an  instance  of  `mmec-sint64',  then  it
recursively calls the constructor on the result."
  (mmec-ldouble (mmec-sint64 init)))

(cl-defmethod mmec-ldouble ((init mmec-unsigned-integer))
  "Constructor for number objects of type `mmec-ldouble'.

This   constructor  method   normalises   any   number  of   type
`mmec-unsigned-integer'  to an  instance  of  `mmec-uint64', then  it
recursively calls the constructor on the result."
  (mmec-ldouble (mmec-uint64 init)))

;;; --------------------------------------------------------------------

(cl-defmethod mmec-ldouble ((init mmec-float))
  "Constructor for number objects of type `mmec-ldouble'.

The argument INIT is an instance of `mmec-float'."
  (mmec--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-usrptr-float (mmec--extract-obj float init))))

(cl-defmethod mmec-ldouble ((init mmec-double))
  "Constructor for number objects of type `mmec-ldouble'.

The argument INIT is an instance of `mmec-double'."
  (mmec--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-elisp-float (mmec--extract-obj double init))))

;;; --------------------------------------------------------------------

(cl-defmethod mmec-ldouble ((init mmec-number))
  "Constructor for number objects of type `mmec-ldouble'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'mmec-ldouble init)))


;;;; range inclusion

(cl-macrolet
    ((mmec--def (TYPE-OR-STEM USRPTR-ARGTYPE NORMALISED-TYPE-OR-STEM)
		(let* ((TYPE-STEM.str		(mmec--strip-prefix-from-symbol-name TYPE-OR-STEM))
		       (TYPE			(intern (mmec--prepend-prefix-to-symbol-name TYPE-OR-STEM)))
		       (NORMALISED-STEM.str	(mmec--strip-prefix-from-symbol-name NORMALISED-TYPE-OR-STEM))
		       (NORMALISED-TYPE		(intern (mmec--prepend-prefix-to-symbol-name NORMALISED-TYPE-OR-STEM)))
		       (FUNCNAME		(intern (format "mmec-fits-%s-p" TYPE-STEM.str)))
		       (CLANG-FUNCNAME		(intern (format "mmec-c-%s-fits-%s-p" NORMALISED-STEM.str TYPE-STEM.str)))
		       (DOCSTRING		(format "Return true if the argument fits an object of type `%s'." TYPE)))
		  `(progn
		     (cl-defgeneric ,FUNCNAME (op)
		       ,DOCSTRING)
		     (cl-defmethod  ,FUNCNAME ((op ,USRPTR-ARGTYPE))
		       ,DOCSTRING
		       (,CLANG-FUNCNAME (mmec--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
		     (cl-defmethod  ,FUNCNAME ((op integer))
		       ,DOCSTRING
		       (,CLANG-FUNCNAME (mmec--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
		     (cl-defmethod  ,FUNCNAME ((op float))
		       ,DOCSTRING
		       (,CLANG-FUNCNAME (mmec--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
		     )))
     (mmec--def/signed-integer (TYPE-OR-STEM)
			       `(mmec--def ,TYPE-OR-STEM mmec-signed-integer   mmec-sint64))
     (mmec--def/unsigned-integer (TYPE-OR-STEM)
				 `(mmec--def ,TYPE-OR-STEM mmec-unsigned-integer mmec-uint64))
     (mmec--def/floating-point (TYPE-OR-STEM)
			       `(mmec--def ,TYPE-OR-STEM mmec-floating-point   mmec-ldouble)))

  (mmec--def/signed-integer	char)
  (mmec--def/signed-integer	schar)
  (mmec--def/unsigned-integer	uchar)
  (mmec--def/unsigned-integer	wchar)
  (mmec--def/signed-integer	sshrt)
  (mmec--def/unsigned-integer	ushrt)
  (mmec--def/signed-integer	sint)
  (mmec--def/unsigned-integer	uint)
  (mmec--def/signed-integer	slong)
  (mmec--def/unsigned-integer	ulong)
  (mmec--def/signed-integer	sllong)
  (mmec--def/unsigned-integer	ullong)
  (mmec--def/signed-integer	ssize)
  (mmec--def/unsigned-integer	usize)
  (mmec--def/signed-integer	sintmax)
  (mmec--def/unsigned-integer	uintmax)
  (mmec--def/signed-integer	ptrdiff)
  (mmec--def/signed-integer	sint8)
  (mmec--def/unsigned-integer	uint8)
  (mmec--def/signed-integer	sint16)
  (mmec--def/unsigned-integer	uint16)
  (mmec--def/signed-integer	sint32)
  (mmec--def/unsigned-integer	uint32)
  (mmec--def/signed-integer	sint64)
  (mmec--def/unsigned-integer	uint64)
  (mmec--def/floating-point	float)
  (mmec--def/floating-point	double)
  (mmec--def/floating-point	ldouble))


;;;; printing

(cl-macrolet
    ((mmec--def (TYPESTEM)
		"This is for number objects having an object of type `integer' as internal representation."
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (OBJ-EXTRACTOR	(mmec-sformat "mmec-%s-obj" TYPESTEM))
		       (DOCSTRING	(format "Print to a stream the representation of a number object of type `%s'." NUMTYPE))
		       (TEMPLATE	(format "#s(%s %%d)" NUMTYPE)))
		  `(cl-defmethod cl-print-object ((obj ,NUMTYPE) stream)
		     ,DOCSTRING
		     (cl-print-object (format ,TEMPLATE (,OBJ-EXTRACTOR obj)) stream)))))
  (mmec--def char)
  (mmec--def schar)
  (mmec--def uchar)
  (mmec--def sshrt)
  (mmec--def ushrt)
  (mmec--def sint8)
  (mmec--def uint8)
  (mmec--def sint16)
  (mmec--def uint16))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		"This is for number objects having an object of type `float' as internal representation."
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (OBJ-EXTRACTOR	(mmec-sformat "mmec-%s-obj" TYPESTEM))
		       (DOCSTRING	(format "Print to a stream the representation of a number object of type `%s'." NUMTYPE))
		       (TEMPLATE	(format "#s(%s %%f)" NUMTYPE)))
		  `(cl-defmethod cl-print-object ((obj ,NUMTYPE) stream)
		     ,DOCSTRING
		     (cl-print-object (format ,TEMPLATE (,OBJ-EXTRACTOR obj)) stream)))))
  (mmec--def double))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		"This is for number objects having a user-pointer object as internal representation."
		(let* ((NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (OBJ-EXTRACTOR	(mmec-sformat "mmec-%s-obj" TYPESTEM))
		       (DOCSTRING	(format "Print to a stream the representation of a number object of type `%s'." NUMTYPE))
		       (CLANG-PRINTER	(mmec-sformat "mmec-c-%s-print-to-string" TYPESTEM)))
		  `(cl-defmethod cl-print-object ((obj ,NUMTYPE) stream)
		     ,DOCSTRING
		     (cl-print-object (,CLANG-PRINTER (,OBJ-EXTRACTOR obj)) stream)))))
  (mmec--def wchar)
  (mmec--def sint)
  (mmec--def uint)
  (mmec--def slong)
  (mmec--def ulong)
  (mmec--def sllong)
  (mmec--def ullong)
  (mmec--def ssize)
  (mmec--def usize)
  (mmec--def sintmax)
  (mmec--def uintmax)
  (mmec--def ptrdiff)
  (mmec--def sint32)
  (mmec--def uint32)
  (mmec--def sint64)
  (mmec--def uint64)
  (mmec--def float)
  (mmec--def ldouble))


;;;; numeric comparison operations
;;
;;To perform a comparison operation we normalise the operands as follows:
;;
;;* We convert all the signed integers to `mmec-sint64'.
;;
;;* We convert all the unsigned integers to `mmec-uint64'.
;;
;;* We convert all the floating-point numbers to `mmec-ldouble'.
;;
;;* When  comparing  integers  and floating-point  numbers  we  convert  all  the integer  types  to
;;  `mmec-ldouble'.
;;

(defmacro mmec--define-numeric-comparison-generic-functions (OPERATOR)
  (let* ((MMEC-FUNC	(intern (format "mmec%s"   OPERATOR)))
	 (MMEC-FUNC2	(intern (format "mmec-2%s" OPERATOR)))
	 (DOCSTRING	(format "Return true if every operand is %s to the one following it; otherwise return false." OPERATOR))
	 (DOCSTRING2	(format "Return true if OP1 %s OP2; otherwise return false." OPERATOR)))
    `(progn
       (defun ,MMEC-FUNC (op &rest ops)
    	 ,DOCSTRING
	 ;;FIXME Should I rewrite this to use `cl-loop'?  (Marco Maggi; Feb 5, 2020)
	 (let ((rv t))
	   (while ops
	     (let ((item (car ops)))
	       (if (,MMEC-FUNC2 op item)
		   (progn
		     (setq op item)
		     (setq ops (cdr ops)))
		 (progn
		   (setq rv  nil)
		   (setq ops nil)))))
	   rv))

       (cl-defgeneric ,MMEC-FUNC2 (op1 op2)
	 ,DOCSTRING2)
       )))

(defmacro mmec--def-numeric-compar-method (MMEC-FUNC2 OPERATOR OPERATION TYPE1 CONVERTER1 TYPE2 CONVERTER2)
  ;;Define  a  comparison  methods that  converts  the  operands  and  then invokes  an  appropriate
  ;;operation.  Examples:
  ;;
  ;; (mmec--def-numeric-compar-method mmec-=2 = mmec-=2 integer mmec-sint64 mmec-float mmec-ldouble)
  ;; ==> (cl-defmethod mmec-=2 ((op1 integer) (op2 mmec-float))
  ;;       "..."
  ;;       (mmec-=2 (mmec-sint64 op1) (mmec-ldouble op2)))
  ;;
  ;; (mmec--def-numeric-compar-method mmec-=2 = = integer identity integer identity)
  ;; ==> (cl-defmethod mmec-=2 ((op1 integer) (op2 integer))
  ;;       "..."
  ;;       (= op1 op2))
  ;;
  (let* ((OPERATOR.str	(symbol-name OPERATOR))
	 (TYPE1.str	(symbol-name TYPE1))
	 (TYPE2.str	(symbol-name TYPE2))
	 (DOCSTRING	(format "Return true if OP1 %s OP2; otherwise return false.

The argument OP1 must be of type `%s'.

The argument OP2 must be of type `%s'.
" OPERATOR TYPE1 TYPE2))
	 (CONVERSION1	(if (eq CONVERTER1 'identity) 'op1 `(,CONVERTER1 op1)))
	 (CONVERSION2	(if (eq CONVERTER2 'identity) 'op2 `(,CONVERTER2 op2))))
    `(cl-defmethod ,MMEC-FUNC2 ((op1 ,TYPE1) (op2 ,TYPE2))
       ,DOCSTRING
       (,OPERATION ,CONVERSION1 ,CONVERSION2))))

(defmacro mmec--define-numeric-comparison (OPERATOR)
  ;;Define everything needed to perform a comparison operation among exact integers.
  ;;
  (let* ((MMEC-FUNC2			(intern (format "mmec-2%s" OPERATOR)))
	 (OPERATION-SINT64		(intern (format "mmec-c-sint64%s" OPERATOR)))
	 (OPERATION-UINT64		(intern (format "mmec-c-uint64%s" OPERATOR)))
	 (OPERATION-SINT64-UINT64	(intern (format "mmec-c-sint64-uint64%s" OPERATOR)))
	 (OPERATION-UINT64-SINT64	(intern (format "mmec-c-uint64-sint64%s" OPERATOR)))
	 (OPERATION-LDOUBLE		(intern (format "mmec-c-ldouble%s" OPERATOR))))
    `(progn
       (mmec--define-numeric-comparison-generic-functions ,OPERATOR)

       ;; These are the methods that actually do the operation on built-in numeric objects.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATOR integer identity integer identity)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATOR integer identity float   identity)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATOR float   identity integer identity)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATOR float   identity float   identity)

       ;; These are the methods that actually do the operation on custom user-pointer objects.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATION-SINT64        mmec-sint64 mmec-sint64-obj mmec-sint64 mmec-sint64-obj)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATION-UINT64        mmec-uint64 mmec-uint64-obj mmec-uint64 mmec-uint64-obj)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATION-SINT64-UINT64 mmec-sint64 mmec-sint64-obj mmec-uint64 mmec-uint64-obj)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATION-UINT64-SINT64 mmec-uint64 mmec-uint64-obj mmec-sint64 mmec-sint64-obj)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,OPERATION-LDOUBLE
					mmec-ldouble mmec-ldouble-obj
					mmec-ldouble mmec-ldouble-obj)

       ;; These are the methods that normalise operands among operational types.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-sint64 mmec-ldouble mmec-ldouble identity)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-uint64 mmec-ldouble mmec-ldouble identity)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-ldouble identity mmec-sint64 mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-ldouble identity mmec-uint64 mmec-ldouble)

       ;; These are the methods that normalise among integer types.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-signed-integer   mmec-sint64 mmec-signed-integer   mmec-sint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-unsigned-integer mmec-uint64 mmec-unsigned-integer mmec-uint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-signed-integer   mmec-sint64 mmec-unsigned-integer mmec-uint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-unsigned-integer mmec-uint64 mmec-signed-integer   mmec-sint64)

       ;; These are the methods that normalise among floating point types.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-floating-point mmec-ldouble mmec-floating-point mmec-ldouble)

       ;; These are the methods that normalise mixed numeric types: `mmec-floating-point', `mmec-signed-integer', `mmec-unsigned-intger'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-floating-point   mmec-ldouble mmec-signed-integer   mmec-sint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-signed-integer   mmec-sint64      mmec-floating-point   mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-floating-point   mmec-ldouble mmec-unsigned-integer mmec-uint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-unsigned-integer mmec-uint64      mmec-floating-point   mmec-ldouble)

       ;; These are the methods that normalise mixed numeric types: `integer' and `mmec-floating-point'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 integer           mmec-ldouble mmec-floating-point mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-floating-point mmec-ldouble integer           mmec-ldouble)

       ;; These are the methods that normalise mixed numeric types: `integer' and `mmec-signed-integer'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 integer           mmec-sint64 mmec-signed-integer mmec-sint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-signed-integer mmec-sint64 integer           mmec-sint64)

       ;; These are the methods that normalise mixed numeric types: `integer' and `mmec-unsigned-integer'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 integer             mmec-sint64 mmec-unsigned-integer mmec-uint64)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-unsigned-integer mmec-uint64 integer             mmec-sint64)

       ;; These are the methods that normalise mixed numeric types: `float' and `mmec-floating-point'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 float             mmec-ldouble mmec-floating-point mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-floating-point mmec-ldouble float             mmec-ldouble)

       ;; These are the methods that normalise mixed numeric types: `float' and `mmec-signed-integer'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 float             mmec-ldouble mmec-signed-integer mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-signed-integer mmec-ldouble float             mmec-ldouble)

       ;; These are the methods that normalise mixed numeric types: `float' and `mmec-unsigned-integer'.
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 float               mmec-ldouble mmec-unsigned-integer mmec-ldouble)
       (mmec--def-numeric-compar-method ,MMEC-FUNC2 ,OPERATOR ,MMEC-FUNC2 mmec-unsigned-integer mmec-ldouble float               mmec-ldouble)
       )))

(mmec--define-numeric-comparison =)
(mmec--define-numeric-comparison <)
(mmec--define-numeric-comparison >)
(mmec--define-numeric-comparison <=)
(mmec--define-numeric-comparison >=)
(mmec--define-numeric-comparison /=)


;;;; done

(provide 'mmec-number-objects)

;;; end of file
