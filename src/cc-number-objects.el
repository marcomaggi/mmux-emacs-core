;;; cc-number-objects.el --- numeric type definitions for C language intefaces

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb  6, 2020
;; Time-stamp: <2020-02-16 06:52:36 marco>
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

(require 'cc-basics)
(require 'cc-constants)


;;;; basic numeric type definitions

;; Base type of all the custom number types defined by this module.
(cl-defstruct (cc-number
	       (:constructor	cc-number--make)))

;; Base type of all the custom exact integer types defined by this module.
(cl-defstruct (cc-integer
	       (:include	cc-number)
	       (:constructor	cc-integer--make)))

;; Base type of all the custom exact signed integer number types defined by this module.
(cl-defstruct (cc-signed-integer
	       (:include	cc-integer)
	       (:constructor	cc-signed-integer--make)))

;; Base type of all the custom exact unsigned integer number types defined by this module.
(cl-defstruct (cc-unsigned-integer
	       (:include	cc-integer)
	       (:constructor	cc-unsigned-integer--make)))

;; Base type of all the custom floating-point number types defined by this module.
(cl-defstruct (cc-floating-point
	       (:include	cc-number)
	       (:constructor	cc-floating-point--make)))

;;; --------------------------------------------------------------------

(defmacro cc--define-abstract-type-constructor (TYPE)
  `(defun ,TYPE (&rest args)
     (signal 'mmux-core-instantiating-abstract-type (quote ,TYPE))))

(cc--define-abstract-type-constructor cc-number)
(cc--define-abstract-type-constructor cc-integer)
(cc--define-abstract-type-constructor cc-signed-integer)
(cc--define-abstract-type-constructor cc-unsigned-integer)
(cc--define-abstract-type-constructor cc-floating-point)


;;;; C language type wrappers: char

(cl-defstruct (cc-char
	       (:include	cc-signed-integer)
	       (:constructor	cc-char--make))
  obj)

(cl-defgeneric cc-char (init)
  "Constructor for number objects of type `cc-char'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-char ((init cc-char))
  "Constructor for number objects of type `cc-char'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-char--make :obj (cc-char-obj init)))

(cl-defmethod cc-char ((init cc-signed-integer))
  "Constructor for number objects of type `cc-char'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is  builds an  object of  type `cc-char',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-char-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-char init)))
    (cc-char--make :obj (mmec-c-make-integer-char-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-char ((init integer))
  "Constructor for number objects of type `cc-char'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is  builds an  object of  type `cc-char',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-char-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-char init)))
    (cc-char--make :obj (mmec-c-make-integer-char-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-char ((init float))
  "Constructor for number objects of type `cc-char'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-char',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-char-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-char init)))
    (cc-char--make :obj (mmec-c-make-integer-char-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-char ((init cc-number))
  "Constructor for number objects of type `cc-char'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-char init)))


;;;; C language type wrappers: signed char

(cl-defstruct (cc-schar
	       (:include	cc-signed-integer)
	       (:constructor	cc-schar--make))
  obj)

(cl-defgeneric cc-schar (init)
  "Constructor for number objects of type `cc-schar'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-schar ((init cc-schar))
  "Constructor for number objects of type `cc-schar'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-schar--make :obj (cc-schar-obj init)))

(cl-defmethod cc-schar ((init cc-signed-integer))
  "Constructor for number objects of type `cc-schar'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-schar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-schar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-schar init)))
    (cc-schar--make :obj (mmec-c-make-integer-schar-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-schar ((init integer))
  "Constructor for number objects of type `cc-schar'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-schar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-schar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-schar init)))
    (cc-schar--make :obj (mmec-c-make-integer-schar-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-schar ((init float))
  "Constructor for number objects of type `cc-schar'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-schar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-schar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-schar init)))
    (cc-schar--make :obj (mmec-c-make-integer-schar-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-schar ((init cc-number))
  "Constructor for number objects of type `cc-schar'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-schar init)))


;;;; C language type wrappers: unsigned char

(cl-defstruct (cc-uchar
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uchar--make))
  obj)

(cl-defgeneric cc-uchar (init)
  "Constructor for number objects of type `cc-uchar'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-uchar ((init cc-uchar))
  "Constructor for number objects of type `cc-uchar'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-uchar--make :obj (cc-uchar-obj init)))

(cl-defmethod cc-uchar ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-uchar'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uchar',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uchar init)))
    (cc-uchar--make :obj (mmec-c-make-integer-uchar-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uchar ((init integer))
  "Constructor for number objects of type `cc-uchar'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uchar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uchar init)))
    (cc-uchar--make :obj (mmec-c-make-integer-uchar-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uchar ((init float))
  "Constructor for number objects of type `cc-uchar'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uchar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uchar init)))
    (cc-uchar--make :obj (mmec-c-make-integer-uchar-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uchar ((init cc-number))
  "Constructor for number objects of type `cc-uchar'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-uchar init)))


;;;; C language type wrappers: wchar_t

(cl-defstruct (cc-wchar
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-wchar--make))
  obj)

(cl-defgeneric cc-wchar (init)
  "Constructor for number objects of type `cc-wchar'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-wchar ((init cc-wchar))
  "Constructor for number objects of type `cc-wchar'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-wchar--make :obj (cc-wchar-obj init)))

(cl-defmethod cc-wchar ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-wchar'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-wchar',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-wchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-wchar init)))
    (cc-wchar--make :obj (mmec-c-make-usrptr-wchar-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-wchar ((init integer))
  "Constructor for number objects of type `cc-wchar'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uchar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-wchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-wchar init)))
    (cc-wchar--make :obj (mmec-c-make-usrptr-wchar-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-wchar ((init float))
  "Constructor for number objects of type `cc-wchar'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uchar',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-wchar-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-wchar init)))
    (cc-wchar--make :obj (mmec-c-make-usrptr-wchar-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-wchar ((init cc-number))
  "Constructor for number objects of type `cc-wchar'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-wchar init)))


;;;; C language type wrappers: signed short int

(cl-defstruct (cc-sshrt
	       (:include	cc-signed-integer)
	       (:constructor	cc-sshrt--make))
  obj)

(cl-defgeneric cc-sshrt (init)
  "Constructor for number objects of type `cc-sshrt'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-sshrt ((init cc-sshrt))
  "Constructor for number objects of type `cc-sshrt'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-sshrt--make :obj (cc-sshrt-obj init)))

(cl-defmethod cc-sshrt ((init cc-signed-integer))
  "Constructor for number objects of type `cc-sshrt'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sshrt',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sshrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sshrt init)))
    (cc-sshrt--make :obj (mmec-c-make-integer-sshrt-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sshrt ((init integer))
  "Constructor for number objects of type `cc-sshrt'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sshrt',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sshrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sshrt init)))
    (cc-sshrt--make :obj (mmec-c-make-integer-sshrt-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sshrt ((init float))
  "Constructor for number objects of type `cc-sshrt'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sshrt',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sshrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sshrt init)))
    (cc-sshrt--make :obj (mmec-c-make-integer-sshrt-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sshrt ((init cc-number))
  "Constructor for number objects of type `cc-sshrt'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-sshrt init)))


;;;; C language type wrappers: unsigned short int

(cl-defstruct (cc-ushrt
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-ushrt--make))
  obj)

(cl-defgeneric cc-ushrt (init)
  "Constructor for number objects of type `cc-ushrt'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-ushrt ((init cc-ushrt))
  "Constructor for number objects of type `cc-ushrt'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-ushrt--make :obj (cc-ushrt-obj init)))

(cl-defmethod cc-ushrt ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-ushrt'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ushrt',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-ushrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ushrt init)))
    (cc-ushrt--make :obj (mmec-c-make-integer-ushrt-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-ushrt ((init integer))
  "Constructor for number objects of type `cc-ushrt'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ushrt',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-ushrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ushrt init)))
    (cc-ushrt--make :obj (mmec-c-make-integer-ushrt-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-ushrt ((init float))
  "Constructor for number objects of type `cc-ushrt'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ushrt',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-ushrt-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ushrt init)))
    (cc-ushrt--make :obj (mmec-c-make-integer-ushrt-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-ushrt ((init cc-number))
  "Constructor for number objects of type `cc-ushrt'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-ushrt init)))


;;;; C language type wrappers: signed int

(cl-defstruct (cc-sint
	       (:include	cc-signed-integer)
	       (:constructor	cc-sint--make))
  obj)

(cl-defgeneric cc-sint (init)
  "Constructor for number objects of type `cc-sint'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-sint ((init cc-sint))
  "Constructor for number objects of type `cc-sint'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-sint--make :obj (cc-sint-obj init)))

(cl-defmethod cc-sint ((init cc-signed-integer))
  "Constructor for number objects of type `cc-sint'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sint',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint init)))
    (cc-sint--make :obj (mmec-c-make-usrptr-sint-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint ((init integer))
  "Constructor for number objects of type `cc-sint'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sint',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint init)))
    (cc-sint--make :obj (mmec-c-make-usrptr-sint-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint ((init float))
  "Constructor for number objects of type `cc-sint'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sint',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint init)))
    (cc-sint--make :obj (mmec-c-make-usrptr-sint-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint ((init cc-number))
  "Constructor for number objects of type `cc-sint'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-sint init)))


;;;; C language type wrappers: unsigned int

(cl-defstruct (cc-uint
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uint--make))
  obj)

(cl-defgeneric cc-uint (init)
  "Constructor for number objects of type `cc-uint'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-uint ((init cc-uint))
  "Constructor for number objects of type `cc-uint'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-uint--make :obj (cc-uint-obj init)))

(cl-defmethod cc-uint ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-uint'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uint',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint init)))
    (cc-uint--make :obj (mmec-c-make-usrptr-uint-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint ((init integer))
  "Constructor for number objects of type `cc-uint'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uint',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint init)))
    (cc-uint--make :obj (mmec-c-make-usrptr-uint-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint ((init float))
  "Constructor for number objects of type `cc-uint'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uint',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint init)))
    (cc-uint--make :obj (mmec-c-make-usrptr-uint-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint ((init cc-number))
  "Constructor for number objects of type `cc-uint'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-uint init)))


;;;; C language type wrappers: signed long int

(cl-defstruct (cc-slong
	       (:include	cc-signed-integer)
	       (:constructor	cc-slong--make))
  obj)

(cl-defgeneric cc-slong (init)
  "Constructor for number objects of type `cc-slong'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-slong ((init cc-slong))
  "Constructor for number objects of type `cc-slong'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-slong--make :obj (cc-slong-obj init)))

(cl-defmethod cc-slong ((init cc-signed-integer))
  "Constructor for number objects of type `cc-slong'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-slong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-slong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-slong init)))
    (cc-slong--make :obj (mmec-c-make-usrptr-slong-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-slong ((init integer))
  "Constructor for number objects of type `cc-slong'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-slong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-slong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-slong init)))
    (cc-slong--make :obj (mmec-c-make-usrptr-slong-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-slong ((init float))
  "Constructor for number objects of type `cc-slong'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-slong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-slong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-slong init)))
    (cc-slong--make :obj (mmec-c-make-usrptr-slong-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-slong ((init cc-number))
  "Constructor for number objects of type `cc-slong'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-slong init)))


;;;; C language type wrappers: unsigned long int

(cl-defstruct (cc-ulong
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-ulong--make))
  obj)

(cl-defgeneric cc-ulong (init)
  "Constructor for number objects of type `cc-ulong'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-ulong ((init cc-ulong))
  "Constructor for number objects of type `cc-ulong'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-ulong--make :obj (cc-ulong-obj init)))

(cl-defmethod cc-ulong ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-ulong'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ulong',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-ulong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ulong init)))
    (cc-ulong--make :obj (mmec-c-make-usrptr-ulong-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-ulong ((init integer))
  "Constructor for number objects of type `cc-ulong'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ulong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-ulong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ulong init)))
    (cc-ulong--make :obj (mmec-c-make-usrptr-ulong-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-ulong ((init float))
  "Constructor for number objects of type `cc-ulong'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ulong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-ulong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ulong init)))
    (cc-ulong--make :obj (mmec-c-make-usrptr-ulong-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-ulong ((init cc-number))
  "Constructor for number objects of type `cc-ulong'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-ulong init)))


;;;; C language type wrappers: signed long long int

(cl-defstruct (cc-sllong
	       (:include	cc-signed-integer)
	       (:constructor	cc-sllong--make))
  obj)

(cl-defgeneric cc-sllong (init)
  "Constructor for number objects of type `cc-sllong'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-sllong ((init cc-sllong))
  "Constructor for number objects of type `cc-sllong'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-sllong--make :obj (cc-sllong-obj init)))

(cl-defmethod cc-sllong ((init cc-signed-integer))
  "Constructor for number objects of type `cc-sllong'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sllong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sllong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sllong init)))
    (cc-sllong--make :obj (mmec-c-make-usrptr-sllong-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sllong ((init integer))
  "Constructor for number objects of type `cc-sllong'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sllong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sllong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sllong init)))
    (cc-sllong--make :obj (mmec-c-make-usrptr-sllong-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sllong ((init float))
  "Constructor for number objects of type `cc-sllong'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sllong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sllong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sllong init)))
    (cc-sllong--make :obj (mmec-c-make-usrptr-sllong-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sllong ((init cc-number))
  "Constructor for number objects of type `cc-sllong'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-sllong init)))


;;;; C language type wrappers: unsigned long long int

(cl-defstruct (cc-ullong
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-ullong--make))
  obj)

(cl-defgeneric cc-ullong (init)
  "Constructor for number objects of type `cc-ullong'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-ullong ((init cc-ullong))
  "Constructor for number objects of type `cc-ullong'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-ullong--make :obj (cc-ullong-obj init)))

(cl-defmethod cc-ullong ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-ullong'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ullong',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-ullong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ullong init)))
    (cc-ullong--make :obj (mmec-c-make-usrptr-ullong-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-ullong ((init integer))
  "Constructor for number objects of type `cc-ullong'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ullong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-ullong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ullong init)))
    (cc-ullong--make :obj (mmec-c-make-usrptr-ullong-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-ullong ((init float))
  "Constructor for number objects of type `cc-ullong'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ullong',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-ullong-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ullong init)))
    (cc-ullong--make :obj (mmec-c-make-usrptr-ullong-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-ullong ((init cc-number))
  "Constructor for number objects of type `cc-ullong'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-ullong init)))


;;;; C language type wrappers: intmax_t

(cl-defstruct (cc-sintmax
	       (:include	cc-signed-integer)
	       (:constructor	cc-sintmax--make))
  obj)

(cl-defgeneric cc-sintmax (init)
  "Constructor for number objects of type `cc-sintmax'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-sintmax ((init cc-sintmax))
  "Constructor for number objects of type `cc-sintmax'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-sintmax--make :obj (cc-sintmax-obj init)))

(cl-defmethod cc-sintmax ((init cc-signed-integer))
  "Constructor for number objects of type `cc-sintmax'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sintmax',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sintmax init)))
    (cc-sintmax--make :obj (mmec-c-make-usrptr-sintmax-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sintmax ((init integer))
  "Constructor for number objects of type `cc-sintmax'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sintmax',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sintmax init)))
    (cc-sintmax--make :obj (mmec-c-make-usrptr-sintmax-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sintmax ((init float))
  "Constructor for number objects of type `cc-sintmax'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sintmax',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sintmax init)))
    (cc-sintmax--make :obj (mmec-c-make-usrptr-sintmax-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sintmax ((init cc-number))
  "Constructor for number objects of type `cc-sintmax'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-sintmax init)))


;;;; C language type wrappers: uintmax_t

(cl-defstruct (cc-uintmax
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uintmax--make))
  obj)

(cl-defgeneric cc-uintmax (init)
  "Constructor for number objects of type `cc-uintmax'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-uintmax ((init cc-uintmax))
  "Constructor for number objects of type `cc-uintmax'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-uintmax--make :obj (cc-uintmax-obj init)))

(cl-defmethod cc-uintmax ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-uintmax'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uintmax',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uintmax init)))
    (cc-uintmax--make :obj (mmec-c-make-usrptr-uintmax-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uintmax ((init integer))
  "Constructor for number objects of type `cc-uintmax'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uintmax',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uintmax init)))
    (cc-uintmax--make :obj (mmec-c-make-usrptr-uintmax-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uintmax ((init float))
  "Constructor for number objects of type `cc-uintmax'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uintmax',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uintmax-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uintmax init)))
    (cc-uintmax--make :obj (mmec-c-make-usrptr-uintmax-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uintmax ((init cc-number))
  "Constructor for number objects of type `cc-uintmax'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-uintmax init)))


;;;; C language type wrappers: ssize_t

(cl-defstruct (cc-ssize
	       (:include	cc-signed-integer)
	       (:constructor	cc-ssize--make))
  obj)

(cl-defgeneric cc-ssize (init)
  "Constructor for number objects of type `cc-ssize'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-ssize ((init cc-ssize))
  "Constructor for number objects of type `cc-ssize'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-ssize--make :obj (cc-ssize-obj init)))

(cl-defmethod cc-ssize ((init cc-signed-integer))
  "Constructor for number objects of type `cc-ssize'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ssize',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-ssize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ssize init)))
    (cc-ssize--make :obj (mmec-c-make-usrptr-ssize-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-ssize ((init integer))
  "Constructor for number objects of type `cc-ssize'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ssize',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-ssize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ssize init)))
    (cc-ssize--make :obj (mmec-c-make-usrptr-ssize-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-ssize ((init float))
  "Constructor for number objects of type `cc-ssize'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-ssize',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-ssize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ssize init)))
    (cc-ssize--make :obj (mmec-c-make-usrptr-ssize-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-ssize ((init cc-number))
  "Constructor for number objects of type `cc-ssize'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-ssize init)))


;;;; C language type wrappers: size_t

(cl-defstruct (cc-usize
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-usize--make))
  obj)

(cl-defgeneric cc-usize (init)
  "Constructor for number objects of type `cc-usize'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-usize ((init cc-usize))
  "Constructor for number objects of type `cc-usize'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-usize--make :obj (cc-usize-obj init)))

(cl-defmethod cc-usize ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-usize'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-usize',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-usize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-usize init)))
    (cc-usize--make :obj (mmec-c-make-usrptr-usize-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-usize ((init integer))
  "Constructor for number objects of type `cc-usize'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-usize',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-usize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-usize init)))
    (cc-usize--make :obj (mmec-c-make-usrptr-usize-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-usize ((init float))
  "Constructor for number objects of type `cc-usize'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-usize',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-usize-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-usize init)))
    (cc-usize--make :obj (mmec-c-make-usrptr-usize-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-usize ((init cc-number))
  "Constructor for number objects of type `cc-usize'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-usize init)))


;;;; C language type wrappers: ptrdiff_t

(cl-defstruct (cc-ptrdiff
	       (:include	cc-signed-integer)
	       (:constructor	cc-ptrdiff--make))
  obj)

(cl-defgeneric cc-ptrdiff (init)
  "Constructor for number objects of type `cc-ptrdiff'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-ptrdiff ((init cc-ptrdiff))
  "Constructor for number objects of type `cc-ptrdiff'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-ptrdiff--make :obj (cc-ptrdiff-obj init)))

(cl-defmethod cc-ptrdiff ((init cc-signed-integer))
  "Constructor for number objects of type `cc-ptrdiff'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it is  is builds an object of type  `cc-ptrdiff', otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-ptrdiff-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ptrdiff init)))
    (cc-ptrdiff--make :obj (mmec-c-make-usrptr-ptrdiff-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-ptrdiff ((init integer))
  "Constructor for number objects of type `cc-ptrdiff'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it is  is builds an object of type  `cc-ptrdiff', otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-ptrdiff-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ptrdiff init)))
    (cc-ptrdiff--make :obj (mmec-c-make-usrptr-ptrdiff-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-ptrdiff ((init float))
  "Constructor for number objects of type `cc-ptrdiff'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it is  is builds an object of type  `cc-ptrdiff', otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-ptrdiff-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-ptrdiff init)))
    (cc-ptrdiff--make :obj (mmec-c-make-usrptr-ptrdiff-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-ptrdiff ((init cc-number))
  "Constructor for number objects of type `cc-ptrdiff'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-ptrdiff init)))


;;;; C language type wrappers: int8_t

(cl-defstruct (cc-sint8
	       (:include	cc-signed-integer)
	       (:constructor	cc-sint8--make))
  obj)

(cl-defgeneric cc-sint8 (init)
  "Constructor for number objects of type `cc-sint8'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-sint8 ((init cc-sint8))
  "Constructor for number objects of type `cc-sint8'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-sint8--make :obj (cc-sint8-obj init)))

(cl-defmethod cc-sint8 ((init cc-signed-integer))
  "Constructor for number objects of type `cc-sint8'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sint8',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint8 init)))
    (cc-sint8--make :obj (mmec-c-make-integer-sint8-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint8 ((init integer))
  "Constructor for number objects of type `cc-sint8'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sint8',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint8 init)))
    (cc-sint8--make :obj (mmec-c-make-integer-sint8-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint8 ((init float))
  "Constructor for number objects of type `cc-sint8'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sint8',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint8 init)))
    (cc-sint8--make :obj (mmec-c-make-integer-sint8-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint8 ((init cc-number))
  "Constructor for number objects of type `cc-sint8'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-sint8 init)))


;;;; C language type wrappers: uint8_t

(cl-defstruct (cc-uint8
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uint8--make))
  obj)

(cl-defgeneric cc-uint8 (init)
  "Constructor for number objects of type `cc-uint8'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-uint8 ((init cc-uint8))
  "Constructor for number objects of type `cc-uint8'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-uint8--make :obj (cc-uint8-obj init)))

(cl-defmethod cc-uint8 ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-uint8'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uint8',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint8 init)))
    (cc-uint8--make :obj (mmec-c-make-integer-uint8-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint8 ((init integer))
  "Constructor for number objects of type `cc-uint8'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uint8',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint8 init)))
    (cc-uint8--make :obj (mmec-c-make-integer-uint8-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint8 ((init float))
  "Constructor for number objects of type `cc-uint8'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uint8',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint8-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint8 init)))
    (cc-uint8--make :obj (mmec-c-make-integer-uint8-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint8 ((init cc-number))
  "Constructor for number objects of type `cc-uint8'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-uint8 init)))


;;;; C language type wrappers: int16_t

(cl-defstruct (cc-sint16
	       (:include	cc-signed-integer)
	       (:constructor	cc-sint16--make))
  obj)

(cl-defgeneric cc-sint16 (init)
  "Constructor for number objects of type `cc-sint16'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-sint16 ((init cc-sint16))
  "Constructor for number objects of type `cc-sint16'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-sint16--make :obj (cc-sint16-obj init)))

(cl-defmethod cc-sint16 ((init cc-signed-integer))
  "Constructor for number objects of type `cc-sint16'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds an  object of type `cc-sint16',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint16 init)))
    (cc-sint16--make :obj (mmec-c-make-integer-sint16-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint16 ((init integer))
  "Constructor for number objects of type `cc-sint16'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sint16',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint16 init)))
    (cc-sint16--make :obj (mmec-c-make-integer-sint16-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint16 ((init float))
  "Constructor for number objects of type `cc-sint16'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sint16',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint16 init)))
    (cc-sint16--make :obj (mmec-c-make-integer-sint16-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint16 ((init cc-number))
  "Constructor for number objects of type `cc-sint16'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-sint16 init)))


;;;; C language type wrappers: uint16_t

(cl-defstruct (cc-uint16
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uint16--make))
  obj)

(cl-defgeneric cc-uint16 (init)
  "Constructor for number objects of type `cc-uint16'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-uint16 ((init cc-uint16))
  "Constructor for number objects of type `cc-uint16'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-uint16--make :obj (cc-uint16-obj init)))

(cl-defmethod cc-uint16 ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-uint16'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds an  object of type `cc-uint16',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint16 init)))
    (cc-uint16--make :obj (mmec-c-make-integer-uint16-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint16 ((init integer))
  "Constructor for number objects of type `cc-uint16'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uint16',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint16 init)))
    (cc-uint16--make :obj (mmec-c-make-integer-uint16-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint16 ((init float))
  "Constructor for number objects of type `cc-uint16'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uint16',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint16-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint16 init)))
    (cc-uint16--make :obj (mmec-c-make-integer-uint16-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint16 ((init cc-number))
  "Constructor for number objects of type `cc-uint16'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-uint16 init)))


;;;; C language type wrappers: int32_t

(cl-defstruct (cc-sint32
	       (:include	cc-signed-integer)
	       (:constructor	cc-sint32--make))
  obj)

(cl-defgeneric cc-sint32 (init)
  "Constructor for number objects of type `cc-sint32'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-sint32 ((init cc-sint32))
  "Constructor for number objects of type `cc-sint32'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-sint32--make :obj (cc-sint32-obj init)))

(cl-defmethod cc-sint32 ((init cc-signed-integer))
  "Constructor for number objects of type `cc-sint32'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds an  object of type `cc-sint32',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint32 init)))
    (cc-sint32--make :obj (mmec-c-make-usrptr-sint32-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint32 ((init integer))
  "Constructor for number objects of type `cc-sint32'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sint32',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint32 init)))
    (cc-sint32--make :obj (mmec-c-make-usrptr-sint32-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint32 ((init float))
  "Constructor for number objects of type `cc-sint32'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-sint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-sint32',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-sint64 init)))
    (unless (cc-fits-sint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-sint32 init)))
    (cc-sint32--make :obj (mmec-c-make-usrptr-sint32-from-usrptr-sint64 (cc-sint64-obj obj)))))

(cl-defmethod cc-sint32 ((init cc-number))
  "Constructor for number objects of type `cc-sint32'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-sint32 init)))


;;;; C language type wrappers: uint32_t

(cl-defstruct (cc-uint32
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uint32--make))
  obj)

(cl-defgeneric cc-uint32 (init)
  "Constructor for number objects of type `cc-uint32'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-uint32 ((init cc-uint32))
  "Constructor for number objects of type `cc-uint32'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-uint32--make :obj (cc-uint32-obj init)))

(cl-defmethod cc-uint32 ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-uint32'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds an  object of type `cc-uint32',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint32 init)))
    (cc-uint32--make :obj (mmec-c-make-usrptr-uint32-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint32 ((init integer))
  "Constructor for number objects of type `cc-uint32'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uint32',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint32 init)))
    (cc-uint32--make :obj (mmec-c-make-usrptr-uint32-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint32 ((init float))
  "Constructor for number objects of type `cc-uint32'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-uint64', the it  checks if the range is valid:
if it  is is builds  an object  of type `cc-uint32',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-uint64 init)))
    (unless (cc-fits-uint32-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-uint32 init)))
    (cc-uint32--make :obj (mmec-c-make-usrptr-uint32-from-usrptr-uint64 (cc-uint64-obj obj)))))

(cl-defmethod cc-uint32 ((init cc-number))
  "Constructor for number objects of type `cc-uint32'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-uint32 init)))


;;;; C language type wrappers: sint64_t
;;
;;The custom number type `cc-sint64' is special  because it is used for normalised representation of
;;all the unsigned integer numbers, both built-in and custom.
;;

(cl-defstruct (cc-sint64
	       (:include	cc-signed-integer)
	       (:constructor	cc-sint64--make))
  obj)

(cl-defgeneric cc-sint64 (init)
  "Constructor for number objects of type `cc-sint64'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-sint64 ((init cc-sint64))
  "Constructor for number objects of type `cc-sint64'.

The argument INIT is an instance  of type `cc-sit64': this is the
copy constructor  implemented as  method.  This method  creates a
duplicate  of  the  elisp  object, but  it  reuses  the  internal
representation (which is immutable)."
  (cc--make cc-sint64 :obj (cc--extract-obj cc-sint64 init)))

(cl-defmethod cc-sint64 ((init integer))
  "Constructor for number objects of type `cc-sint64'.

The argument INIT must be a  value of type `integer'."
  (cc--make sint64 :obj (mmec-c-make-usrptr-sint64-from-elisp-integer init)))

(cl-defmethod cc-sint64 ((init float))
  "Constructor for number objects of type `cc-sint64'.

The argument INIT must be a  value of type `float'."
  (cc--make sint64 :obj (mmec-c-make-usrptr-sint64-from-elisp-float init)))

(cl-defmethod cc-sint64 ((init cc-number))
  "Constructor for number objects of type `cc-sint64'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-sint64 init)))

(defmacro cc--define-sint64-constructor-method-for-integer-intrep-init (INIT-TYPE-OR-STEM)
  "Expand into a `cl-defmethod' form defining a constructor method for `cc-sint64' values.

The argument  INIT-TYPE-OR-STEM must  be a symbol  representing a
type name or  type stem name; this type must  be a signed integer
with `integer' as internal representation."
  (let* ((INIT-TYPE	(intern (cc--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
	 (DOCSTRING	(format "Constructor for number objects of type `cc-sint64'.

The argument INIT must be an object of type `%s'." INIT-TYPE)))
    `(cl-defmethod cc-sint64 ((init ,INIT-TYPE))
       ,DOCSTRING
       (cc--make sint64 :obj (mmec-c-make-usrptr-sint64-from-elisp-integer (cc--extract-obj ,INIT-TYPE init))))))

(cc--define-sint64-constructor-method-for-integer-intrep-init char)
(cc--define-sint64-constructor-method-for-integer-intrep-init schar)
(cc--define-sint64-constructor-method-for-integer-intrep-init sshrt)
(cc--define-sint64-constructor-method-for-integer-intrep-init sint8)
(cc--define-sint64-constructor-method-for-integer-intrep-init sint16)

(defmacro cc--define-sint64-constructor-method-for-usrptr-intrep-init (INIT-TYPE-OR-STEM)
  "Expand into a `cl-defmethod' form defining a constructor method for `cc-sint64' values.

The argument  INIT-TYPE-OR-STEM must  be a symbol  representing a
type name or  type stem name; this type must  be a signed integer
with a user-pointer object as internal representation."
  (let* ((INIT-TYPE		(intern (cc--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
	 (CLANG-CONSTRUCTOR	(intern (format "mmec-c-make-usrptr-sint64-from-usrptr-%s" INIT-TYPE-OR-STEM)))
	 (DOCSTRING		(format "Constructor for number objects of type `cc-sint64'.

The argument INIT must be an object of type `%s'." INIT-TYPE)))
    `(cl-defmethod cc-sint64 ((init ,INIT-TYPE))
       ,DOCSTRING
       (cc--make sint64 :obj (,CLANG-CONSTRUCTOR (cc--extract-obj ,INIT-TYPE init))))))

(cc--define-sint64-constructor-method-for-usrptr-intrep-init sint)
(cc--define-sint64-constructor-method-for-usrptr-intrep-init slong)
(cc--define-sint64-constructor-method-for-usrptr-intrep-init sllong)
(cc--define-sint64-constructor-method-for-usrptr-intrep-init sintmax)
(cc--define-sint64-constructor-method-for-usrptr-intrep-init ssize)
(cc--define-sint64-constructor-method-for-usrptr-intrep-init sint32)
(cc--define-sint64-constructor-method-for-usrptr-intrep-init ptrdiff)


;;;; C language type wrappers: uint64_t
;;
;;The custom number type `cc-uint64' is special  because it is used for normalised representation of
;;all the unsigned integer numbers, both built-in and custom.
;;

(cl-defstruct (cc-uint64
	       (:include	cc-unsigned-integer)
	       (:constructor	cc-uint64--make))
  obj)

(cl-defgeneric cc-uint64 (init)
  "Constructor for number objects of type `cc-uint64'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-uint64 ((init cc-uint64))
  "Constructor for number objects of type `cc-uint64'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc--make cc-uint64 :obj (cc--extract-obj cc-uint64 init)))

(cl-defmethod cc-uint64 ((init integer))
  "Constructor for number objects of type `cc-uint64'.

This  constructor method  accepts  as  initialisation argument  a
value of the Emacs's built-in type `integer'."
  (cc--make uint64 :obj (mmec-c-make-usrptr-uint64-from-elisp-integer init)))

(cl-defmethod cc-uint64 ((init float))
  "Constructor for number objects of type `cc-uint64'.

This  constructor method  accepts  as  initialisation argument  a
value  of the  Emacs's built-in  type `float'.   If the  value is
negative:   the   condition  `mmec-error-value-out-of-range'   is
raised."
  (when (> 0.0 init)
    (signal 'mmec-error-value-out-of-range (list 'cc-uint64 init)))
  (cc--make uint64 :obj (mmec-c-make-usrptr-uint64-from-elisp-float init)))

(cl-defmethod cc-uint64 ((init cc-number))
  "Constructor for number objects of type `cc-uint64'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-uint64 init)))

(defmacro cc--define-uint64-constructor-method-for-integer-init (INIT-TYPE-OR-STEM)
  (let* ((INIT-TYPE	(intern (cc--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
	 (DOCSTRING	(format "Constructor for number objects of type `cc-uint64'.

This  constructor method  accepts as  initialisation argument  an
instance of type `%s'." INIT-TYPE)))
    `(cl-defmethod cc-uint64 ((init ,INIT-TYPE))
       ,DOCSTRING
       (cc--make uint64 :obj (mmec-c-make-usrptr-uint64-from-elisp-integer (cc--extract-obj ,INIT-TYPE init))))))

(cc--define-uint64-constructor-method-for-integer-init uchar)
(cc--define-uint64-constructor-method-for-integer-init ushrt)
(cc--define-uint64-constructor-method-for-integer-init uint8)
(cc--define-uint64-constructor-method-for-integer-init uint16)

(defmacro cc--define-uint64-constructor-method-for-usrptr-init (INIT-TYPE-OR-STEM)
  (let* ((INIT-TYPE		(intern (cc--prepend-prefix-to-symbol-name INIT-TYPE-OR-STEM)))
	 (CLANG-CONSTRUCTOR	(intern (format "mmec-c-make-usrptr-uint64-from-usrptr-%s" INIT-TYPE-OR-STEM)))
	 (DOCSTRING		(format "Constructor for number objects of type `cc-uint64'.

This  constructor method  accepts as  initialisation argument  an
instance of type `%s'." INIT-TYPE)))
    `(cl-defmethod cc-uint64 ((init ,INIT-TYPE))
       ,DOCSTRING
       (cc--make uint64 :obj (,CLANG-CONSTRUCTOR  (cc--extract-obj ,INIT-TYPE init))))))

(cc--define-uint64-constructor-method-for-usrptr-init wchar)
(cc--define-uint64-constructor-method-for-usrptr-init uint)
(cc--define-uint64-constructor-method-for-usrptr-init ulong)
(cc--define-uint64-constructor-method-for-usrptr-init ullong)
(cc--define-uint64-constructor-method-for-usrptr-init uintmax)
(cc--define-uint64-constructor-method-for-usrptr-init usize)
(cc--define-uint64-constructor-method-for-usrptr-init uint32)


;;;; C language type wrappers: float

(cl-defstruct (cc-float
	       (:include	cc-floating-point)
	       (:constructor	cc-float--make))
  obj)

(cl-defgeneric cc-float (init)
  "Constructor for number objects of type `cc-float'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-float ((init cc-float))
  "Constructor for number objects of type `cc-float'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-float--make :obj (cc-float-obj init)))

(cl-defmethod cc-float ((init cc-integer))
  "Constructor for number objects of type `cc-float'.

The argument INIT is a value of type `cc-integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-ldouble', the it checks if the range is valid:
if it  is is builds  an object  of type `cc-float',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-ldouble init)))
    (unless (cc-fits-float-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-float init)))
    (cc-float--make :obj (mmec-c-make-usrptr-float-from-usrptr-ldouble (cc-ldouble-obj obj)))))

(cl-defmethod cc-float ((init integer))
  "Constructor for number objects of type `cc-float'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-ldouble', the it checks if the range is valid:
if it  is is builds  an object  of type `cc-float',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-ldouble init)))
    (unless (cc-fits-float-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-float init)))
    (cc-float--make :obj (mmec-c-make-usrptr-float-from-usrptr-ldouble (cc-ldouble-obj obj)))))

(cl-defmethod cc-float ((init float))
  "Constructor for number objects of type `cc-float'.

This constructor  accepts as  initialisation argument a  value of
type `float'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-ldouble', the it checks if the range is valid:
if it  is is builds  an object  of type `cc-float',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-ldouble init)))
    (unless (cc-fits-float-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-float init)))
    (cc-float--make :obj (mmec-c-make-usrptr-float-from-usrptr-ldouble (cc-ldouble-obj obj)))))

(cl-defmethod cc-float ((init cc-number))
  "Constructor for number objects of type `cc-float'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-float init)))


;;;; C language type wrappers: double float

(cl-defstruct (cc-double
	       (:include	cc-floating-point)
	       (:constructor	cc-double--make))
  obj)

(cl-defgeneric cc-double (init)
  "Constructor for number objects of type `cc-double'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-double ((init cc-double))
  "Constructor for number objects of type `cc-double'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc-double--make :obj (cc-double-obj init)))

(cl-defmethod cc-double ((init cc-signed-integer))
  "Constructor for number objects of type `cc-double'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-ldouble', the it checks if the range is valid:
if it  is is builds an  object of type `cc-double',  otherwise is
raised the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-ldouble init)))
    (unless (cc-fits-double-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-double init)))
    (cc-double--make :obj (mmec-c-make-elisp-float-from-usrptr-ldouble (cc-ldouble-obj obj)))))

(cl-defmethod cc-double ((init integer))
  "Constructor for number objects of type `cc-double'.

This constructor  accepts as  initialisation argument a  value of
type `integer'.

This  constructor normalises  the initialisation  argument to  an
object of type `cc-ldouble', the it checks if the range is valid:
if it  is is builds an  object of type `cc-double',  otherwise it
raises the error condition `mmec-error-value-out-of-range'."
  (let ((obj (cc-ldouble init)))
    (unless (cc-fits-double-p obj)
      (signal 'mmec-error-value-out-of-range (list 'cc-double init)))
    (cc-double--make :obj (mmec-c-make-elisp-float-from-usrptr-ldouble (cc-ldouble-obj obj)))))

(cl-defmethod cc-double ((init float))
  "Constructor for number objects of type `cc-double'.

This constructor  accepts as  initialisation argument a  value of
type `float'."
  (cc-double--make :obj init))

(cl-defmethod cc-double ((init cc-number))
  "Constructor for number objects of type `cc-double'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-double init)))


;;;; C language type wrappers: long double float
;;
;;The custom number type `cc-ldouble' is special because it is used for normalised representation of
;;all the floating-point numbers, both built-in and custom.
;;

(cl-defstruct (cc-ldouble
	       (:include	cc-floating-point)
	       (:constructor	cc-ldouble--make))
  obj)

(cl-defgeneric cc-ldouble (init)
  "Constructor for number objects of type `cc-ldouble'.

This type constructor is implemented  as a generic function.  The
argument INIT must be a number value.")

(cl-defmethod cc-ldouble ((init cc-ldouble))
  "Constructor for number objects of type `cc-ldouble'.

This is the copy constructor  implemented as method.  This method
creates  a duplicate  of  the  elisp object,  but  it reuses  the
internal representation (which is immutable)."
  (cc--make cc-ldouble :obj (cc--extract-obj cc-ldouble init)))

;;; --------------------------------------------------------------------

(cl-defmethod cc-ldouble ((init integer))
  "Constructor for number objects of type `cc-ldouble'.

This  constructor method  accepts  as  initialisation argument  a
value of the Emacs's built-in type `integer'.  The initialisation
value  is  normalised  to  an   instance  of  `float',  then  the
constructor is recursively applied to the normalised value."
  (cc-ldouble (float init)))

(cl-defmethod cc-ldouble ((init float))
  "Constructor for number objects of type `cc-ldouble'.

This  constructor method  accepts  as  initialisation argument  a
value of the Emacs's built-in type `float'."
  (cc--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-elisp-float init)))

;;; --------------------------------------------------------------------

(cl-defmethod cc-ldouble ((init cc-sint64))
  "Constructor for number objects of type `cc-ldouble'.

This constructor  method initalises the returned  object with the
value from an instance of `cc-sint64'"
  (cc--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-usrptr-sint64 (cc--extract-obj sint64 init))))

(cl-defmethod cc-ldouble ((init cc-uint64))
  "Constructor for number objects of type `cc-ldouble'.

This constructor  method initalises the returned  object with the
value from an instance of `cc-uint64'"
  (cc--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-usrptr-uint64 (cc--extract-obj uint64 init))))

;;; --------------------------------------------------------------------

(cl-defmethod cc-ldouble ((init cc-signed-integer))
  "Constructor for number objects of type `cc-ldouble'.

This   constructor  method   normalises   any   number  of   type
`cc-signed-integer'  to  an  instance  of  `cc-sint64',  then  it
recursively calls the constructor on the result."
  (cc-ldouble (cc-sint64 init)))

(cl-defmethod cc-ldouble ((init cc-unsigned-integer))
  "Constructor for number objects of type `cc-ldouble'.

This   constructor  method   normalises   any   number  of   type
`cc-unsigned-integer'  to an  instance  of  `cc-uint64', then  it
recursively calls the constructor on the result."
  (cc-ldouble (cc-uint64 init)))

;;; --------------------------------------------------------------------

(cl-defmethod cc-ldouble ((init cc-float))
  "Constructor for number objects of type `cc-ldouble'.

The argument INIT is an instance of `cc-float'."
  (cc--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-usrptr-float (cc--extract-obj float init))))

(cl-defmethod cc-ldouble ((init cc-double))
  "Constructor for number objects of type `cc-ldouble'.

The argument INIT is an instance of `cc-double'."
  (cc--make ldouble :obj (mmec-c-make-usrptr-ldouble-from-elisp-float (cc--extract-obj double init))))

;;; --------------------------------------------------------------------

(cl-defmethod cc-ldouble ((init cc-number))
  "Constructor for number objects of type `cc-ldouble'.

This  constructor method  signals that  the given  initialisation
argument is invalid."
  (signal 'mmec-error-unsupported-init-type (list 'cc-ldouble init)))


;;;; range inclusion

(defmacro cc--define-fits-function (TYPE-OR-STEM USRPTR-ARGTYPE NORMALISED-TYPE-OR-STEM)
  (let* ((TYPE-STEM.str		(cc--strip-prefix-from-symbol-name TYPE-OR-STEM))
	 (TYPE			(intern (cc--prepend-prefix-to-symbol-name TYPE-OR-STEM)))
	 (NORMALISED-STEM.str	(cc--strip-prefix-from-symbol-name NORMALISED-TYPE-OR-STEM))
	 (NORMALISED-TYPE	(intern (cc--prepend-prefix-to-symbol-name NORMALISED-TYPE-OR-STEM)))
	 (FUNCNAME		(intern (format "cc-fits-%s-p" TYPE-STEM.str)))
	 (CLANG-FUNCNAME	(intern (format "mmec-c-%s-fits-%s-p" NORMALISED-STEM.str TYPE-STEM.str)))
	 (DOCSTRING		(format "Return true if the argument fits an object of type `%s'." TYPE)))
    `(progn
       (cl-defgeneric ,FUNCNAME (op)
	 ,DOCSTRING)
       (cl-defmethod  ,FUNCNAME ((op ,USRPTR-ARGTYPE))
	 ,DOCSTRING
	 (,CLANG-FUNCNAME (cc--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
       (cl-defmethod  ,FUNCNAME ((op integer))
	 ,DOCSTRING
	 (,CLANG-FUNCNAME (cc--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
       (cl-defmethod  ,FUNCNAME ((op float))
	 ,DOCSTRING
	 (,CLANG-FUNCNAME (cc--extract-obj ,NORMALISED-TYPE (,NORMALISED-TYPE op))))
       )))

(defmacro cc--define-fits-function/signed-integer (TYPE-OR-STEM)
  `(cc--define-fits-function ,TYPE-OR-STEM cc-signed-integer   cc-sint64))

(defmacro cc--define-fits-function/unsigned-integer (TYPE-OR-STEM)
  `(cc--define-fits-function ,TYPE-OR-STEM cc-unsigned-integer cc-uint64))

(defmacro cc--define-fits-function/floating-point (TYPE-OR-STEM)
  `(cc--define-fits-function ,TYPE-OR-STEM cc-floating-point   cc-ldouble))

(cc--define-fits-function/signed-integer	char)
(cc--define-fits-function/signed-integer	schar)
(cc--define-fits-function/unsigned-integer	uchar)
(cc--define-fits-function/unsigned-integer	wchar)
(cc--define-fits-function/signed-integer	sshrt)
(cc--define-fits-function/unsigned-integer	ushrt)
(cc--define-fits-function/signed-integer	sint)
(cc--define-fits-function/unsigned-integer	uint)
(cc--define-fits-function/signed-integer	slong)
(cc--define-fits-function/unsigned-integer	ulong)
(cc--define-fits-function/signed-integer	sllong)
(cc--define-fits-function/unsigned-integer	ullong)
(cc--define-fits-function/signed-integer	ssize)
(cc--define-fits-function/unsigned-integer	usize)
(cc--define-fits-function/signed-integer	sintmax)
(cc--define-fits-function/unsigned-integer	uintmax)
(cc--define-fits-function/signed-integer	ptrdiff)
(cc--define-fits-function/signed-integer	sint8)
(cc--define-fits-function/unsigned-integer	uint8)
(cc--define-fits-function/signed-integer	sint16)
(cc--define-fits-function/unsigned-integer	uint16)
(cc--define-fits-function/signed-integer	sint32)
(cc--define-fits-function/unsigned-integer	uint32)
(cc--define-fits-function/signed-integer	sint64)
(cc--define-fits-function/unsigned-integer	uint64)
(cc--define-fits-function/floating-point	float)
(cc--define-fits-function/floating-point	double)
(cc--define-fits-function/floating-point	ldouble)


;;;; numeric comparison operations
;;
;;To perform a comparison operation we normalise the operands as follows:
;;
;;* We convert all the signed integers to `cc-sint64'.
;;
;;* We convert all the unsigned integers to `cc-uint64'.
;;
;;* We convert all the floating-point numbers to `cc-ldouble'.
;;
;;* When  comparing  integers  and floating-point  numbers  we  convert  all  the integer  types  to
;;  `cc-ldouble'.
;;

(defmacro cc--define-numeric-comparison-generic-functions (OPERATOR)
  (let* ((CC-FUNC	(intern (format "cc%s"   OPERATOR)))
	 (CC-FUNC2	(intern (format "cc-2%s" OPERATOR)))
	 (DOCSTRING	(format "Return true if every operand is %s to the one following it; otherwise return false." OPERATOR))
	 (DOCSTRING2	(format "Return true if OP1 %s OP2; otherwise return false." OPERATOR)))
    `(progn
       (defun ,CC-FUNC (op &rest ops)
    	 ,DOCSTRING
	 ;;FIXME Should I rewrite this to use `cl-loop'?  (Marco Maggi; Feb 5, 2020)
	 (let ((rv t))
	   (while ops
	     (let ((item (car ops)))
	       (if (,CC-FUNC2 op item)
		   (progn
		     (setq op item)
		     (setq ops (cdr ops)))
		 (progn
		   (setq rv  nil)
		   (setq ops nil)))))
	   rv))

       (cl-defgeneric ,CC-FUNC2 (op1 op2)
	 ,DOCSTRING2)
       )))

(defmacro cc--def-numeric-compar-method (CC-FUNC2 OPERATOR OPERATION TYPE1 CONVERTER1 TYPE2 CONVERTER2)
  ;;Define  a  comparison  methods that  converts  the  operands  and  then invokes  an  appropriate
  ;;operation.  Examples:
  ;;
  ;; (cc--def-numeric-compar-method cc-=2 = cc-=2 integer cc-sint64 cc-float cc-ldouble)
  ;; ==> (cl-defmethod cc-=2 ((op1 integer) (op2 cc-float))
  ;;       "..."
  ;;       (cc-=2 (cc-sint64 op1) (cc-ldouble op2)))
  ;;
  ;; (cc--def-numeric-compar-method cc-=2 = = integer identity integer identity)
  ;; ==> (cl-defmethod cc-=2 ((op1 integer) (op2 integer))
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
    `(cl-defmethod ,CC-FUNC2 ((op1 ,TYPE1) (op2 ,TYPE2))
       ,DOCSTRING
       (,OPERATION ,CONVERSION1 ,CONVERSION2))))

(defmacro cc--define-numeric-comparison (OPERATOR)
  ;;Define everything needed to perform a comparison operation among exact integers.
  ;;
  (let* ((CC-FUNC2			(intern (format "cc-2%s" OPERATOR)))
	 (OPERATION-SINT64		(intern (format "mmec-c-sint64%s" OPERATOR)))
	 (OPERATION-UINT64		(intern (format "mmec-c-uint64%s" OPERATOR)))
	 (OPERATION-SINT64-UINT64	(intern (format "mmec-c-sint64-uint64%s" OPERATOR)))
	 (OPERATION-UINT64-SINT64	(intern (format "mmec-c-uint64-sint64%s" OPERATOR)))
	 (OPERATION-LDOUBLE		(intern (format "mmec-c-ldouble%s" OPERATOR))))
    `(progn
       (cc--define-numeric-comparison-generic-functions ,OPERATOR)

       ;; These are the methods that actually do the operation on built-in numeric objects.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATOR integer identity integer identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATOR integer identity float   identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATOR float   identity integer identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATOR float   identity float   identity)

       ;; These are the methods that actually do the operation on custom user-pointer objects.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-SINT64        cc-sint64 cc-sint64-obj cc-sint64 cc-sint64-obj)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-UINT64        cc-uint64 cc-uint64-obj cc-uint64 cc-uint64-obj)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-SINT64-UINT64 cc-sint64 cc-sint64-obj cc-uint64 cc-uint64-obj)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-UINT64-SINT64 cc-uint64 cc-uint64-obj cc-sint64 cc-sint64-obj)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,OPERATION-LDOUBLE
				      cc-ldouble cc-ldouble-obj
				      cc-ldouble cc-ldouble-obj)

       ;; These are the methods that normalise operands among operational types.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-sint64 cc-ldouble cc-ldouble identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-uint64 cc-ldouble cc-ldouble identity)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-ldouble identity cc-sint64 cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-ldouble identity cc-uint64 cc-ldouble)

       ;; These are the methods that normalise among integer types.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer   cc-sint64 cc-signed-integer   cc-sint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64 cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer   cc-sint64 cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64 cc-signed-integer   cc-sint64)

       ;; These are the methods that normalise among floating point types.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point cc-ldouble cc-floating-point cc-ldouble)

       ;; These are the methods that normalise mixed numeric types: `cc-floating-point', `cc-signed-integer', `cc-unsigned-intger'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point   cc-ldouble cc-signed-integer   cc-sint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer   cc-sint64      cc-floating-point   cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point   cc-ldouble cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64      cc-floating-point   cc-ldouble)

       ;; These are the methods that normalise mixed numeric types: `integer' and `cc-floating-point'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 integer           cc-ldouble cc-floating-point cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point cc-ldouble integer           cc-ldouble)

       ;; These are the methods that normalise mixed numeric types: `integer' and `cc-signed-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 integer           cc-sint64 cc-signed-integer cc-sint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer cc-sint64 integer           cc-sint64)

       ;; These are the methods that normalise mixed numeric types: `integer' and `cc-unsigned-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 integer             cc-sint64 cc-unsigned-integer cc-uint64)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-uint64 integer             cc-sint64)

       ;; These are the methods that normalise mixed numeric types: `float' and `cc-floating-point'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 float             cc-ldouble cc-floating-point cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-floating-point cc-ldouble float             cc-ldouble)

       ;; These are the methods that normalise mixed numeric types: `float' and `cc-signed-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 float             cc-ldouble cc-signed-integer cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-signed-integer cc-ldouble float             cc-ldouble)

       ;; These are the methods that normalise mixed numeric types: `float' and `cc-unsigned-integer'.
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 float               cc-ldouble cc-unsigned-integer cc-ldouble)
       (cc--def-numeric-compar-method ,CC-FUNC2 ,OPERATOR ,CC-FUNC2 cc-unsigned-integer cc-ldouble float               cc-ldouble)
       )))

(cc--define-numeric-comparison =)
(cc--define-numeric-comparison <)
(cc--define-numeric-comparison >)
(cc--define-numeric-comparison <=)
(cc--define-numeric-comparison >=)
(cc--define-numeric-comparison /=)


;;;; done

(provide 'cc-number-objects)

;;; end of file
