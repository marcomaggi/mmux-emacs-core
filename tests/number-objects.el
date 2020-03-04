;;; number-objects.el --- dynamic module test

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


;;;; helpers

(defmacro my--unsupported-type-error (TYPE UNSUPPORTED-TYPE INIT)
  `(should (condition-case nil
	       (,TYPE (,UNSUPPORTED-TYPE ,INIT))
	     ((mmec-error-unsupported-init-type)
	      t)
	     (t nil))))

(defmacro my--init-argument-does-not-fit (TYPE INIT)
  `(should (condition-case nil
	       (,TYPE ,INIT)
	     ((mmec-error-value-out-of-range)
	      t)
	     (t nil))))

;;All the number type stems.
;;
;; (mmec--def	char)
;; (mmec--def	schar)
;; (mmec--def	uchar)
;; (mmec--def	wchar)
;; (mmec--def	sshrt)
;; (mmec--def	ushrt)
;; (mmec--def	sint)
;; (mmec--def	uint)
;; (mmec--def	slong)
;; (mmec--def	ulong)
;; (mmec--def	sllong)
;; (mmec--def	ullong)
;; (mmec--def	ssize)
;; (mmec--def	usize)
;; (mmec--def	sintmax)
;; (mmec--def	uintmax)
;; (mmec--def	ptrdiff)
;; (mmec--def	sint8)
;; (mmec--def	uint8)
;; (mmec--def	sint16)
;; (mmec--def	uint16)
;; (mmec--def	sint32)
;; (mmec--def	uint32)
;; (mmec--def	sint64)
;; (mmec--def	uint64)
;; (mmec--def	float)
;; (mmec--def	double)
;; (mmec--def	ldouble))


;;;; constants

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME	(mmec-sformat "mmec-test-%s-min-max-constants" TYPESTEM))
		       (MINVAR		(mmec-sformat "mmec-%s-min" TYPESTEM))
		       (MAXVAR		(mmec-sformat "mmec-%s-max" TYPESTEM)))
		  `(ert-deftest ,TESTNAME ()
		     (should (mmec= (mmec-limit-min ,TYPESTEM) ,MINVAR))
		     (should (mmec= (mmec-limit-max ,TYPESTEM) ,MAXVAR))))))
  (mmec--def char)
  (mmec--def schar)
  (mmec--def uchar)
  (mmec--def wchar)
  (mmec--def sshrt)
  (mmec--def ushrt)
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
  (mmec--def sint8)
  (mmec--def uint8)
  (mmec--def sint16)
  (mmec--def uint16)
  (mmec--def sint32)
  (mmec--def uint32)
  (mmec--def sint64)
  (mmec--def uint64)
  (mmec--def float)
  (mmec--def double)
  (mmec--def ldouble))


;;;; predicates

(ert-deftest mmec-test-fits-predicates ()
  "Test the expansion of the macro `mmec-fits-number-type-p'."
  (should	(mmec-fits-number-type-p mmec-sint	(mmec-sshrt 123)))
  (should (not	(mmec-fits-number-type-p sshrt		mmec-sllong-max)))
  (should	(mmec-fits-number-type-p slong		(mmec-limit-min sint8))))


;;;; construction

(cl-macrolet
    ((mmec--def (TYPESTEM ARGSTEM PROPERTY)
		(let* ((TESTNAME	(mmec-sformat "mmec-test-%s-constructors-%s" TYPESTEM ARGSTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (ARGTYPE		(mmec-sformat "mmec-%s" TYPESTEM))
		       (CONSTRUCTOR	NUMTYPE))
		  ;;(mmec-debug-print TYPESTEM ARGSTEM PROPERTY)
		  (cl-case PROPERTY
		    (fits		`(ert-deftest ,TESTNAME ()
					   (should (mmec-number-type-p ,TYPESTEM (,CONSTRUCTOR (mmec-limit-min ,ARGSTEM))))
					   (should (mmec-number-type-p ,TYPESTEM (,CONSTRUCTOR (mmec-limit-max ,ARGSTEM))))))
		    (no-fits		`(ert-deftest ,TESTNAME ()
					   ;; (should (condition-case exc
					   ;; 	       (progn
					   ;; 		 (,CONSTRUCTOR (mmec-limit-min ,ARGSTEM))
					   ;; 		 (signal 'mmec-test-error (list "Expected exception: no-fits.")))
					   ;; 	     (mmec-error-value-out-of-range	t)))
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

;;; --------------------------------------------------------------------
;;; mmec-char

  (cl-macrolet
      ((mmec--deftest (ARGSTEM PROPERTY) `(mmec--def char ,ARGSTEM ,PROPERTY)))
    (mmec--deftest char		fits)
    (mmec--deftest schar	fits)
    (mmec--deftest uchar	unsupported)
    (mmec--deftest wchar	no-fits)
    (mmec--deftest sshrt	no-fits)
    (mmec--deftest ushrt	unsupported)
    (mmec--deftest sint		no-fits)
    (mmec--deftest uint		unsupported)
    (mmec--deftest slong	no-fits)
    (mmec--deftest ulong	unsupported)
    (mmec--deftest sllong	no-fits)
    (mmec--deftest ullong	unsupported)
    (mmec--deftest ssize	no-fits)
    (mmec--deftest usize	unsupported)
    (mmec--deftest sintmax	no-fits)
    (mmec--deftest uintmax	unsupported)
    (mmec--deftest ptrdiff	no-fits)
    (mmec--deftest sint8	fits)
    (mmec--deftest uint8	unsupported)
    (mmec--deftest sint16	no-fits)
    (mmec--deftest uint16	unsupported)
    (mmec--deftest sint32	no-fits)
    (mmec--deftest uint32	unsupported)
    (mmec--deftest sint64	no-fits)
    (mmec--deftest uint64	unsupported)
    (mmec--deftest float	unsupported)
    (mmec--deftest double	unsupported)
    (mmec--deftest ldouble	unsupported))

;;; --------------------------------------------------------------------
;;; mmec-schar

  (cl-macrolet
      ((mmec--deftest (ARGSTEM PROPERTY) `(mmec--def schar ,ARGSTEM ,PROPERTY)))
    (mmec--deftest char		fits)
    (mmec--deftest schar	fits)
    (mmec--deftest uchar	unsupported)
    (mmec--deftest wchar	no-fits)
    (mmec--deftest sshrt	no-fits)
    (mmec--deftest ushrt	unsupported)
    (mmec--deftest sint		no-fits)
    (mmec--deftest uint		unsupported)
    (mmec--deftest slong	no-fits)
    (mmec--deftest ulong	unsupported)
    (mmec--deftest sllong	no-fits)
    (mmec--deftest ullong	unsupported)
    (mmec--deftest ssize	no-fits)
    (mmec--deftest usize	unsupported)
    (mmec--deftest sintmax	no-fits)
    (mmec--deftest uintmax	unsupported)
    (mmec--deftest ptrdiff	no-fits)
    (mmec--deftest sint8	fits)
    (mmec--deftest uint8	unsupported)
    (mmec--deftest sint16	no-fits)
    (mmec--deftest uint16	unsupported)
    (mmec--deftest sint32	no-fits)
    (mmec--deftest uint32	unsupported)
    (mmec--deftest sint64	no-fits)
    (mmec--deftest uint64	unsupported)
    (mmec--deftest float	unsupported)
    (mmec--deftest double	unsupported)
    (mmec--deftest ldouble	unsupported))

;;; --------------------------------------------------------------------
;;; mmec-uchar

  (cl-macrolet
      ((mmec--deftest (ARGSTEM PROPERTY) `(mmec--def uchar ,ARGSTEM ,PROPERTY)))
    (mmec--deftest char		unsupported)
    (mmec--deftest schar	unsupported)
    (mmec--deftest uchar	fits)
    (mmec--deftest wchar	unsupported)
    (mmec--deftest sshrt	unsupported)
    (mmec--deftest ushrt	no-fits)
    (mmec--deftest sint		unsupported)
    (mmec--deftest uint		no-fits)
    (mmec--deftest slong	unsupported)
    (mmec--deftest ulong	no-fits)
    (mmec--deftest sllong	unsupported)
    (mmec--deftest ullong	no-fits)
    (mmec--deftest ssize	unsupported)
    (mmec--deftest usize	no-fits)
    (mmec--deftest sintmax	unsupported)
    (mmec--deftest uintmax	no-fits)
    (mmec--deftest ptrdiff	unsupported)
    (mmec--deftest sint8	unsupported)
    (mmec--deftest uint8	fits)
    (mmec--deftest sint16	unsupported)
    (mmec--deftest uint16	no-fits)
    (mmec--deftest sint32	unsupported)
    (mmec--deftest uint32	no-fits)
    (mmec--deftest sint64	unsupported)
    (mmec--deftest uint64	no-fits)
    (mmec--deftest float	unsupported)
    (mmec--deftest double	unsupported)
    (mmec--deftest ldouble	unsupported))

;;; --------------------------------------------------------------------
;;; mmec-wchar

  (cl-macrolet
      ((mmec--deftest (ARGSTEM PROPERTY) `(mmec--def wchar ,ARGSTEM ,PROPERTY)))
    (mmec--deftest char		fits)
    (mmec--deftest schar	fits)
    (mmec--deftest uchar	unsupported)
    (mmec--deftest wchar	fits)
    (mmec--deftest sshrt	fits)
    (mmec--deftest ushrt	unsupported)
    (mmec--deftest sint		fits)
    (mmec--deftest uint		unsupported)
    (mmec--deftest slong	no-fits)
    (mmec--deftest ulong	unsupported)
    (mmec--deftest sllong	no-fits)
    (mmec--deftest ullong	unsupported)
    (mmec--deftest ssize	no-fits)
    (mmec--deftest usize	unsupported)
    (mmec--deftest sintmax	no-fits)
    (mmec--deftest uintmax	unsupported)
    (mmec--deftest ptrdiff	no-fits)
    (mmec--deftest sint8	fits)
    (mmec--deftest uint8	unsupported)
    (mmec--deftest sint16	fits)
    (mmec--deftest uint16	unsupported)
    (mmec--deftest sint32	fits)
    (mmec--deftest uint32	unsupported)
    (mmec--deftest sint64	no-fits)
    (mmec--deftest uint64	unsupported)
    (mmec--deftest float	unsupported)
    (mmec--deftest double	unsupported)
    (mmec--deftest ldouble	unsupported))

;;; --------------------------------------------------------------------
;;; mmec-sshrt

  (cl-macrolet
      ((mmec--deftest (ARGSTEM PROPERTY) `(mmec--def sshrt ,ARGSTEM ,PROPERTY)))
    (mmec--deftest char		fits)
    (mmec--deftest schar	fits)
    (mmec--deftest uchar	unsupported)
    (mmec--deftest wchar	no-fits)
    (mmec--deftest sshrt	fits)
    (mmec--deftest ushrt	unsupported)
    (mmec--deftest sint		no-fits)
    (mmec--deftest uint		unsupported)
    (mmec--deftest slong	no-fits)
    (mmec--deftest ulong	unsupported)
    (mmec--deftest sllong	no-fits)
    (mmec--deftest ullong	unsupported)
    (mmec--deftest ssize	no-fits)
    (mmec--deftest usize	unsupported)
    (mmec--deftest sintmax	no-fits)
    (mmec--deftest uintmax	unsupported)
    (mmec--deftest ptrdiff	no-fits)
    (mmec--deftest sint8	fits)
    (mmec--deftest uint8	unsupported)
    (mmec--deftest sint16	fits)
    (mmec--deftest uint16	unsupported)
    (mmec--deftest sint32	no-fits)
    (mmec--deftest uint32	unsupported)
    (mmec--deftest sint64	no-fits)
    (mmec--deftest uint64	unsupported)
    (mmec--deftest float	unsupported)
    (mmec--deftest double	unsupported)
    (mmec--deftest ldouble	unsupported))

;;; --------------------------------------------------------------------
;;; mmec-ushrt

  (cl-macrolet
      ((mmec--deftest (ARGSTEM PROPERTY) `(mmec--def ushrt ,ARGSTEM ,PROPERTY)))
    (mmec--deftest char		unsupported)
    (mmec--deftest schar	unsupported)
    (mmec--deftest uchar	fits)
    (mmec--deftest wchar	unsupported)
    (mmec--deftest sshrt	unsupported)
    (mmec--deftest ushrt	fits)
    (mmec--deftest sint		unsupported)
    (mmec--deftest uint		no-fits)
    (mmec--deftest slong	unsupported)
    (mmec--deftest ulong	no-fits)
    (mmec--deftest sllong	unsupported)
    (mmec--deftest ullong	no-fits)
    (mmec--deftest ssize	unsupported)
    (mmec--deftest usize	no-fits)
    (mmec--deftest sintmax	unsupported)
    (mmec--deftest uintmax	no-fits)
    (mmec--deftest ptrdiff	unsupported)
    (mmec--deftest sint8	unsupported)
    (mmec--deftest uint8	fits)
    (mmec--deftest sint16	unsupported)
    (mmec--deftest uint16	fits)
    (mmec--deftest sint32	unsupported)
    (mmec--deftest uint32	no-fits)
    (mmec--deftest sint64	unsupported)
    (mmec--deftest uint64	no-fits)
    (mmec--deftest float	unsupported)
    (mmec--deftest double	unsupported)
    (mmec--deftest ldouble	unsupported))

  ;; End of outer CL-MACROLET.
  )


;;;; equality tests

(defmacro mmux-core-test--equality--integers-signed (TYPE)
  (let* ((TESTNAME	(mmec-sformat "mmec-test-%s-equality" TYPE))
	 (DOCSTRING	(format "Compare objects of type `%s' for equality." TYPE))
	 (MAKER		(if (eq TYPE 'integer)
			    'identity
			  TYPE)))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should	(mmec= (,MAKER 1) (,MAKER 1)))
	 (should (not	(mmec= (,MAKER 5) (,MAKER 17))))
	 ;; Compare with a signed integer.
	 (should	(mmec= (,MAKER 1) (mmec-sint 1)))
	 (should (not	(mmec= (,MAKER 5) (mmec-sint 17))))
	 (should	(mmec= (mmec-sint 1) (,MAKER 1)))
	 (should (not	(mmec= (mmec-sint 5) (,MAKER 17))))
	 ))))

(defmacro mmux-core-test--equality--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-equality" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should	(mmec= (,TYPE 1) (,TYPE 1)))
	 (should (not	(mmec= (,TYPE 5) (,TYPE 17))))
	 ;; Compare with an unsigned integer.
	 (should	(mmec= (,TYPE 1) (mmec-uint 1)))
	 (should (not	(mmec= (,TYPE 5) (mmec-uint 17))))
	 (should	(mmec= (mmec-uint 1) (,TYPE 1)))
	 (should (not	(mmec= (mmec-uint 5) (,TYPE 17))))
	 ))))

(defmacro mmux-core-test--floating-point--equal-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-equality" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality."))a)
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should	(mmec= (,TYPE 1.0) (,TYPE 1.0)))
	 (should (not	(mmec= (,TYPE 5.0) (,TYPE 17.0))))
	 ;; Compare with a `float'.
	 (should	(mmec= (,TYPE 1.0) 1.0))
	 (should (not	(mmec= (,TYPE 5.0) 17.0)))
	 (should	(mmec= 1.0 (,TYPE 1.0)))
	 (should (not	(mmec= 5.0 (,TYPE 17.0))))
	 ;; Compare with a `mmec-float'.
	 (should	(mmec= (,TYPE 1.0) (mmec-float 1.0)))
	 (should (not	(mmec= (,TYPE 5.0) (mmec-float 17.0))))
	 (should	(mmec= (mmec-float 1.0) (,TYPE 1.0)))
	 (should (not	(mmec= (mmec-float 5.0) (,TYPE 17.0))))
	 ;; Compare with a `mmec-ldouble'.
	 (should	(mmec= (,TYPE 1.0) (mmec-ldouble 1.0)))
	 (should (not	(mmec= (,TYPE 5.0) (mmec-ldouble 17.0))))
	 (should	(mmec= (mmec-ldouble 1.0) (,TYPE 1.0)))
	 (should (not	(mmec= (mmec-ldouble 5.0) (,TYPE 17.0))))
	 ))))

(mmux-core-test--equality--integers-signed	integer)
(mmux-core-test--equality--integers-signed	mmec-char)
(mmux-core-test--equality--integers-unsigned	mmec-uchar)
(mmux-core-test--equality--integers-signed	mmec-schar)
(mmux-core-test--equality--integers-signed	mmec-wchar)
(mmux-core-test--equality--integers-signed	mmec-sshrt)
(mmux-core-test--equality--integers-unsigned	mmec-ushrt)
(mmux-core-test--equality--integers-signed	mmec-sint)
(mmux-core-test--equality--integers-unsigned	mmec-uint)
(mmux-core-test--equality--integers-signed	mmec-slong)
(mmux-core-test--equality--integers-unsigned	mmec-ulong)
(mmux-core-test--equality--integers-signed	mmec-sllong)
(mmux-core-test--equality--integers-unsigned	mmec-ullong)
(mmux-core-test--equality--integers-signed	mmec-sint8)
(mmux-core-test--equality--integers-unsigned	mmec-uint8)
(mmux-core-test--equality--integers-signed	mmec-sint16)
(mmux-core-test--equality--integers-unsigned	mmec-uint16)
(mmux-core-test--equality--integers-signed	mmec-sint32)
(mmux-core-test--equality--integers-unsigned	mmec-uint32)
(mmux-core-test--equality--integers-signed	mmec-sint64)
(mmux-core-test--equality--integers-unsigned	mmec-uint64)
(mmux-core-test--equality--integers-signed	mmec-ssize)
(mmux-core-test--equality--integers-unsigned	mmec-usize)
(mmux-core-test--equality--integers-signed	mmec-sintmax)
(mmux-core-test--equality--integers-unsigned	mmec-uintmax)
(mmux-core-test--equality--integers-signed	mmec-ptrdiff)

(mmux-core-test--floating-point--equal-tests	float)
(mmux-core-test--floating-point--equal-tests	mmec-float)
(mmux-core-test--floating-point--equal-tests	mmec-double)
(mmux-core-test--floating-point--equal-tests	mmec-ldouble)


;;;; not-equality tests

(defmacro mmux-core-test--not-equal--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-not-equality" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for not-equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not	(mmec/= (,TYPE 1) (,TYPE 1))))
	 (should 	(mmec/= (,TYPE 5) (,TYPE 17)))
	 ;; Compare with a signed integer.
	 (should (not	(mmec/= (,TYPE 1) (mmec-sint 1))))
	 (should 	(mmec/= (,TYPE 5) (mmec-sint 17)))
	 (should (not	(mmec/= (mmec-sint 1) (,TYPE 1))))
	 (should 	(mmec/= (mmec-sint 5) (,TYPE 17)))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
	 ;; (should (not	(mmec/= (,TYPE 1) (mmec-uint 1))))
	 ;; (should 	(mmec/= (,TYPE 5) (mmec-uint 17)))
	 ;; (should (not	(mmec/= (mmec-uint 1) (,TYPE 1))))
	 ;; (should 	(mmec/= (mmec-uint 5) (,TYPE 17)))
	 ))))

(defmacro mmux-core-test--not-equal--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-not-equality" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for not-equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not	(mmec/= (,TYPE 1) (,TYPE 1))))
	 (should 	(mmec/= (,TYPE 5) (,TYPE 17)))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
	 ;; (should (not	(mmec/= (,TYPE 1) (mmec-sint 1))))
	 ;; (should 	(mmec/= (,TYPE 5) (mmec-sint 17)))
	 ;; (should (not	(mmec/= (mmec-sint 1) (,TYPE 1))))
	 ;; (should 	(mmec/= (mmec-sint 5) (,TYPE 17)))
	 ;; Compare with an unsigned integer.
	 (should (not	(mmec/= (,TYPE 1) (mmec-uint 1))))
	 (should 	(mmec/= (,TYPE 5) (mmec-uint 17)))
	 (should (not	(mmec/= (mmec-uint 1) (,TYPE 1))))
	 (should 	(mmec/= (mmec-uint 5) (,TYPE 17)))
	 ))))

(defmacro mmux-core-test--floating-point--not-equal-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-not-equality" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for not-equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not	(mmec/= (,TYPE 1.0) (,TYPE 1.0))))
	 (should 	(mmec/= (,TYPE 5.0) (,TYPE 17.0)))
	 ;; Compare with a float.
	 (should (not	(mmec/= (,TYPE 1.0) 1.0)))
	 (should 	(mmec/= (,TYPE 5.0) 17.0))
	 (should (not	(mmec/= 1.0 (,TYPE 1.0))))
	 (should 	(mmec/= 5.0 (,TYPE 17.0)))
	 ;; Compare with a mmec-float.
	 (should (not	(mmec/= (,TYPE 1.0) (mmec-float 1.0))))
	 (should 	(mmec/= (,TYPE 5.0) (mmec-float 17.0)))
	 (should (not	(mmec/= (mmec-float 1.0) (,TYPE 1.0))))
	 (should 	(mmec/= (mmec-float 5.0) (,TYPE 17.0)))
	 ;; Compare with a mmec-ldouble.
	 (should (not	(mmec/= (,TYPE 1.0) (mmec-ldouble 1.0))))
	 (should 	(mmec/= (,TYPE 5.0) (mmec-ldouble 17.0)))
	 (should (not	(mmec/= (mmec-ldouble 1.0) (,TYPE 1.0))))
	 (should 	(mmec/= (mmec-ldouble 5.0) (,TYPE 17.0)))
	 ))))

(ert-deftest mmec-test-integer-not-equality ()
  "Compare objects of type `integer' for not-equality."
  (should (not	(mmec/= 1 1)))
  (should 	(mmec/= 1 2)))

(ert-deftest mmec-test-float-not-equality ()
  "Compare objects of type `float' for not-equality."
  (should (not	(mmec/= 1.0 1.0)))
  (should 	(mmec/= 1.0 2.0)))

(mmux-core-test--not-equal--integers-signed	mmec-char)
(mmux-core-test--not-equal--integers-unsigned	mmec-uchar)
(mmux-core-test--not-equal--integers-signed	mmec-schar)
(mmux-core-test--not-equal--integers-signed	mmec-wchar)
(mmux-core-test--not-equal--integers-signed	mmec-sshrt)
(mmux-core-test--not-equal--integers-unsigned	mmec-ushrt)
(mmux-core-test--not-equal--integers-signed	mmec-sint)
(mmux-core-test--not-equal--integers-unsigned	mmec-uint)
(mmux-core-test--not-equal--integers-signed	mmec-slong)
(mmux-core-test--not-equal--integers-unsigned	mmec-ulong)
(mmux-core-test--not-equal--integers-signed	mmec-sllong)
(mmux-core-test--not-equal--integers-unsigned	mmec-ullong)
(mmux-core-test--not-equal--integers-signed	mmec-sint8)
(mmux-core-test--not-equal--integers-unsigned	mmec-uint8)
(mmux-core-test--not-equal--integers-signed	mmec-sint16)
(mmux-core-test--not-equal--integers-unsigned	mmec-uint16)
(mmux-core-test--not-equal--integers-signed	mmec-sint32)
(mmux-core-test--not-equal--integers-unsigned	mmec-uint32)
(mmux-core-test--not-equal--integers-signed	mmec-sint64)
(mmux-core-test--not-equal--integers-unsigned	mmec-uint64)
(mmux-core-test--not-equal--integers-signed	mmec-ssize)
(mmux-core-test--not-equal--integers-unsigned	mmec-usize)
(mmux-core-test--not-equal--integers-signed	mmec-sintmax)
(mmux-core-test--not-equal--integers-unsigned	mmec-uintmax)
(mmux-core-test--not-equal--integers-signed	mmec-ptrdiff)

(mmux-core-test--floating-point--not-equal-tests float)
(mmux-core-test--floating-point--not-equal-tests mmec-float)
(mmux-core-test--floating-point--not-equal-tests mmec-ldouble)


;;;; less-than tests

(defmacro mmux-core-test--less-than--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-less-than" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec< (,TYPE 1) (,TYPE 2)))
	 (should (not	(mmec< (,TYPE 1) (,TYPE 1))))
	 (should (not	(mmec< (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 (should 	(mmec< (,TYPE 1) (mmec-sint 2)))
	 (should (not	(mmec< (,TYPE 1) (mmec-sint 1))))
	 (should (not	(mmec< (,TYPE 2) (mmec-sint 1))))
	 (should 	(mmec< (mmec-sint 1) (,TYPE 2)))
	 (should (not	(mmec< (mmec-sint 1) (,TYPE 1))))
	 (should (not	(mmec< (mmec-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (mmec< (,TYPE 1) (mmec-uint 2)))
         ;; (should (not   (mmec< (,TYPE 1) (mmec-uint 1))))
         ;; (should (not   (mmec< (,TYPE 2) (mmec-uint 1))))
         ;; (should        (mmec< (mmec-uint 1) (,TYPE 2)))
         ;; (should (not   (mmec< (mmec-uint 1) (,TYPE 1))))
         ;; (should (not   (mmec< (mmec-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--less-than--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-less-than" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec< (,TYPE 1) (,TYPE 2)))
	 (should (not	(mmec< (,TYPE 1) (,TYPE 1))))
	 (should (not	(mmec< (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (mmec< (,TYPE 1) (mmec-sint 2)))
         ;; (should (not   (mmec< (,TYPE 1) (mmec-sint 1))))
         ;; (should (not   (mmec< (,TYPE 2) (mmec-sint 1))))
         ;; (should        (mmec< (mmec-sint 1) (,TYPE 2)))
         ;; (should (not   (mmec< (mmec-sint 1) (,TYPE 1))))
         ;; (should (not   (mmec< (mmec-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 (should 	(mmec< (,TYPE 1) (mmec-uint 2)))
	 (should (not	(mmec< (,TYPE 1) (mmec-uint 1))))
	 (should (not	(mmec< (,TYPE 2) (mmec-uint 1))))
	 (should 	(mmec< (mmec-uint 1) (,TYPE 2)))
	 (should (not	(mmec< (mmec-uint 1) (,TYPE 1))))
	 (should (not	(mmec< (mmec-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--floating-point--less-than-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-less-than" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec< (,TYPE 1.0) (,TYPE 2.0)))
	 (should (not	(mmec< (,TYPE 1.0) (,TYPE 1.0))))
	 (should (not	(mmec< (,TYPE 2.0) (,TYPE 1.0))))
	 ;; Compare with a float.
	 (should 	(mmec< (,TYPE 1.0) 2.0))
	 (should (not	(mmec< (,TYPE 1.0) 1.0)))
	 (should (not	(mmec< (,TYPE 2.0) 1.0)))
	 (should 	(mmec< 1.0 (,TYPE 2.0)))
	 (should (not	(mmec< 1.0 (,TYPE 1.0))))
	 (should (not	(mmec< 2.0 (,TYPE 1.0))))
	 ;; Compare with a mmec-float.
	 (should 	(mmec< (,TYPE 1.0) (mmec-float 2.0)))
	 (should (not	(mmec< (,TYPE 1.0) (mmec-float 1.0))))
	 (should (not	(mmec< (,TYPE 2.0) (mmec-float 1.0))))
	 (should 	(mmec< (mmec-float 1.0) (,TYPE 2.0)))
	 (should (not	(mmec< (mmec-float 1.0) (,TYPE 1.0))))
	 (should (not	(mmec< (mmec-float 2.0) (,TYPE 1.0))))
	 ;; Compare with a mmec-ldouble.
	 (should 	(mmec< (,TYPE 1.0) (mmec-ldouble 2.0)))
	 (should (not	(mmec< (,TYPE 1.0) (mmec-ldouble 1.0))))
	 (should (not	(mmec< (,TYPE 2.0) (mmec-ldouble 1.0))))
	 (should 	(mmec< (mmec-ldouble 1.0) (,TYPE 2.0)))
	 (should (not	(mmec< (mmec-ldouble 1.0) (,TYPE 1.0))))
	 (should (not	(mmec< (mmec-ldouble 2.0) (,TYPE 1.0))))
	 ))))

(ert-deftest mmec-test-integer-less-than ()
  "Compare objects of type `integer' for equality."
  (should 	(mmec< 1 2))
  (should (not	(mmec< 1 1)))
  (should (not	(mmec< 2 1))))

(ert-deftest mmec-test-float-less-than ()
  "Compare objects of type `float' for equality."
  (should 	(mmec< 1.0 2.0))
  (should (not	(mmec< 1.0 1.0)))
  (should (not	(mmec< 2.0 1.0))))

(mmux-core-test--less-than--integers-signed	mmec-char)
(mmux-core-test--less-than--integers-unsigned	mmec-uchar)
(mmux-core-test--less-than--integers-signed	mmec-schar)
(mmux-core-test--less-than--integers-signed	mmec-wchar)
(mmux-core-test--less-than--integers-signed	mmec-sshrt)
(mmux-core-test--less-than--integers-unsigned	mmec-ushrt)
(mmux-core-test--less-than--integers-signed	mmec-sint)
(mmux-core-test--less-than--integers-unsigned	mmec-uint)
(mmux-core-test--less-than--integers-signed	mmec-slong)
(mmux-core-test--less-than--integers-unsigned	mmec-ulong)
(mmux-core-test--less-than--integers-signed	mmec-sllong)
(mmux-core-test--less-than--integers-unsigned	mmec-ullong)
(mmux-core-test--less-than--integers-signed	mmec-sint8)
(mmux-core-test--less-than--integers-unsigned	mmec-uint8)
(mmux-core-test--less-than--integers-signed	mmec-sint16)
(mmux-core-test--less-than--integers-unsigned	mmec-uint16)
(mmux-core-test--less-than--integers-signed	mmec-sint32)
(mmux-core-test--less-than--integers-unsigned	mmec-uint32)
(mmux-core-test--less-than--integers-signed	mmec-sint64)
(mmux-core-test--less-than--integers-unsigned	mmec-uint64)
(mmux-core-test--less-than--integers-signed	mmec-ssize)
(mmux-core-test--less-than--integers-unsigned	mmec-usize)
(mmux-core-test--less-than--integers-signed	mmec-sintmax)
(mmux-core-test--less-than--integers-unsigned	mmec-uintmax)
(mmux-core-test--less-than--integers-signed	mmec-ptrdiff)

(mmux-core-test--floating-point--less-than-tests float)
(mmux-core-test--floating-point--less-than-tests mmec-float)
(mmux-core-test--floating-point--less-than-tests mmec-ldouble)


;;;; greater-than tests

(defmacro mmux-core-test--greater-than--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-greater-than" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec> (,TYPE 1) (,TYPE 2))))
	 (should (not	(mmec> (,TYPE 1) (,TYPE 1))))
	 (should 	(mmec> (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
	 (should (not 	(mmec> (,TYPE 1) (mmec-sint 2))))
	 (should (not	(mmec> (,TYPE 1) (mmec-sint 1))))
	 (should 	(mmec> (,TYPE 2) (mmec-sint 1)))
	 (should (not	(mmec> (mmec-sint 1) (,TYPE 2))))
	 (should (not	(mmec> (mmec-sint 1) (,TYPE 1))))
	 (should 	(mmec> (mmec-sint 2) (,TYPE 1)))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should (not   (mmec> (,TYPE 1) (mmec-uint 2))))
         ;; (should (not   (mmec> (,TYPE 1) (mmec-uint 1))))
         ;; (should        (mmec> (,TYPE 2) (mmec-uint 1)))
         ;; (should (not   (mmec> (mmec-uint 1) (,TYPE 2))))
         ;; (should (not   (mmec> (mmec-uint 1) (,TYPE 1))))
         ;; (should        (mmec> (mmec-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--greater-than--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-greater-than" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec> (,TYPE 1) (,TYPE 2))))
	 (should (not	(mmec> (,TYPE 1) (,TYPE 1))))
	 (should 	(mmec> (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
         ;; (should (not   (mmec> (,TYPE 1) (mmec-sint 2))))
         ;; (should (not   (mmec> (,TYPE 1) (mmec-sint 1))))
         ;; (should        (mmec> (,TYPE 2) (mmec-sint 1)))
         ;; (should (not   (mmec> (mmec-sint 1) (,TYPE 2))))
         ;; (should (not   (mmec> (mmec-sint 1) (,TYPE 1))))
         ;; (should        (mmec> (mmec-sint 2) (,TYPE 1)))
	 ;;
	 ;; Compare with an unsigned integer.
	 ;;
	 (should (not	(mmec> (,TYPE 1) (mmec-uint 2))))
	 (should (not	(mmec> (,TYPE 1) (mmec-uint 1))))
	 (should	(mmec> (,TYPE 2) (mmec-uint 1)))
	 (should (not	(mmec> (mmec-uint 1) (,TYPE 2))))
	 (should (not	(mmec> (mmec-uint 1) (,TYPE 1))))
	 (should	(mmec> (mmec-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--floating-point--greater-than-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-greater-than" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec> (,TYPE 1.0) (,TYPE 2.0))))
	 (should (not	(mmec> (,TYPE 1.0) (,TYPE 1.0))))
	 (should 	(mmec> (,TYPE 2.0) (,TYPE 1.0)))
	 ;; Compare with a float.
	 (should (not 	(mmec> (,TYPE 1.0) 2.0)))
	 (should (not	(mmec> (,TYPE 1.0) 1.0)))
	 (should 	(mmec> (,TYPE 2.0) 1.0))
	 (should (not 	(mmec> 1.0 (,TYPE 2.0))))
	 (should (not	(mmec> 1.0 (,TYPE 1.0))))
	 (should 	(mmec> 2.0 (,TYPE 1.0)))
	 ;; Compare with a mmec-float.
	 (should (not 	(mmec> (,TYPE 1.0) (mmec-float 2.0))))
	 (should (not	(mmec> (,TYPE 1.0) (mmec-float 1.0))))
	 (should 	(mmec> (,TYPE 2.0) (mmec-float 1.0)))
	 (should (not 	(mmec> (mmec-float 1.0) (,TYPE 2.0))))
	 (should (not	(mmec> (mmec-float 1.0) (,TYPE 1.0))))
	 (should 	(mmec> (mmec-float 2.0) (,TYPE 1.0)))
	 ;; Compare with a mmec-ldouble.
	 (should (not 	(mmec> (,TYPE 1.0) (mmec-ldouble 2.0))))
	 (should (not	(mmec> (,TYPE 1.0) (mmec-ldouble 1.0))))
	 (should 	(mmec> (,TYPE 2.0) (mmec-ldouble 1.0)))
	 (should (not 	(mmec> (mmec-ldouble 1.0) (,TYPE 2.0))))
	 (should (not	(mmec> (mmec-ldouble 1.0) (,TYPE 1.0))))
	 (should 	(mmec> (mmec-ldouble 2.0) (,TYPE 1.0)))
	 ))))

(ert-deftest mmec-test-integer-greater-than ()
  "Compare objects of type `integer' for equality."
  (should (not 	(mmec> 1 2)))
  (should (not	(mmec> 1 1)))
  (should 	(mmec> 2 1)))

(ert-deftest mmec-test-float-greater-than ()
  "Compare objects of type `float' for equality."
  (should (not 	(mmec> 1.0 2.0)))
  (should (not	(mmec> 1.0 1.0)))
  (should 	(mmec> 2.0 1.0)))

(mmux-core-test--greater-than--integers-signed		mmec-char)
(mmux-core-test--greater-than--integers-unsigned	mmec-uchar)
(mmux-core-test--greater-than--integers-signed		mmec-schar)
(mmux-core-test--greater-than--integers-signed	mmec-wchar)
(mmux-core-test--greater-than--integers-signed		mmec-sshrt)
(mmux-core-test--greater-than--integers-unsigned	mmec-ushrt)
(mmux-core-test--greater-than--integers-signed		mmec-sint)
(mmux-core-test--greater-than--integers-unsigned	mmec-uint)
(mmux-core-test--greater-than--integers-signed		mmec-slong)
(mmux-core-test--greater-than--integers-unsigned	mmec-ulong)
(mmux-core-test--greater-than--integers-signed		mmec-sllong)
(mmux-core-test--greater-than--integers-unsigned	mmec-ullong)
(mmux-core-test--greater-than--integers-signed		mmec-sint8)
(mmux-core-test--greater-than--integers-unsigned	mmec-uint8)
(mmux-core-test--greater-than--integers-signed		mmec-sint16)
(mmux-core-test--greater-than--integers-unsigned	mmec-uint16)
(mmux-core-test--greater-than--integers-signed		mmec-sint32)
(mmux-core-test--greater-than--integers-unsigned	mmec-uint32)
(mmux-core-test--greater-than--integers-signed		mmec-sint64)
(mmux-core-test--greater-than--integers-unsigned	mmec-uint64)
(mmux-core-test--greater-than--integers-signed		mmec-ssize)
(mmux-core-test--greater-than--integers-unsigned	mmec-usize)
(mmux-core-test--greater-than--integers-signed		mmec-sintmax)
(mmux-core-test--greater-than--integers-unsigned	mmec-uintmax)
(mmux-core-test--greater-than--integers-signed		mmec-ptrdiff)

(mmux-core-test--floating-point--greater-than-tests float)
(mmux-core-test--floating-point--greater-than-tests mmec-float)
(mmux-core-test--floating-point--greater-than-tests mmec-ldouble)


;;;; less-than or equal-to tests

(defmacro mmux-core-test--less-than-or-equal-to--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-less-than-or-equal-to" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec<= (,TYPE 1) (,TYPE 2)))
	 (should 	(mmec<= (,TYPE 1) (,TYPE 1)))
	 (should (not	(mmec<= (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 (should 	(mmec<= (,TYPE 1) (mmec-sint 2)))
	 (should 	(mmec<= (,TYPE 1) (mmec-sint 1)))
	 (should (not	(mmec<= (,TYPE 2) (mmec-sint 1))))
	 (should 	(mmec<= (mmec-sint 1) (,TYPE 2)))
	 (should 	(mmec<= (mmec-sint 1) (,TYPE 1)))
	 (should (not	(mmec<= (mmec-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (mmec<= (,TYPE 1) (mmec-uint 2)))
         ;; (should        (mmec<= (,TYPE 1) (mmec-uint 1)))
         ;; (should (not   (mmec<= (,TYPE 2) (mmec-uint 1))))
         ;; (should        (mmec<= (mmec-uint 1) (,TYPE 2)))
         ;; (should        (mmec<= (mmec-uint 1) (,TYPE 1)))
         ;; (should (not   (mmec<= (mmec-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--less-than-or-equal-to--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-less-than-or-equal-to" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec<= (,TYPE 1) (,TYPE 2)))
	 (should 	(mmec<= (,TYPE 1) (,TYPE 1)))
	 (should (not	(mmec<= (,TYPE 2) (,TYPE 1))))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should        (mmec<= (,TYPE 1) (mmec-sint 2)))
         ;; (should        (mmec<= (,TYPE 1) (mmec-sint 1)))
         ;; (should (not   (mmec<= (,TYPE 2) (mmec-sint 1))))
         ;; (should        (mmec<= (mmec-sint 1) (,TYPE 2)))
         ;; (should        (mmec<= (mmec-sint 1) (,TYPE 1)))
         ;; (should (not   (mmec<= (mmec-sint 2) (,TYPE 1))))
	 ;; Compare with an unsigned integer.
	 (should 	(mmec<= (,TYPE 1) (mmec-uint 2)))
	 (should 	(mmec<= (,TYPE 1) (mmec-uint 1)))
	 (should (not	(mmec<= (,TYPE 2) (mmec-uint 1))))
	 (should 	(mmec<= (mmec-uint 1) (,TYPE 2)))
	 (should 	(mmec<= (mmec-uint 1) (,TYPE 1)))
	 (should (not	(mmec<= (mmec-uint 2) (,TYPE 1))))
	 ))))

(defmacro mmux-core-test--floating-point--less-than-or-equal-to-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-less-than-or-equal-to" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should 	(mmec<= (,TYPE 1.0) (,TYPE 2.0)))
	 (should 	(mmec<= (,TYPE 1.0) (,TYPE 1.0)))
	 (should (not	(mmec<= (,TYPE 2.0) (,TYPE 1.0))))
	 ;; Compare with a float.
	 (should 	(mmec<= (,TYPE 1.0) 2.0))
	 (should 	(mmec<= (,TYPE 1.0) 1.0))
	 (should (not	(mmec<= (,TYPE 2.0) 1.0)))
	 (should 	(mmec<= 1.0 (,TYPE 2.0)))
	 (should 	(mmec<= 1.0 (,TYPE 1.0)))
	 (should (not	(mmec<= 2.0 (,TYPE 1.0))))
	 ;; Compare with a mmec-float.
	 (should 	(mmec<= (,TYPE 1.0) (mmec-float 2.0)))
	 (should 	(mmec<= (,TYPE 1.0) (mmec-float 1.0)))
	 (should (not	(mmec<= (,TYPE 2.0) (mmec-float 1.0))))
	 (should 	(mmec<= (mmec-float 1.0) (,TYPE 2.0)))
	 (should 	(mmec<= (mmec-float 1.0) (,TYPE 1.0)))
	 (should (not	(mmec<= (mmec-float 2.0) (,TYPE 1.0))))
	 ;; Compare with a mmec-ldouble.
	 (should 	(mmec<= (,TYPE 1.0) (mmec-ldouble 2.0)))
	 (should 	(mmec<= (,TYPE 1.0) (mmec-ldouble 1.0)))
	 (should (not	(mmec<= (,TYPE 2.0) (mmec-ldouble 1.0))))
	 (should 	(mmec<= (mmec-ldouble 1.0) (,TYPE 2.0)))
	 (should 	(mmec<= (mmec-ldouble 1.0) (,TYPE 1.0)))
	 (should (not	(mmec<= (mmec-ldouble 2.0) (,TYPE 1.0))))
	 ))))

(ert-deftest mmec-test-integer-less-than-or-equal-to ()
  "Compare objects of type `integer' for equality."
  (should 	(mmec<= 1 2))
  (should 	(mmec<= 1 1))
  (should (not	(mmec<= 2 1))))

(ert-deftest mmec-test-float-less-than-or-equal-to ()
  "Compare objects of type `float' for equality."
  (should 	(mmec<= 1.0 2.0))
  (should 	(mmec<= 1.0 1.0))
  (should (not	(mmec<= 2.0 1.0))))

(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-char)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uchar)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-schar)
(mmux-core-test--less-than-or-equal-to--integers-signed	mmec-wchar)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sshrt)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-ushrt)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sint)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uint)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-slong)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-ulong)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sllong)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-ullong)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sint8)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uint8)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sint16)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uint16)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sint32)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uint32)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sint64)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uint64)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-ssize)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-usize)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-sintmax)
(mmux-core-test--less-than-or-equal-to--integers-unsigned	mmec-uintmax)
(mmux-core-test--less-than-or-equal-to--integers-signed		mmec-ptrdiff)

(mmux-core-test--floating-point--less-than-or-equal-to-tests float)
(mmux-core-test--floating-point--less-than-or-equal-to-tests mmec-float)
(mmux-core-test--floating-point--less-than-or-equal-to-tests mmec-ldouble)


;;;; greater-than or equal-to tests

(defmacro mmux-core-test--greater-than-or-equal-to--integers-signed (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-greater-than-or-equal-to" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec>= (,TYPE 1) (,TYPE 2))))
	 (should 	(mmec>= (,TYPE 1) (,TYPE 1)))
	 (should 	(mmec>= (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
	 (should (not 	(mmec>= (,TYPE 1) (mmec-sint 2))))
	 (should 	(mmec>= (,TYPE 1) (mmec-sint 1)))
	 (should 	(mmec>= (,TYPE 2) (mmec-sint 1)))
	 (should (not	(mmec>= (mmec-sint 1) (,TYPE 2))))
	 (should 	(mmec>= (mmec-sint 1) (,TYPE 1)))
	 (should 	(mmec>= (mmec-sint 2) (,TYPE 1)))
	 ;; Compare with an unsigned integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should (not   (mmec>= (,TYPE 1) (mmec-uint 2))))
         ;; (should        (mmec>= (,TYPE 1) (mmec-uint 1)))
         ;; (should        (mmec>= (,TYPE 2) (mmec-uint 1)))
         ;; (should (not   (mmec>= (mmec-uint 1) (,TYPE 2))))
         ;; (should        (mmec>= (mmec-uint 1) (,TYPE 1)))
         ;; (should        (mmec>= (mmec-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--greater-than-or-equal-to--integers-unsigned (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-greater-than-or-equal-to" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec>= (,TYPE 1) (,TYPE 2))))
	 (should 	(mmec>= (,TYPE 1) (,TYPE 1)))
	 (should 	(mmec>= (,TYPE 2) (,TYPE 1)))
	 ;; Compare with a signed integer.
	 ;;
	 ;; FIXME Commented out  because, at present, the package does  not support comparing signed
	 ;; with unsigned integers.  But in future things may change.  (Marco Maggi; Feb 8, 2020)
	 ;;
         ;; (should (not   (mmec>= (,TYPE 1) (mmec-sint 2))))
         ;; (should        (mmec>= (,TYPE 1) (mmec-sint 1)))
         ;; (should        (mmec>= (,TYPE 2) (mmec-sint 1)))
         ;; (should (not   (mmec>= (mmec-sint 1) (,TYPE 2))))
         ;; (should        (mmec>= (mmec-sint 1) (,TYPE 1)))
         ;; (should        (mmec>= (mmec-sint 2) (,TYPE 1)))
	 ;;
	 ;; Compare with an unsigned integer.
	 (should (not	(mmec>= (,TYPE 1) (mmec-uint 2))))
	 (should 	(mmec>= (,TYPE 1) (mmec-uint 1)))
	 (should 	(mmec>= (,TYPE 2) (mmec-uint 1)))
	 (should (not 	(mmec>= (mmec-uint 1) (,TYPE 2))))
	 (should 	(mmec>= (mmec-uint 1) (,TYPE 1)))
	 (should 	(mmec>= (mmec-uint 2) (,TYPE 1)))
	 ))))

(defmacro mmux-core-test--floating-point--greater-than-or-equal-to-tests (TYPE)
  (let* ((TYPE.str	(symbol-name TYPE))
	 (TESTNAME	(mmec-sformat "mmec-test-%s-greater-than-or-equal-to" TYPE))
	 (DOCSTRING	(concat "Compare objects of type `" TYPE.str "' for equality.")))
    `(progn
       (ert-deftest ,TESTNAME ()
	 ,DOCSTRING
	 ;; Compare integers of same type.
	 (should (not 	(mmec>= (,TYPE 1.0) (,TYPE 2.0))))
	 (should 	(mmec>= (,TYPE 1.0) (,TYPE 1.0)))
	 (should 	(mmec>= (,TYPE 2.0) (,TYPE 1.0)))
	 ;; Compare with a float.
	 (should (not 	(mmec>= (,TYPE 1.0) 2.0)))
	 (should 	(mmec>= (,TYPE 1.0) 1.0))
	 (should 	(mmec>= (,TYPE 2.0) 1.0))
	 (should (not 	(mmec>= 1.0 (,TYPE 2.0))))
	 (should 	(mmec>= 1.0 (,TYPE 1.0)))
	 (should 	(mmec>= 2.0 (,TYPE 1.0)))
	 ;; Compare with a mmec-float.
	 (should (not 	(mmec>= (,TYPE 1.0) (mmec-float 2.0))))
	 (should 	(mmec>= (,TYPE 1.0) (mmec-float 1.0)))
	 (should 	(mmec>= (,TYPE 2.0) (mmec-float 1.0)))
	 (should (not 	(mmec>= (mmec-float 1.0) (,TYPE 2.0))))
	 (should 	(mmec>= (mmec-float 1.0) (,TYPE 1.0)))
	 (should 	(mmec>= (mmec-float 2.0) (,TYPE 1.0)))
	 ;; Compare with a mmec-ldouble.
	 (should (not 	(mmec>= (,TYPE 1.0) (mmec-ldouble 2.0))))
	 (should 	(mmec>= (,TYPE 1.0) (mmec-ldouble 1.0)))
	 (should 	(mmec>= (,TYPE 2.0) (mmec-ldouble 1.0)))
	 (should (not 	(mmec>= (mmec-ldouble 1.0) (,TYPE 2.0))))
	 (should 	(mmec>= (mmec-ldouble 1.0) (,TYPE 1.0)))
	 (should 	(mmec>= (mmec-ldouble 2.0) (,TYPE 1.0)))
	 ))))

(ert-deftest mmec-test-integer-greater-than-or-equal-to ()
  "Compare objects of type `integer' for equality."
  (should (not 	(mmec>= 1 2)))
  (should 	(mmec>= 1 1))
  (should 	(mmec>= 2 1)))

(ert-deftest mmec-test-float-greater-than-or-equal-to ()
  "Compare objects of type `float' for equality."
  (should (not 	(mmec>= 1.0 2.0)))
  (should 	(mmec>= 1.0 1.0))
  (should 	(mmec>= 2.0 1.0)))

(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-char)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uchar)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-schar)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-wchar)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sshrt)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-ushrt)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sint)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uint)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-slong)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-ulong)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sllong)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-ullong)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sint8)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uint8)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sint16)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uint16)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sint32)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uint32)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sint64)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uint64)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-ssize)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-usize)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-sintmax)
(mmux-core-test--greater-than-or-equal-to--integers-unsigned	mmec-uintmax)
(mmux-core-test--greater-than-or-equal-to--integers-signed	mmec-ptrdiff)

(mmux-core-test--floating-point--greater-than-or-equal-to-tests float)
(mmux-core-test--floating-point--greater-than-or-equal-to-tests mmec-float)
(mmux-core-test--floating-point--greater-than-or-equal-to-tests mmec-ldouble)


;;;; mixed integer floating-point equality

(cl-macrolet
    ((mmec--def (FTYPE)
		(let* ((FTYPE.str	(symbol-name FTYPE))
		       (TESTNAME	(mmec-sformat "mmec-test-%s-mixed-equality-integer" FTYPE)))
		  `(progn
		     (ert-deftest ,TESTNAME ()
		       (should	(mmec= 1   (,FTYPE 1.0)))
		       (should	(mmec= (,FTYPE 1.0) 1))
		       (should (not	(mmec= 5   (,FTYPE 17.0))))
		       (should (not	(mmec= (,FTYPE 5.0) 17)))
		       )))))
  (mmec--def		mmec-float)
  (mmec--def		float)
  (mmec--def		mmec-ldouble))

;;; --------------------------------------------------------------------

(cl-macrolet
    ((mmec--def (ITYPE FTYPE)
		(let* ((ITYPE.str	(symbol-name ITYPE))
		       (FTYPE.str	(symbol-name FTYPE))
		       (TESTNAME	(mmec-sformat "mmec-test-%s-mixed-equality-integer-%s" FTYPE ITYPE)))
		  `(progn
		     (ert-deftest ,TESTNAME ()
		       (should	(mmec= (,ITYPE 1)   (,FTYPE 1.0)))
		       (should	(mmec= (,FTYPE 1.0) (,ITYPE 1)))
		       (should (not	(mmec= (,ITYPE 5)   (,FTYPE 17.0))))
		       (should (not	(mmec= (,FTYPE 5.0) (,ITYPE 17))))
		       )))))

  (mmec--def mmec-sint	mmec-float)
  (mmec--def mmec-uint	mmec-float)

  (mmec--def mmec-sint	float)
  (mmec--def mmec-uint	float)

  (mmec--def mmec-sint	mmec-ldouble)
  (mmec--def mmec-uint	mmec-ldouble))


;;;; number objects: printing

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME	(mmec-sformat "mmec-test-%s-print"	TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s"			TYPESTEM))
		       (DOCSTRING	(format "Test printing numbers of type `%s'." NUMTYPE))
		       (RESULT		(format "#s(%s 123)" NUMTYPE)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     (should (equal ,RESULT (cl-prin1-to-string (,NUMTYPE 123))))))))
  (mmec--def char)
  (mmec--def schar)
  (mmec--def uchar)
  (mmec--def wchar)
  (mmec--def sshrt)
  (mmec--def ushrt)
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
  (mmec--def sint8)
  (mmec--def uint8)
  (mmec--def sint16)
  (mmec--def uint16)
  (mmec--def sint32)
  (mmec--def uint32)
  (mmec--def sint64)
  (mmec--def uint64))

(cl-macrolet
    ((mmec--def (TYPESTEM)
		(let* ((TESTNAME	(mmec-sformat "mmec-test-%s-print"	TYPESTEM))
		       (NUMTYPE		(mmec-sformat "mmec-%s"			TYPESTEM))
		       (DOCSTRING	(format "Test printing numbers of type `%s'." NUMTYPE))
		       (RESULT		(format "#s(%s 123)" NUMTYPE)))
		  `(ert-deftest ,TESTNAME ()
		     ,DOCSTRING
		     (should (equal ,RESULT (cl-prin1-to-string (,NUMTYPE 123.0))))))))
  (mmec--def float)
  (mmec--def double)
  (mmec--def ldouble))


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; number-objects.el ends here
