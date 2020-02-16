;;; user-pointer-objects.el --- dynamic module test

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


;;;; number objects makers: mmec-char

(ert-deftest mmec-number-char ()
  "Build a `mmec-char' object."
  (should	(mmec-fits-char-p	123))
  (should (not	(mmec-fits-char-p	123000)))
  ;;
  (should	(mmec-char-p	(mmec-char 123)))
  (should	(mmec-char-p	(mmec-char 123.0)))
  (should	(mmec-char-p	(mmec-char (mmec-char	123))))
  (should	(mmec-char-p	(mmec-char (mmec-schar	123))))
  (should	(mmec-char-p	(mmec-char (mmec-sshrt	123))))
  (should	(mmec-char-p	(mmec-char (mmec-sint	123))))
  (should	(mmec-char-p	(mmec-char (mmec-slong	123))))
  (should	(mmec-char-p	(mmec-char (mmec-sllong	123))))
  (should	(mmec-char-p	(mmec-char (mmec-sintmax	123))))
  (should	(mmec-char-p	(mmec-char (mmec-ssize	123))))
  (should	(mmec-char-p	(mmec-char (mmec-ptrdiff	123))))
  ;;
  (my--unsupported-type-error mmec-char mmec-uchar		123)
  (my--unsupported-type-error mmec-char mmec-wchar		123)
  (my--unsupported-type-error mmec-char mmec-ushrt		123)
  (my--unsupported-type-error mmec-char mmec-uint		123)
  (my--unsupported-type-error mmec-char mmec-ulong		123)
  (my--unsupported-type-error mmec-char mmec-ullong		123)
  (my--unsupported-type-error mmec-char mmec-uintmax	123)
  (my--unsupported-type-error mmec-char mmec-usize		123)
  ;;
  (my--init-argument-does-not-fit mmec-char 1230)
  nil)

;;; --------------------------------------------------------------------

(ert-deftest mmec-number-float ()
  "Build a `mmec-float' object."
  (should	(mmec-fits-float-p	123))
  (should	(mmec-fits-float-p	12.3))
  ;;
  (should	(mmec-float-p	(mmec-float 123)))
  (should	(mmec-float-p	(mmec-float 12.3)))
  ;;
  (should	(mmec-float-p	(mmec-float (mmec-char	123))))
  (should	(mmec-float-p	(mmec-float (mmec-schar	123))))
  (should	(mmec-float-p	(mmec-float (mmec-uchar	123))))
  (should	(mmec-float-p	(mmec-float (mmec-sshrt	123))))
  (should	(mmec-float-p	(mmec-float (mmec-ushrt	123))))
  (should	(mmec-float-p	(mmec-float (mmec-sint	123))))
  (should	(mmec-float-p	(mmec-float (mmec-uint	123))))
  (should	(mmec-float-p	(mmec-float (mmec-slong	123))))
  (should	(mmec-float-p	(mmec-float (mmec-ulong	123))))
  (should	(mmec-float-p	(mmec-float (mmec-sllong	123))))
  (should	(mmec-float-p	(mmec-float (mmec-ullong	123))))
  (should	(mmec-float-p	(mmec-float (mmec-sintmax	123))))
  (should	(mmec-float-p	(mmec-float (mmec-uintmax	123))))
  (should	(mmec-float-p	(mmec-float (mmec-ssize	123))))
  (should	(mmec-float-p	(mmec-float (mmec-usize	123))))
  (should	(mmec-float-p	(mmec-float (mmec-ptrdiff	123))))
  ;;
  (my--init-argument-does-not-fit mmec-float 12.30)
  t)


;;;; bytevector makers

(ert-deftest mmec-bytevector-u8 ()
  "Build a `mmec-bytevector-u8' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-u8 123)))
  (should	(mmec-integer-bytevector-p	(mmec-bytevector-u8 123)))
  (should (not	(mmec-float-bytevector-p		(mmec-bytevector-u8 123))))
  (should	(mmec-bytevector-u8-p		(mmec-bytevector-u8 123)))
  (should (not	(mmec-bytevector-u16-p		(mmec-bytevector-u8 123))))
  (should (not	(mmec-bytevector-u32-p		(mmec-bytevector-u8 123))))
  (should (not	(mmec-bytevector-u64-p		(mmec-bytevector-u8 123))))
  (should (not	(mmec-bytevector-s8-p		(mmec-bytevector-u8 123))))
  (should (not	(mmec-bytevector-s16-p		(mmec-bytevector-u8 123))))
  (should (not	(mmec-bytevector-s32-p		(mmec-bytevector-u8 123))))
  (should (not	(mmec-bytevector-s64-p		(mmec-bytevector-u8 123))))
  (should (not	(mmec-bytevector-float-p		(mmec-bytevector-u8 123))))
  (should (not	(mmec-bytevector-double-p		(mmec-bytevector-u8 123))))
  (should (not	(mmec-bytevector-ldouble-p	(mmec-bytevector-u8 123)))))

(ert-deftest mmec-bytevector-s8 ()
  "Build a `mmec-bytevector-s8' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-s8 123)))
  (should	(mmec-integer-bytevector-p	(mmec-bytevector-s8 123)))
  (should (not	(mmec-float-bytevector-p		(mmec-bytevector-s8 123))))
  (should (not	(mmec-bytevector-u8-p		(mmec-bytevector-s8 123))))
  (should (not	(mmec-bytevector-u16-p		(mmec-bytevector-s8 123))))
  (should (not	(mmec-bytevector-u32-p		(mmec-bytevector-s8 123))))
  (should (not	(mmec-bytevector-u64-p		(mmec-bytevector-s8 123))))
  (should 	(mmec-bytevector-s8-p		(mmec-bytevector-s8 123)))
  (should (not	(mmec-bytevector-s16-p		(mmec-bytevector-s8 123))))
  (should (not	(mmec-bytevector-s32-p		(mmec-bytevector-s8 123))))
  (should (not	(mmec-bytevector-s64-p		(mmec-bytevector-s8 123))))
  (should (not	(mmec-bytevector-float-p		(mmec-bytevector-s8 123))))
  (should (not	(mmec-bytevector-double-p		(mmec-bytevector-s8 123))))
  (should (not	(mmec-bytevector-ldouble-p	(mmec-bytevector-s8 123)))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-bytevector-u16 ()
  "Build a `mmec-bytevector-u16' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-u16 123)))
  (should	(mmec-integer-bytevector-p	(mmec-bytevector-u16 123)))
  (should (not	(mmec-float-bytevector-p		(mmec-bytevector-u16 123))))
  (should (not	(mmec-bytevector-u8-p		(mmec-bytevector-u16 123))))
  (should 	(mmec-bytevector-u16-p		(mmec-bytevector-u16 123)))
  (should (not	(mmec-bytevector-u32-p		(mmec-bytevector-u16 123))))
  (should (not	(mmec-bytevector-u64-p		(mmec-bytevector-u16 123))))
  (should (not	(mmec-bytevector-s8-p		(mmec-bytevector-u16 123))))
  (should (not	(mmec-bytevector-s16-p		(mmec-bytevector-u16 123))))
  (should (not	(mmec-bytevector-s32-p		(mmec-bytevector-u16 123))))
  (should (not	(mmec-bytevector-s64-p		(mmec-bytevector-u16 123))))
  (should (not	(mmec-bytevector-float-p		(mmec-bytevector-u16 123))))
  (should (not	(mmec-bytevector-double-p		(mmec-bytevector-u16 123))))
  (should (not	(mmec-bytevector-ldouble-p	(mmec-bytevector-u16 123)))))

(ert-deftest mmec-bytevector-s16 ()
  "Build a `mmec-bytevector-s16' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-s16 123)))
  (should	(mmec-integer-bytevector-p	(mmec-bytevector-s16 123)))
  (should (not	(mmec-float-bytevector-p		(mmec-bytevector-s16 123))))
  (should (not	(mmec-bytevector-u8-p		(mmec-bytevector-s16 123))))
  (should (not	(mmec-bytevector-u16-p		(mmec-bytevector-s16 123))))
  (should (not	(mmec-bytevector-u32-p		(mmec-bytevector-s16 123))))
  (should (not	(mmec-bytevector-u64-p		(mmec-bytevector-s16 123))))
  (should (not	(mmec-bytevector-s8-p		(mmec-bytevector-s16 123))))
  (should 	(mmec-bytevector-s16-p		(mmec-bytevector-s16 123)))
  (should (not	(mmec-bytevector-s32-p		(mmec-bytevector-s16 123))))
  (should (not	(mmec-bytevector-s64-p		(mmec-bytevector-s16 123))))
  (should (not	(mmec-bytevector-float-p		(mmec-bytevector-s16 123))))
  (should (not	(mmec-bytevector-double-p		(mmec-bytevector-s16 123))))
  (should (not	(mmec-bytevector-ldouble-p	(mmec-bytevector-s16 123)))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-bytevector-u32 ()
  "Build a `mmec-bytevector-u32' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-u32 123)))
  (should	(mmec-integer-bytevector-p	(mmec-bytevector-u32 123)))
  (should (not	(mmec-float-bytevector-p		(mmec-bytevector-u32 123))))
  (should (not	(mmec-bytevector-u8-p		(mmec-bytevector-u32 123))))
  (should (not	(mmec-bytevector-u16-p		(mmec-bytevector-u32 123))))
  (should 	(mmec-bytevector-u32-p		(mmec-bytevector-u32 123)))
  (should (not	(mmec-bytevector-u64-p		(mmec-bytevector-u32 123))))
  (should (not	(mmec-bytevector-s8-p		(mmec-bytevector-u32 123))))
  (should (not	(mmec-bytevector-s16-p		(mmec-bytevector-u32 123))))
  (should (not	(mmec-bytevector-s32-p		(mmec-bytevector-u32 123))))
  (should (not	(mmec-bytevector-s64-p		(mmec-bytevector-u32 123))))
  (should (not	(mmec-bytevector-float-p		(mmec-bytevector-u32 123))))
  (should (not	(mmec-bytevector-double-p		(mmec-bytevector-u32 123))))
  (should (not	(mmec-bytevector-ldouble-p	(mmec-bytevector-u32 123)))))

(ert-deftest mmec-bytevector-s32 ()
  "Build a `mmec-bytevector-s32' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-s32 123)))
  (should	(mmec-integer-bytevector-p	(mmec-bytevector-s32 123)))
  (should (not	(mmec-float-bytevector-p		(mmec-bytevector-s32 123))))
  (should (not	(mmec-bytevector-u8-p		(mmec-bytevector-s32 123))))
  (should (not	(mmec-bytevector-u16-p		(mmec-bytevector-s32 123))))
  (should (not	(mmec-bytevector-u32-p		(mmec-bytevector-s32 123))))
  (should (not	(mmec-bytevector-u64-p		(mmec-bytevector-s32 123))))
  (should (not 	(mmec-bytevector-s8-p		(mmec-bytevector-s32 123))))
  (should (not	(mmec-bytevector-s16-p		(mmec-bytevector-s32 123))))
  (should 	(mmec-bytevector-s32-p		(mmec-bytevector-s32 123)))
  (should (not	(mmec-bytevector-s64-p		(mmec-bytevector-s32 123))))
  (should (not	(mmec-bytevector-float-p		(mmec-bytevector-s32 123))))
  (should (not	(mmec-bytevector-double-p		(mmec-bytevector-s32 123))))
  (should (not	(mmec-bytevector-ldouble-p	(mmec-bytevector-s32 123)))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-bytevector-u64 ()
  "Build a `mmec-bytevector-u64' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-u64 123)))
  (should	(mmec-integer-bytevector-p	(mmec-bytevector-u64 123)))
  (should (not	(mmec-float-bytevector-p		(mmec-bytevector-u64 123))))
  (should (not	(mmec-bytevector-u8-p		(mmec-bytevector-u64 123))))
  (should (not	(mmec-bytevector-u16-p		(mmec-bytevector-u64 123))))
  (should (not 	(mmec-bytevector-u32-p		(mmec-bytevector-u64 123))))
  (should 	(mmec-bytevector-u64-p		(mmec-bytevector-u64 123)))
  (should (not	(mmec-bytevector-s8-p		(mmec-bytevector-u64 123))))
  (should (not	(mmec-bytevector-s16-p		(mmec-bytevector-u64 123))))
  (should (not	(mmec-bytevector-s32-p		(mmec-bytevector-u64 123))))
  (should (not	(mmec-bytevector-s64-p		(mmec-bytevector-u64 123))))
  (should (not	(mmec-bytevector-float-p		(mmec-bytevector-u64 123))))
  (should (not	(mmec-bytevector-double-p		(mmec-bytevector-u64 123))))
  (should (not	(mmec-bytevector-ldouble-p	(mmec-bytevector-u64 123)))))

(ert-deftest mmec-bytevector-s64 ()
  "Build a `mmec-bytevector-642' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-s64 123)))
  (should	(mmec-integer-bytevector-p	(mmec-bytevector-s64 123)))
  (should (not	(mmec-float-bytevector-p		(mmec-bytevector-s64 123))))
  (should (not	(mmec-bytevector-u8-p		(mmec-bytevector-s64 123))))
  (should (not	(mmec-bytevector-u16-p		(mmec-bytevector-s64 123))))
  (should (not	(mmec-bytevector-u32-p		(mmec-bytevector-s64 123))))
  (should (not	(mmec-bytevector-u64-p		(mmec-bytevector-s64 123))))
  (should (not 	(mmec-bytevector-s8-p		(mmec-bytevector-s64 123))))
  (should (not	(mmec-bytevector-s16-p		(mmec-bytevector-s64 123))))
  (should (not 	(mmec-bytevector-s32-p		(mmec-bytevector-s64 123))))
  (should 	(mmec-bytevector-s64-p		(mmec-bytevector-s64 123))))
  (should (not	(mmec-bytevector-float-p		(mmec-bytevector-s64 123))))
  (should (not	(mmec-bytevector-double-p		(mmec-bytevector-s64 123))))
  (should (not	(mmec-bytevector-ldouble-p	(mmec-bytevector-s64 123))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-bytevector-float ()
  "Build a `mmec-bytevector-float' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-float 123)))
  (should (not	(mmec-integer-bytevector-p	(mmec-bytevector-float 123))))
  (should 	(mmec-float-bytevector-p		(mmec-bytevector-float 123)))
  (should (not	(mmec-bytevector-u8-p		(mmec-bytevector-float 123))))
  (should (not	(mmec-bytevector-u16-p		(mmec-bytevector-float 123))))
  (should (not	(mmec-bytevector-u32-p		(mmec-bytevector-float 123))))
  (should (not	(mmec-bytevector-u64-p		(mmec-bytevector-float 123))))
  (should (not	(mmec-bytevector-s8-p		(mmec-bytevector-float 123))))
  (should (not	(mmec-bytevector-s16-p		(mmec-bytevector-float 123))))
  (should (not	(mmec-bytevector-s32-p		(mmec-bytevector-float 123))))
  (should (not	(mmec-bytevector-s64-p		(mmec-bytevector-float 123))))
  (should 	(mmec-bytevector-float-p		(mmec-bytevector-float 123)))
  (should (not	(mmec-bytevector-double-p		(mmec-bytevector-float 123))))
  (should (not	(mmec-bytevector-ldouble-p	(mmec-bytevector-float 123)))))

(ert-deftest mmec-bytevector-double ()
  "Build a `mmec-bytevector-double' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-double 123)))
  (should (not	(mmec-integer-bytevector-p	(mmec-bytevector-double 123))))
  (should 	(mmec-float-bytevector-p		(mmec-bytevector-double 123)))
  (should (not	(mmec-bytevector-u8-p		(mmec-bytevector-double 123))))
  (should (not	(mmec-bytevector-u16-p		(mmec-bytevector-double 123))))
  (should (not	(mmec-bytevector-u32-p		(mmec-bytevector-double 123))))
  (should (not	(mmec-bytevector-u64-p		(mmec-bytevector-double 123))))
  (should (not	(mmec-bytevector-s8-p		(mmec-bytevector-double 123))))
  (should (not	(mmec-bytevector-s16-p		(mmec-bytevector-double 123))))
  (should (not	(mmec-bytevector-s32-p		(mmec-bytevector-double 123))))
  (should (not	(mmec-bytevector-s64-p		(mmec-bytevector-double 123))))
  (should (not	(mmec-bytevector-float-p		(mmec-bytevector-double 123))))
  (should 	(mmec-bytevector-double-p		(mmec-bytevector-double 123)))
  (should (not	(mmec-bytevector-ldouble-p	(mmec-bytevector-double 123)))))

(ert-deftest mmec-bytevector-ldouble ()
  "Build a `mmec-bytevector-ldouble' object."
  (should	(mmec-bytevector-p		(mmec-bytevector-ldouble 123)))
  (should (not	(mmec-integer-bytevector-p	(mmec-bytevector-ldouble 123))))
  (should 	(mmec-float-bytevector-p		(mmec-bytevector-ldouble 123)))
  (should (not	(mmec-bytevector-u8-p		(mmec-bytevector-ldouble 123))))
  (should (not	(mmec-bytevector-u16-p		(mmec-bytevector-ldouble 123))))
  (should (not	(mmec-bytevector-u32-p		(mmec-bytevector-ldouble 123))))
  (should (not	(mmec-bytevector-u64-p		(mmec-bytevector-ldouble 123))))
  (should (not	(mmec-bytevector-s8-p		(mmec-bytevector-ldouble 123))))
  (should (not	(mmec-bytevector-s16-p		(mmec-bytevector-ldouble 123))))
  (should (not	(mmec-bytevector-s32-p		(mmec-bytevector-ldouble 123))))
  (should (not	(mmec-bytevector-s64-p		(mmec-bytevector-ldouble 123))))
  (should (not	(mmec-bytevector-float-p		(mmec-bytevector-ldouble 123))))
  (should (not	(mmec-bytevector-double-p		(mmec-bytevector-ldouble 123))))
  (should 	(mmec-bytevector-ldouble-p	(mmec-bytevector-ldouble 123))))


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; test.el ends here
