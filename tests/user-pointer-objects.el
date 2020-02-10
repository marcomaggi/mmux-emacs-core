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
(require 'cc-core)


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


;;;; number objects makers

(ert-deftest cc-number-char ()
  "Build a `cc-char' object."
  (should	(cc-fits-char-p	123))
  (should (not	(cc-fits-char-p	123000)))
  ;;
  (should	(cc-char-p	(cc-char 123)))
  (should	(cc-char-p	(cc-char 123.0)))
  (should	(cc-char-p	(cc-char (cc-char	123))))
  (should	(cc-char-p	(cc-char (cc-schar	123))))
  (should	(cc-char-p	(cc-char (cc-sshrt	123))))
  (should	(cc-char-p	(cc-char (cc-sint	123))))
  (should	(cc-char-p	(cc-char (cc-slong	123))))
  (should	(cc-char-p	(cc-char (cc-sllong	123))))
  (should	(cc-char-p	(cc-char (cc-sintmax	123))))
  (should	(cc-char-p	(cc-char (cc-ssize	123))))
  (should	(cc-char-p	(cc-char (cc-ptrdiff	123))))
  ;;
  (my--unsupported-type-error cc-char cc-uchar		123)
  (my--unsupported-type-error cc-char cc-wchar		123)
  (my--unsupported-type-error cc-char cc-ushrt		123)
  (my--unsupported-type-error cc-char cc-uint		123)
  (my--unsupported-type-error cc-char cc-ulong		123)
  (my--unsupported-type-error cc-char cc-ullong		123)
  (my--unsupported-type-error cc-char cc-uintmax	123)
  (my--unsupported-type-error cc-char cc-usize		123)
  ;;
  (my--init-argument-does-not-fit cc-char 1230)
  )


;;;; bytevector makers

(ert-deftest cc-bytevector-u8 ()
  "Build a `cc-bytevector-u8' object."
  (should	(cc-bytevector-p		(cc-bytevector-u8 123)))
  (should	(cc-integer-bytevector-p	(cc-bytevector-u8 123)))
  (should (not	(cc-float-bytevector-p		(cc-bytevector-u8 123))))
  (should	(cc-bytevector-u8-p		(cc-bytevector-u8 123)))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-u8 123))))
  (should (not	(cc-bytevector-u32-p		(cc-bytevector-u8 123))))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-u8 123))))
  (should (not	(cc-bytevector-s8-p		(cc-bytevector-u8 123))))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-u8 123))))
  (should (not	(cc-bytevector-s32-p		(cc-bytevector-u8 123))))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-u8 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-u8 123))))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-u8 123))))
  (should (not	(cc-bytevector-ldouble-p	(cc-bytevector-u8 123)))))

(ert-deftest cc-bytevector-s8 ()
  "Build a `cc-bytevector-s8' object."
  (should	(cc-bytevector-p		(cc-bytevector-s8 123)))
  (should	(cc-integer-bytevector-p	(cc-bytevector-s8 123)))
  (should (not	(cc-float-bytevector-p		(cc-bytevector-s8 123))))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-s8 123))))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-s8 123))))
  (should (not	(cc-bytevector-u32-p		(cc-bytevector-s8 123))))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-s8 123))))
  (should 	(cc-bytevector-s8-p		(cc-bytevector-s8 123)))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-s8 123))))
  (should (not	(cc-bytevector-s32-p		(cc-bytevector-s8 123))))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-s8 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-s8 123))))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-s8 123))))
  (should (not	(cc-bytevector-ldouble-p	(cc-bytevector-s8 123)))))

;;; --------------------------------------------------------------------

(ert-deftest cc-bytevector-u16 ()
  "Build a `cc-bytevector-u16' object."
  (should	(cc-bytevector-p		(cc-bytevector-u16 123)))
  (should	(cc-integer-bytevector-p	(cc-bytevector-u16 123)))
  (should (not	(cc-float-bytevector-p		(cc-bytevector-u16 123))))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-u16 123))))
  (should 	(cc-bytevector-u16-p		(cc-bytevector-u16 123)))
  (should (not	(cc-bytevector-u32-p		(cc-bytevector-u16 123))))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-u16 123))))
  (should (not	(cc-bytevector-s8-p		(cc-bytevector-u16 123))))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-u16 123))))
  (should (not	(cc-bytevector-s32-p		(cc-bytevector-u16 123))))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-u16 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-u16 123))))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-u16 123))))
  (should (not	(cc-bytevector-ldouble-p	(cc-bytevector-u16 123)))))

(ert-deftest cc-bytevector-s16 ()
  "Build a `cc-bytevector-s16' object."
  (should	(cc-bytevector-p		(cc-bytevector-s16 123)))
  (should	(cc-integer-bytevector-p	(cc-bytevector-s16 123)))
  (should (not	(cc-float-bytevector-p		(cc-bytevector-s16 123))))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-s16 123))))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-s16 123))))
  (should (not	(cc-bytevector-u32-p		(cc-bytevector-s16 123))))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-s16 123))))
  (should (not	(cc-bytevector-s8-p		(cc-bytevector-s16 123))))
  (should 	(cc-bytevector-s16-p		(cc-bytevector-s16 123)))
  (should (not	(cc-bytevector-s32-p		(cc-bytevector-s16 123))))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-s16 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-s16 123))))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-s16 123))))
  (should (not	(cc-bytevector-ldouble-p	(cc-bytevector-s16 123)))))

;;; --------------------------------------------------------------------

(ert-deftest cc-bytevector-u32 ()
  "Build a `cc-bytevector-u32' object."
  (should	(cc-bytevector-p		(cc-bytevector-u32 123)))
  (should	(cc-integer-bytevector-p	(cc-bytevector-u32 123)))
  (should (not	(cc-float-bytevector-p		(cc-bytevector-u32 123))))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-u32 123))))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-u32 123))))
  (should 	(cc-bytevector-u32-p		(cc-bytevector-u32 123)))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-u32 123))))
  (should (not	(cc-bytevector-s8-p		(cc-bytevector-u32 123))))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-u32 123))))
  (should (not	(cc-bytevector-s32-p		(cc-bytevector-u32 123))))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-u32 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-u32 123))))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-u32 123))))
  (should (not	(cc-bytevector-ldouble-p	(cc-bytevector-u32 123)))))

(ert-deftest cc-bytevector-s32 ()
  "Build a `cc-bytevector-s32' object."
  (should	(cc-bytevector-p		(cc-bytevector-s32 123)))
  (should	(cc-integer-bytevector-p	(cc-bytevector-s32 123)))
  (should (not	(cc-float-bytevector-p		(cc-bytevector-s32 123))))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-s32 123))))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-s32 123))))
  (should (not	(cc-bytevector-u32-p		(cc-bytevector-s32 123))))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-s32 123))))
  (should (not 	(cc-bytevector-s8-p		(cc-bytevector-s32 123))))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-s32 123))))
  (should 	(cc-bytevector-s32-p		(cc-bytevector-s32 123)))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-s32 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-s32 123))))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-s32 123))))
  (should (not	(cc-bytevector-ldouble-p	(cc-bytevector-s32 123)))))

;;; --------------------------------------------------------------------

(ert-deftest cc-bytevector-u64 ()
  "Build a `cc-bytevector-u64' object."
  (should	(cc-bytevector-p		(cc-bytevector-u64 123)))
  (should	(cc-integer-bytevector-p	(cc-bytevector-u64 123)))
  (should (not	(cc-float-bytevector-p		(cc-bytevector-u64 123))))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-u64 123))))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-u64 123))))
  (should (not 	(cc-bytevector-u32-p		(cc-bytevector-u64 123))))
  (should 	(cc-bytevector-u64-p		(cc-bytevector-u64 123)))
  (should (not	(cc-bytevector-s8-p		(cc-bytevector-u64 123))))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-u64 123))))
  (should (not	(cc-bytevector-s32-p		(cc-bytevector-u64 123))))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-u64 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-u64 123))))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-u64 123))))
  (should (not	(cc-bytevector-ldouble-p	(cc-bytevector-u64 123)))))

(ert-deftest cc-bytevector-s64 ()
  "Build a `cc-bytevector-642' object."
  (should	(cc-bytevector-p		(cc-bytevector-s64 123)))
  (should	(cc-integer-bytevector-p	(cc-bytevector-s64 123)))
  (should (not	(cc-float-bytevector-p		(cc-bytevector-s64 123))))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-s64 123))))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-s64 123))))
  (should (not	(cc-bytevector-u32-p		(cc-bytevector-s64 123))))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-s64 123))))
  (should (not 	(cc-bytevector-s8-p		(cc-bytevector-s64 123))))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-s64 123))))
  (should (not 	(cc-bytevector-s32-p		(cc-bytevector-s64 123))))
  (should 	(cc-bytevector-s64-p		(cc-bytevector-s64 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-s64 123))))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-s64 123))))
  (should (not	(cc-bytevector-ldouble-p	(cc-bytevector-s64 123))))

;;; --------------------------------------------------------------------

(ert-deftest cc-bytevector-float ()
  "Build a `cc-bytevector-float' object."
  (should	(cc-bytevector-p		(cc-bytevector-float 123)))
  (should (not	(cc-integer-bytevector-p	(cc-bytevector-float 123))))
  (should 	(cc-float-bytevector-p		(cc-bytevector-float 123)))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-float 123))))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-float 123))))
  (should (not	(cc-bytevector-u32-p		(cc-bytevector-float 123))))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-float 123))))
  (should (not	(cc-bytevector-s8-p		(cc-bytevector-float 123))))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-float 123))))
  (should (not	(cc-bytevector-s32-p		(cc-bytevector-float 123))))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-float 123))))
  (should 	(cc-bytevector-float-p		(cc-bytevector-float 123)))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-float 123))))
  (should (not	(cc-bytevector-ldouble-p	(cc-bytevector-float 123)))))

(ert-deftest cc-bytevector-double ()
  "Build a `cc-bytevector-double' object."
  (should	(cc-bytevector-p		(cc-bytevector-double 123)))
  (should (not	(cc-integer-bytevector-p	(cc-bytevector-double 123))))
  (should 	(cc-float-bytevector-p		(cc-bytevector-double 123)))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-double 123))))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-double 123))))
  (should (not	(cc-bytevector-u32-p		(cc-bytevector-double 123))))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-double 123))))
  (should (not	(cc-bytevector-s8-p		(cc-bytevector-double 123))))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-double 123))))
  (should (not	(cc-bytevector-s32-p		(cc-bytevector-double 123))))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-double 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-double 123))))
  (should 	(cc-bytevector-double-p		(cc-bytevector-double 123)))
  (should (not	(cc-bytevector-ldouble-p	(cc-bytevector-double 123)))))

(ert-deftest cc-bytevector-ldouble ()
  "Build a `cc-bytevector-ldouble' object."
  (should	(cc-bytevector-p		(cc-bytevector-ldouble 123)))
  (should (not	(cc-integer-bytevector-p	(cc-bytevector-ldouble 123))))
  (should 	(cc-float-bytevector-p		(cc-bytevector-ldouble 123)))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-ldouble 123))))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-ldouble 123))))
  (should (not	(cc-bytevector-u32-p		(cc-bytevector-ldouble 123))))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-ldouble 123))))
  (should (not	(cc-bytevector-s8-p		(cc-bytevector-ldouble 123))))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-ldouble 123))))
  (should (not	(cc-bytevector-s32-p		(cc-bytevector-ldouble 123))))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-ldouble 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-ldouble 123))))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-ldouble 123))))
  (should 	(cc-bytevector-ldouble-p	(cc-bytevector-ldouble 123))))


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; test.el ends here
