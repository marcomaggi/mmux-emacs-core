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
  (should (not	(cc-bytevector-long-double-p	(cc-bytevector-u8 123)))))

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
  (should (not	(cc-bytevector-long-double-p	(cc-bytevector-s8 123)))))

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
  (should (not	(cc-bytevector-long-double-p	(cc-bytevector-u16 123)))))

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
  (should (not	(cc-bytevector-long-double-p	(cc-bytevector-s16 123)))))

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
  (should (not	(cc-bytevector-long-double-p	(cc-bytevector-u32 123)))))

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
  (should (not	(cc-bytevector-long-double-p	(cc-bytevector-s32 123)))))

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
  (should (not	(cc-bytevector-long-double-p	(cc-bytevector-u64 123)))))

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
  (should (not	(cc-bytevector-long-double-p	(cc-bytevector-s64 123))))

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
  (should (not	(cc-bytevector-long-double-p	(cc-bytevector-float 123)))))

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
  (should (not	(cc-bytevector-long-double-p	(cc-bytevector-double 123)))))

(ert-deftest cc-bytevector-long-double ()
  "Build a `cc-bytevector-long-double' object."
  (should	(cc-bytevector-p		(cc-bytevector-long-double 123)))
  (should (not	(cc-integer-bytevector-p	(cc-bytevector-long-double 123))))
  (should 	(cc-float-bytevector-p		(cc-bytevector-long-double 123)))
  (should (not	(cc-bytevector-u8-p		(cc-bytevector-long-double 123))))
  (should (not	(cc-bytevector-u16-p		(cc-bytevector-long-double 123))))
  (should (not	(cc-bytevector-u32-p		(cc-bytevector-long-double 123))))
  (should (not	(cc-bytevector-u64-p		(cc-bytevector-long-double 123))))
  (should (not	(cc-bytevector-s8-p		(cc-bytevector-long-double 123))))
  (should (not	(cc-bytevector-s16-p		(cc-bytevector-long-double 123))))
  (should (not	(cc-bytevector-s32-p		(cc-bytevector-long-double 123))))
  (should (not	(cc-bytevector-s64-p		(cc-bytevector-long-double 123))))
  (should (not	(cc-bytevector-float-p		(cc-bytevector-long-double 123))))
  (should (not	(cc-bytevector-double-p		(cc-bytevector-long-double 123))))
  (should 	(cc-bytevector-long-double-p	(cc-bytevector-long-double 123))))


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; test.el ends here
