;;; bytevector-objects.el --- dynamic module test

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


;;;; bytevector objects: getters and setters

(ert-deftest cc-bytevector-u8 ()
  "Setters and getters for a `cc-bytevector-u8' object."
  (let ((bv	(cc-bytevector-u8 10)))
    (dotimes (i 10)
      (cc-bytevector-set bv i (cc-uint8 (+ 10 i))))
    (dotimes (i 10)
      (should (cc= (cc-uint8 (+ 10 i))
    		   (cc-bytevector-ref bv i))))))

(ert-deftest cc-bytevector-s8 ()
  "Setters and getters for a `cc-bytevector-s8' object."
  (let ((bv	(cc-bytevector-s8 10)))
    (dotimes (i 10)
      (cc-bytevector-set bv i (cc-sint8 (+ -10 i))))
    (dotimes (i 10)
      (should (cc= (cc-sint8 (+ -10 i))
    		   (cc-bytevector-ref bv i))))))

;;; --------------------------------------------------------------------

(ert-deftest cc-bytevector-u16 ()
  "Setters and getters for a `cc-bytevector-u16' object."
  (let ((bv	(cc-bytevector-u16 10)))
    (dotimes (i 10)
      (cc-bytevector-set bv i (cc-uint16 (+ 10 i))))
    (dotimes (i 10)
      (should (cc= (cc-uint16 (+ 10 i))
    		   (cc-bytevector-ref bv i))))))

(ert-deftest cc-bytevector-s16 ()
  "Setters and getters for a `cc-bytevector-s16' object."
  (let ((bv	(cc-bytevector-s16 10)))
    (dotimes (i 10)
      (cc-bytevector-set bv i (cc-sint16 (+ -10 i))))
    (dotimes (i 10)
      (should (cc= (cc-sint16 (+ -10 i))
    		   (cc-bytevector-ref bv i))))))

;;; --------------------------------------------------------------------

(ert-deftest cc-bytevector-u32 ()
  "Setters and getters for a `cc-bytevector-u32' object."
  (let ((bv	(cc-bytevector-u32 10)))
    (dotimes (i 10)
      (cc-bytevector-set bv i (cc-uint32 (+ 10 i))))
    (dotimes (i 10)
      (should (cc= (cc-uint32 (+ 10 i))
    		   (cc-bytevector-ref bv i))))))

(ert-deftest cc-bytevector-s32 ()
  "Setters and getters for a `cc-bytevector-s32' object."
  (let ((bv	(cc-bytevector-s32 10)))
    (dotimes (i 10)
      (cc-bytevector-set bv i (cc-sint32 (+ -10 i))))
    (dotimes (i 10)
      (should (cc= (cc-sint32 (+ -10 i))
    		   (cc-bytevector-ref bv i))))))

;;; --------------------------------------------------------------------

(ert-deftest cc-bytevector-u64 ()
  "Setters and getters for a `cc-bytevector-u64' object."
  (let ((bv	(cc-bytevector-u64 10)))
    (dotimes (i 10)
      (cc-bytevector-set bv i (cc-uint64 (+ 10 i))))
    (dotimes (i 10)
      (should (cc= (cc-uint64 (+ 10 i))
    		   (cc-bytevector-ref bv i))))))

(ert-deftest cc-bytevector-s64 ()
  "Setters and getters for a `cc-bytevector-s64' object."
  (let ((bv	(cc-bytevector-s64 10)))
    (dotimes (i 10)
      (cc-bytevector-set bv i (cc-sint64 (+ -10 i))))
    (dotimes (i 10)
      (should (cc= (cc-sint64 (+ -10 i))
    		   (cc-bytevector-ref bv i))))))


;;;; slots inspection

(ert-deftest cc-bytevector-u8-slots ()
  "Inspect the slots of a `cc-bytevector-u8' object."
  (let ((bv	(cc-bytevector-u8 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= 1 (cc-bytevector-slot-size bv)))
    (should (= (* 10 1) (cc-bytevector-number-of-allocated-bytes bv)))
    (should (not (cc-integer-bytevector-signed bv)))))

(ert-deftest cc-bytevector-u16-slots ()
  "Inspect the slots of a `cc-bytevector-u16' object."
  (let ((bv	(cc-bytevector-u16 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= 2 (cc-bytevector-slot-size bv)))
    (should (= (* 10 2) (cc-bytevector-number-of-allocated-bytes bv)))
    (should (not (cc-integer-bytevector-signed bv)))))

(ert-deftest cc-bytevector-u32-slots ()
  "Inspect the slots of a `cc-bytevector-u32' object."
  (let ((bv	(cc-bytevector-u32 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= 4 (cc-bytevector-slot-size bv)))
    (should (= (* 10 4) (cc-bytevector-number-of-allocated-bytes bv)))
    (should (not (cc-integer-bytevector-signed bv)))))

(ert-deftest cc-bytevector-u64-slots ()
  "Inspect the slots of a `cc-bytevector-u64' object."
  (let ((bv	(cc-bytevector-u64 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= 8 (cc-bytevector-slot-size bv)))
    (should (= (* 10 8) (cc-bytevector-number-of-allocated-bytes bv)))
    (should (not (cc-integer-bytevector-signed bv)))))

;;; --------------------------------------------------------------------

(ert-deftest cc-bytevector-s8-slots ()
  "Inspect the slots of a `cc-bytevector-s8' object."
  (let ((bv	(cc-bytevector-s8 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= 1 (cc-bytevector-slot-size bv)))
    (should (= (* 10 1) (cc-bytevector-number-of-allocated-bytes bv)))
    (should (cc-integer-bytevector-signed bv))))

(ert-deftest cc-bytevector-s16-slots ()
  "Inspect the slots of a `cc-bytevector-s16' object."
  (let ((bv	(cc-bytevector-s16 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= 2 (cc-bytevector-slot-size bv)))
    (should (= (* 10 2) (cc-bytevector-number-of-allocated-bytes bv)))
    (should (cc-integer-bytevector-signed bv))))

(ert-deftest cc-bytevector-s32-slots ()
  "Inspect the slots of a `cc-bytevector-s32' object."
  (let ((bv	(cc-bytevector-s32 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= 4 (cc-bytevector-slot-size bv)))
    (should (= (* 10 4) (cc-bytevector-number-of-allocated-bytes bv)))
    (should (cc-integer-bytevector-signed bv))))

(ert-deftest cc-bytevector-s64-slots ()
  "Inspect the slots of a `cc-bytevector-s64' object."
  (let ((bv	(cc-bytevector-s64 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= 8 (cc-bytevector-slot-size bv)))
    (should (= (* 10 8) (cc-bytevector-number-of-allocated-bytes bv)))
    (should (cc-integer-bytevector-signed bv))))

;;; --------------------------------------------------------------------

(ert-deftest cc-bytevector-float-slots ()
  "Inspect the slots of a `cc-bytevector-float' object."
  (let ((bv	(cc-bytevector-float 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= cc-SIZEOF_FLOAT (cc-bytevector-slot-size bv)))
    (should (= (* 10 cc-SIZEOF_FLOAT) (cc-bytevector-number-of-allocated-bytes bv)))))

(ert-deftest cc-bytevector-double-slots ()
  "Inspect the slots of a `cc-bytevector-double' object."
  (let ((bv	(cc-bytevector-double 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= cc-SIZEOF_DOUBLE (cc-bytevector-slot-size bv)))
    (should (= (* 10 cc-SIZEOF_DOUBLE) (cc-bytevector-number-of-allocated-bytes bv)))))

(ert-deftest cc-bytevector-ldouble-slots ()
  "Inspect the slots of a `cc-bytevector-ldouble' object."
  (let ((bv	(cc-bytevector-ldouble 10)))
    (should (= 10 (cc-bytevector-number-of-slots bv)))
    (should (= cc-SIZEOF_LDOUBLE (cc-bytevector-slot-size bv)))
    (should (= (* 10 cc-SIZEOF_LDOUBLE) (cc-bytevector-number-of-allocated-bytes bv)))))


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; test.el ends here
