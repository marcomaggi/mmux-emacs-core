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
(require 'mmec)


;;;; bytevector objects: getters and setters

(ert-deftest mmec-bytevector-u8 ()
  "Setters and getters for a `mmec-bytevector-u8' object."
  (let ((bv	(mmec-bytevector-u8 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-uint8 (+ 10 i))))
    (dotimes (i 10)
      (should (cc= (mmec-uint8 (+ 10 i))
    		   (mmec-bytevector-ref bv i))))))

(ert-deftest mmec-bytevector-s8 ()
  "Setters and getters for a `mmec-bytevector-s8' object."
  (let ((bv	(mmec-bytevector-s8 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-sint8 (+ -10 i))))
    (dotimes (i 10)
      (should (cc= (mmec-sint8 (+ -10 i))
    		   (mmec-bytevector-ref bv i))))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-bytevector-u16 ()
  "Setters and getters for a `mmec-bytevector-u16' object."
  (let ((bv	(mmec-bytevector-u16 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-uint16 (+ 10 i))))
    (dotimes (i 10)
      (should (cc= (mmec-uint16 (+ 10 i))
    		   (mmec-bytevector-ref bv i))))))

(ert-deftest mmec-bytevector-s16 ()
  "Setters and getters for a `mmec-bytevector-s16' object."
  (let ((bv	(mmec-bytevector-s16 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-sint16 (+ -10 i))))
    (dotimes (i 10)
      (should (cc= (mmec-sint16 (+ -10 i))
    		   (mmec-bytevector-ref bv i))))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-bytevector-u32 ()
  "Setters and getters for a `mmec-bytevector-u32' object."
  (let ((bv	(mmec-bytevector-u32 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-uint32 (+ 10 i))))
    (dotimes (i 10)
      (should (cc= (mmec-uint32 (+ 10 i))
    		   (mmec-bytevector-ref bv i))))
    nil))

(ert-deftest mmec-bytevector-s32 ()
  "Setters and getters for a `mmec-bytevector-s32' object."
  (let ((bv	(mmec-bytevector-s32 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-sint32 (+ -10 i))))
    (dotimes (i 10)
      (should (cc= (mmec-sint32 (+ -10 i))
    		   (mmec-bytevector-ref bv i))))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-bytevector-u64 ()
  "Setters and getters for a `mmec-bytevector-u64' object."
  (let ((bv	(mmec-bytevector-u64 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-uint64 (+ 10 i))))
    (dotimes (i 10)
      (should (cc= (mmec-uint64 (+ 10 i))
    		   (mmec-bytevector-ref bv i))))))

(ert-deftest mmec-bytevector-s64 ()
  "Setters and getters for a `mmec-bytevector-s64' object."
  (let ((bv	(mmec-bytevector-s64 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-sint64 (+ -10 i))))
    (dotimes (i 10)
      (should (cc= (mmec-sint64 (+ -10 i))
    		   (mmec-bytevector-ref bv i))))))


;;;; slots inspection

(ert-deftest mmec-bytevector-u8-slots ()
  "Inspect the slots of a `mmec-bytevector-u8' object."
  (let ((bv	(mmec-bytevector-u8 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 1 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 1) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (not (mmec-integer-bytevector-signed bv)))))

(ert-deftest mmec-bytevector-u16-slots ()
  "Inspect the slots of a `mmec-bytevector-u16' object."
  (let ((bv	(mmec-bytevector-u16 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 2 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 2) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (not (mmec-integer-bytevector-signed bv)))))

(ert-deftest mmec-bytevector-u32-slots ()
  "Inspect the slots of a `mmec-bytevector-u32' object."
  (let ((bv	(mmec-bytevector-u32 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 4 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 4) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (not (mmec-integer-bytevector-signed bv)))))

(ert-deftest mmec-bytevector-u64-slots ()
  "Inspect the slots of a `mmec-bytevector-u64' object."
  (let ((bv	(mmec-bytevector-u64 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 8 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 8) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (not (mmec-integer-bytevector-signed bv)))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-bytevector-s8-slots ()
  "Inspect the slots of a `mmec-bytevector-s8' object."
  (let ((bv	(mmec-bytevector-s8 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 1 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 1) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (mmec-integer-bytevector-signed bv))))

(ert-deftest mmec-bytevector-s16-slots ()
  "Inspect the slots of a `mmec-bytevector-s16' object."
  (let ((bv	(mmec-bytevector-s16 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 2 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 2) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (mmec-integer-bytevector-signed bv))))

(ert-deftest mmec-bytevector-s32-slots ()
  "Inspect the slots of a `mmec-bytevector-s32' object."
  (let ((bv	(mmec-bytevector-s32 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 4 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 4) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (mmec-integer-bytevector-signed bv))))

(ert-deftest mmec-bytevector-s64-slots ()
  "Inspect the slots of a `mmec-bytevector-s64' object."
  (let ((bv	(mmec-bytevector-s64 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 8 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 8) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (mmec-integer-bytevector-signed bv))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-bytevector-float-slots ()
  "Inspect the slots of a `mmec-bytevector-float' object."
  (let ((bv	(mmec-bytevector-float 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= mmec-SIZEOF_FLOAT (mmec-bytevector-slot-size bv)))
    (should (= (* 10 mmec-SIZEOF_FLOAT) (mmec-bytevector-number-of-allocated-bytes bv)))))

(ert-deftest mmec-bytevector-double-slots ()
  "Inspect the slots of a `mmec-bytevector-double' object."
  (let ((bv	(mmec-bytevector-double 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= mmec-SIZEOF_DOUBLE (mmec-bytevector-slot-size bv)))
    (should (= (* 10 mmec-SIZEOF_DOUBLE) (mmec-bytevector-number-of-allocated-bytes bv)))))

(ert-deftest mmec-bytevector-ldouble-slots ()
  "Inspect the slots of a `mmec-bytevector-ldouble' object."
  (let ((bv	(mmec-bytevector-ldouble 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= mmec-SIZEOF_LDOUBLE (mmec-bytevector-slot-size bv)))
    (should (= (* 10 mmec-SIZEOF_LDOUBLE) (mmec-bytevector-number-of-allocated-bytes bv)))))


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; bytevector-objects.el ends here
