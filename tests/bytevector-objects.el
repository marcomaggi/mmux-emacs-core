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

(ert-deftest mmec-uint8-bytevector ()
  "Setters and getters for a `mmec-uint8-bytevector' object."
  (let ((bv	(mmec-uint8-bytevector 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-uint8 (+ 10 i))))
    (dotimes (i 10)
      (should (mmec= (mmec-uint8 (+ 10 i))
		     (mmec-bytevector-ref bv i))))))

(ert-deftest mmec-sint8-bytevector ()
  "Setters and getters for a `mmec-sint8-bytevector' object."
  (let ((bv	(mmec-sint8-bytevector 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-sint8 (+ -10 i))))
    (dotimes (i 10)
      (should (mmec= (mmec-sint8 (+ -10 i))
		     (mmec-bytevector-ref bv i))))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-uint16-bytevector ()
  "Setters and getters for a `mmec-uint16-bytevector' object."
  (let ((bv	(mmec-uint16-bytevector 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-uint16 (+ 10 i))))
    (dotimes (i 10)
      (should (mmec= (mmec-uint16 (+ 10 i))
		     (mmec-bytevector-ref bv i))))))

(ert-deftest mmec-sint16-bytevector ()
  "Setters and getters for a `mmec-sint16-bytevector' object."
  (let ((bv	(mmec-sint16-bytevector 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-sint16 (+ -10 i))))
    (dotimes (i 10)
      (should (mmec= (mmec-sint16 (+ -10 i))
		     (mmec-bytevector-ref bv i))))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-uint32-bytevector ()
  "Setters and getters for a `mmec-uint32-bytevector' object."
  (let ((bv	(mmec-uint32-bytevector 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-uint32 (+ 10 i))))
    (dotimes (i 10)
      (should (mmec= (mmec-uint32 (+ 10 i))
		     (mmec-bytevector-ref bv i))))
    nil))

(ert-deftest mmec-sint32-bytevector ()
  "Setters and getters for a `mmec-sint32-bytevector' object."
  (let ((bv	(mmec-sint32-bytevector 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-sint32 (+ -10 i))))
    (dotimes (i 10)
      (should (mmec= (mmec-sint32 (+ -10 i))
		     (mmec-bytevector-ref bv i))))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-uint64-bytevector ()
  "Setters and getters for a `mmec-uint64-bytevector' object."
  (let ((bv	(mmec-uint64-bytevector 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-uint64 (+ 10 i))))
    (dotimes (i 10)
      (should (mmec= (mmec-uint64 (+ 10 i))
		     (mmec-bytevector-ref bv i))))))

(ert-deftest mmec-sint64-bytevector ()
  "Setters and getters for a `mmec-sint64-bytevector' object."
  (let ((bv	(mmec-sint64-bytevector 10)))
    (dotimes (i 10)
      (mmec-bytevector-set bv i (mmec-sint64 (+ -10 i))))
    (dotimes (i 10)
      (should (mmec= (mmec-sint64 (+ -10 i))
		     (mmec-bytevector-ref bv i))))))


;;;; slots inspection

(ert-deftest mmec-uint8-bytevector-slots ()
  "Inspect the slots of a `mmec-uint8-bytevector' object."
  (let ((bv	(mmec-uint8-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 1 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 1) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (not (mmec-integer-bytevector-signed bv)))))

(ert-deftest mmec-uint16-bytevector-slots ()
  "Inspect the slots of a `mmec-uint16-bytevector' object."
  (let ((bv	(mmec-uint16-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 2 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 2) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (not (mmec-integer-bytevector-signed bv)))))

(ert-deftest mmec-uint32-bytevector-slots ()
  "Inspect the slots of a `mmec-uint32-bytevector' object."
  (let ((bv	(mmec-uint32-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 4 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 4) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (not (mmec-integer-bytevector-signed bv)))))

(ert-deftest mmec-uint64-bytevector-slots ()
  "Inspect the slots of a `mmec-uint64-bytevector' object."
  (let ((bv	(mmec-uint64-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 8 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 8) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (not (mmec-integer-bytevector-signed bv)))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-sint8-bytevector-slots ()
  "Inspect the slots of a `mmec-sint8-bytevector' object."
  (let ((bv	(mmec-sint8-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 1 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 1) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (mmec-integer-bytevector-signed bv))))

(ert-deftest mmec-sint16-bytevector-slots ()
  "Inspect the slots of a `mmec-sint16-bytevector' object."
  (let ((bv	(mmec-sint16-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 2 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 2) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (mmec-integer-bytevector-signed bv))))

(ert-deftest mmec-sint32-bytevector-slots ()
  "Inspect the slots of a `mmec-sint32-bytevector' object."
  (let ((bv	(mmec-sint32-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 4 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 4) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (mmec-integer-bytevector-signed bv))))

(ert-deftest mmec-sint64-bytevector-slots ()
  "Inspect the slots of a `mmec-sint64-bytevector' object."
  (let ((bv	(mmec-sint64-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= 8 (mmec-bytevector-slot-size bv)))
    (should (= (* 10 8) (mmec-bytevector-number-of-allocated-bytes bv)))
    (should (mmec-integer-bytevector-signed bv))))

;;; --------------------------------------------------------------------

(ert-deftest mmec-float-bytevector-slots ()
  "Inspect the slots of a `mmec-float-bytevector' object."
  (let ((bv	(mmec-float-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= mmec-SIZEOF_FLOAT (mmec-bytevector-slot-size bv)))
    (should (= (* 10 mmec-SIZEOF_FLOAT) (mmec-bytevector-number-of-allocated-bytes bv)))))

(ert-deftest mmec-double-bytevector-slots ()
  "Inspect the slots of a `mmec-double-bytevector' object."
  (let ((bv	(mmec-double-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= mmec-SIZEOF_DOUBLE (mmec-bytevector-slot-size bv)))
    (should (= (* 10 mmec-SIZEOF_DOUBLE) (mmec-bytevector-number-of-allocated-bytes bv)))))

(ert-deftest mmec-ldouble-bytevector-slots ()
  "Inspect the slots of a `mmec-ldouble-bytevector' object."
  (let ((bv	(mmec-ldouble-bytevector 10)))
    (should (= 10 (mmec-bytevector-number-of-slots bv)))
    (should (= mmec-SIZEOF_LONG_DOUBLE (mmec-bytevector-slot-size bv)))
    (should (= (* 10 mmec-SIZEOF_LONG_DOUBLE) (mmec-bytevector-number-of-allocated-bytes bv)))))


;;;; done

(ert-run-tests-batch-and-exit)
(garbage-collect)

;;; bytevector-objects.el ends here
