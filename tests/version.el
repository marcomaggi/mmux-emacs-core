;;; version.el --- dynamic module test

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

(ert-deftest version-string ()
  "Retrieve the version string."
  (should (stringp (mmec-version-string)))
  (should (string= "Return the version string."
		   (documentation 'mmec-version-string))))

(ert-deftest version-interface-current ()
  "Retrieve the interface current version."
  (should (<= 0 (mmec-version-interface-current)))
  (should (string= "Return the interface version current number."
		   (documentation 'mmec-version-interface-current))))

(ert-deftest version-interface-revision ()
  "Retrieve the interface revision version."
  (should (<= 0 (mmec-version-interface-revision)))
  (should (string= "Return the interface version revision number."
		   (documentation 'mmec-version-interface-revision))))

(ert-deftest version-interface-age ()
  "Retrieve the interface age version."
  (should (<= 0 (mmec-version-interface-age)))
  (should (string= "Return the interface version age number."
		   (documentation 'mmec-version-interface-age))))

(ert-run-tests-batch-and-exit)

;;; test.el ends here
