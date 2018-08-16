;;; ;; test/rail-test.el ---
;; Copyright (C) 2018 by Free Software Foundation, Inc.

;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; Keywords: MULE, UTF-2000, Genji, FLIM, SEMI, Rail

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:
(require 'ert)
(require 'rail)
(require 'mime-setup)
(load "mime-edit")

(ert-deftest rail:mule-version ()
  (should (string-equal mule-version "6.0 (花散里)")))

(ert-deftest rail:semi-codename ()
  (should
   (string-equal (mime-product-code-name mime-user-interface-product)
                 "春江")))

(ert-deftest rail:flim-codename ()
  (should
   (string-equal (mime-product-code-name mime-library-product)
                 "五条")))


(provide 'rail-test)
