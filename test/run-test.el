;;; ;; test/run-test.el ---
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

(message "Running tests on Emacs %s" emacs-version)

;; Utils
(defun rail:test-join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).
\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'rail:test-join-path rest))
    path))

(defconst rail:test-dir
  (if load-file-name
      (file-name-directory load-file-name)
    ;; Fall back to default directory (in case of M-x eval-buffer)
    default-directory)
  "Directory of the test suite.")

(defconst rail:root-dir (expand-file-name (concat rail:test-dir "..")))

;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list rail:test-dir
            rail:root-dir))

(load "rail-test")

;; Run tests
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))

(provide 'run-test)
;;; run-test.el ends here
