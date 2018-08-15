;;; rail-mime-example.el --- Replace Agent-string Internal Library

;; Copyright (C) 2000 by Free Software Foundation, Inc.

;; Author: SHIMADA Mitsunobu <simm-emacs@fan.gr.jp>
;; Keywords: ~/.mime-example, SEMI, Rail

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

;;

;;; Code:
(eval-when-compile
  (require 'rail-common)
  (require 'mime-view))

(defun rail-mime-example-save-primitive (func)
  (save-excursion
    (or (and rail-user-agent-convert-mime-example
             (set-buffer (get-buffer-create rail-temporary-buffer-name))
             (progn
               (erase-buffer)
               (insert mime-view-version)
               (goto-char (point-min))
               (and (looking-at rail-mime-view-version-format)
                    (eq 'semi (cdr (assoc (buffer-substring
                                           (match-beginning 1)
                                           (match-end 1))
                                          rail-product-name-alist)))
                    (rail-replace-codename-primitive
                     rail-mime-view-version-format
                     rail-additional-semi-codename-alist rail-semi-codename-alist))
               (let ((mime-view-version
                      (buffer-substring (point-min) (point-max))))
                 (funcall func))
               t))
        (funcall func))))

(defun rail-mime-example-make-funcs (func)
  (let ((new (intern (concat "rail-" (prin1-to-string func))))
        (old (intern (concat "rail-" (prin1-to-string func) "-original"))))
    (fset old  (symbol-function func))
    (fset func new)))

(defun rail-mime-save-situation-examples ()
  (let ((mime-situation-examples-file-coding-system 'ctext))
    (rail-mime-example-save-primitive 'rail-mime-save-situation-examples-original)))
(rail-mime-example-make-funcs 'mime-save-situation-examples)

(provide 'rail-mime-example)

;;; rail-mime-example.el ends here
