;;; rail-common.el --- Replace Agent-string Internal Library -*- coding: iso-2022-7bit-ss2; -*-

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: SHIMADA Mitsunobu <simm-emacs@fan.gr.jp>
;;         Youhei SASAKI <uwabami@gfd-dennou.org>
;; Keywords: i18n, internal, rail

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

(require 'rail-vars)

(defun rail-replace-character (string src dst)
  "Replace character src -> dst"
  (let ((len (length string))
        (i 0))
    (while (< i len)
      (if (eq src (aref string i))
          (aset string i dst))
      (setq i (1+ i)))
    string))

(defun rail-replace-into-iso8859-4 (string)
  "Replace ISO-8859-1 string into ISO-8859-4."
  (if rail-user-agent-replace-into-iso8859-4
      (progn
        (rail-replace-character string ?~ ?.DN~)
        (rail-replace-character string ?.AN^ ?.DN^)
        (rail-replace-character string ?.ANo ?.DNo)
        (rail-replace-character string ?.ANO ?.DNO)
        (rail-replace-character string ?.ANr ?.DNr)
        (rail-replace-character string ?.ANR ?.DNR))))

(defmacro rail-assoc (string alist direction)
  "Do assoc or rassoc, according to direction.
If direction is non-nil, returns (cdr (assoc string alist)).
If direction is nil, returns (car (rassoc string alist))."
  (if (symbol-value direction)
      (list 'cdr (list 'assoc string alist))
    (list 'car (list 'rassoc string alist))))


(defun rail-replace-codename-primitive (form &rest alist)
  "Replace codename according to pattern."
  (save-excursion
    (if (looking-at form)
        (let* ((beg   (match-beginning 2))
               (end   (match-end 2))
               (code  (buffer-substring beg end))
               (ccode (rail-assoc code (apply 'append alist) rail-convert-direction)))
          (goto-char beg)
          (delete-region beg end)
          (insert (or ccode code))))))

(defun rail-replace-codename (string flag &rest alist)
  "Replace mule-version string."
  (let (buf result)
    (save-excursion
      (setq buf (get-buffer-create rail-temporary-buffer-name))
      (if (set-buffer buf)
          (progn
            (erase-buffer)
            (insert string)
            (goto-char (point-min))
            (if (not flag)
                (apply 'rail-replace-codename-primitive rail-mule-version-header-format alist))
            (setq result (buffer-substring (point-min) (point-max))))
        (kill-buffer buf)))
    (or result string)))

(provide 'rail-common)

;;; rail-common.el ends here
