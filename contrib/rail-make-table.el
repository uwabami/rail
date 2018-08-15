;; rail-make-table.el -- rail-table-*.el generater

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: TAKAHASHI Kaoru <kaoru@kaisei.org>
;;	SHIMADA Mitsunobu <simm-emacs@fan.gr.jp>
;; Keywords: MULE, Genji, FLIM, SEMI, Rail

;; This file is part of Rail

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

;; usage:
;;    emacs -batch -l ./rail-make-table.el -f rail-make-table-semi
;;    emacs -batch -l ./rail-make-table.el -f rail-make-table-flim
;;    emacs -batch -l ./rail-make-table.el -f rail-make-table-mule


;;; Code:

;; define WHEN
(or (fboundp 'when)
    (defmacro when (cond &rest body)
      "if COND yields non-nil, do BODY, else return nil."
      (list 'if cond (cons 'progn body))))

;; customize variables
(defvar rail-make-table-write-directory nil
  "Write \"rail-table-*.el\" directory. nil means current directory.")

(defvar rail-make-table-read-directory "./contrib"
  "Read \"*_VERSION\" directory. nil means current directory.")

;; make-table functions
(defun rail-make-table-flim ()
  "Make `rail-flim-table.el'."
  (rail-make-table "flim" t))

(defun rail-make-table-semi ()
  "Make `rail-semi-table.el'."
  (rail-make-table "semi" t))

(defun rail-make-table-mule ()
  "Make `rail-mule-table.el'."
  (rail-make-table "mule" nil))

(defun rail-make-table-meadow ()
  "Make `rail-meadow-table.el'."
  (rail-make-table "meadow" t))

;; common functions
(defun rail-make-table (identifier &optional add-flag)
  "Make rail-table-*.el."
  (rail-make-table-write
   identifier
   (rail-make-table-read identifier add-flag)))

(defun rail-make-table-read (identifier &optional add-flag)
  "Read IDENTIFIER_VERSION.  Return codename-alist.
If non-nil ADD-FLAG, read ADD_IDENTIFIER_VERSION."
  (let (codename-alist)
    (setq codename-alist
          (rail-parse-file
           (concat (upcase identifier) "_VERSION")
           rail-make-table-read-directory))
    (when add-flag
      (setq codename-alist
            (append
             (rail-parse-file 
              (concat "ADD_" (upcase identifier) "_VERSION")
              rail-make-table-read-directory t)
             codename-alist)))
    codename-alist))

; (defvar rail-make-table-coding-system-alist
;   '(("flim" . "iso-2022-8bit-ss2")
;     ("semi" . "iso-2022-8bit-ss2")
;     ("mule" . "iso-2022-7bit")
;     ("meadow" . "iso-2022-7bit"))
;   "Coding-system for rail-table-*.el alist.")

(defun rail-make-table-write (identifier value &optional docstring)
  "Make rail-table-IDENTIFIER.el."
  (let* (;;(coding (cdr (assoc identifier rail-make-table-coding-system-alist)))
         ;;(coding-system-for-write (intern-soft coding))
	 (coding-system-for-write 'ctext)
         (basename (concat "rail-table-" identifier ".el"))
         (file (expand-file-name basename rail-make-table-write-directory))
         (temp-buffer (generate-new-buffer " *rail-make-table*")))
    (unwind-protect
        (save-excursion
          (set-buffer temp-buffer)
          ;; insert
          ;(unless (string= "" coding)
	  ; (insert (format ";; -*- coding: %s; -*-\n\n" coding)))
	  (insert (format ";; -*- coding: ctext; -*-\n\n"))
          (rail-make-table-insert-defvar
           (concat "rail-" identifier "-codename-alist")
           value docstring)
          (insert (format "(provide 'rail-table-%s)\n"  identifier))
          ;; write
          (if (file-writable-p file)
              (if (get-file-buffer file)
                  (error "%s is locked. Can't overwrite" file)
                (write-region (point-min) (point-max) file)
                file)
            (error "%s is write-protected" file)))
      (kill-buffer temp-buffer))))



(defun rail-make-table-insert-defvar (symbol-name value &optional docstring)
  "Insert defvar defnition."
  (insert (format "(defvar %s\n'(" symbol-name))
  (rail-make-table-insert-dotpair (car value))
  (mapcar #'rail-make-table-insert-dotpair
          (cdr value))
  ;; last alist
  (delete-char -1)
  (insert ")")
  ;; insert docstring
  (if docstring
      (insert (format "\n%S)" docstring))
    (insert ")"))
  (lisp-indent-line)
  (end-of-line)
  (insert "\n\n"))

(defun rail-make-table-insert-dotpair (dotpair)
  "Insert alist in variable definition."
  (when (consp dotpair)
    (insert (format "(%-20S . %S)\n" (car dotpair) (cdr dotpair)))
    (forward-line -1)
    (lisp-indent-line)
    (forward-line 1)))


;;;; parser
;; flim, semi
(defvar rail-parse-line-flim-regexp
  (concat 
   "^\\([0-9b.]+\\|-+\\)\t+"            ; version
   "\\([^\t\n]+\\)\t+\\([^\t\n]+\\)"    ; en ja
   "\\(\t.*\\)?$")                      ; comment
  "FLIM, SEMI codename line regexp.")

(defun rail-parse-line-flim ()
  "Parse line in `(FLIM|SEMI)_VERSION', return en-code and ja-code pair.
When invaild record, return nil."
  (let (en ja)
    (save-excursion
      (beginning-of-line)
      (save-match-data
	;; VERSION file (SEMI) bug
	(when (looking-at "\\(-------\\) \\(Tamura\\)")
	  ;(forward-char 7)
	  ;(delete-char 1)
	  ;(insert "\t")
	  (replace-match "\\1\t\\2")
	  (beginning-of-line))
        ;; match
        (when (looking-at rail-parse-line-flim-regexp)
          (setq en (match-string 2))
          (setq ja (match-string 3))
          ;; abailable-p
          (if (or (null en) (null ja)
                  (string-match "^-----" en) (string-match "^-----" ja) 
                  (string-match "^(.*)$" en) (string-match "^(.*)$" ja))
              nil                       ; invalid
            (cons en ja)))))))          ; else return dotpair

(defalias 'rail-parse-line-semi 'rail-parse-line-flim)
(defalias 'rail-parse-line-meadow 'rail-parse-line-flim)

;; mule
(defvar rail-parse-line-mule-regexp
  (concat
   "^ +\\([0-9]+\\|\*\*\\)\t+"          ; volume
   "\\([^\t\n]+\\)\t+\\([^\t\n]+\\)\t+\\([^\t\n]+\\)" ; en hira kanji
;;;  "\\(\t+\\([^\t\n]+\\)\t+\\([^\t\n]+\\)?$"
   "\\(\t.*\\)?$")                      ; mule-version date 
  "MULE codename line regexp.")

(defun rail-parse-line-mule ()
  "Parse line in `MULE_VERSION', return en-code and ja-code pair.
When invaild record, return nil."
  ;; comment check
  (let (en ja)
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at rail-parse-line-mule-regexp)
          (setq en (match-string 2))
          (setq ja (match-string 4))
          (cons en ja))))))

;; region-parser
(defun rail-parse-region (begin end parser)
  "Apply PARSER function each line in region.  Return alist."
  (let (code-alist dotpair)
    (save-excursion
      (goto-char end)                   ; end -> begin
      (save-match-data
        (while (> (point) begin)
          (setq dotpair (funcall parser))
          (when (consp dotpair)
            (add-to-list 'code-alist dotpair))
          (forward-line -1))))
    code-alist))

(defvar rail-parse-file-parser-alist
  '(("FLIM_VERSION"      .  rail-parse-line-flim)
    ("ADD_FLIM_VERSION"  .  rail-parse-line-flim)
    ("SEMI_VERSION"      .  rail-parse-line-semi)
    ("ADD_SEMI_VERSION"  .  rail-parse-line-semi)
    ("MEADOW_VERSION"    .  rail-parse-line-meadow)
    ("ADD_MEADOW_VERSION" . rail-parse-line-meadow)
    ("MULE_VERSION"      .  rail-parse-line-mule))
  "Alist of file and parser pair.")

(defvar rail-parse-file-coding-system-alist
  '(("FLIM_VERSION"      .  iso-2022-8bit-ss2)
    ("SEMI_VERSION"      .  iso-2022-8bit-ss2)
    ("MEADOW_VERSION"    .  iso-2022-jp)
    ("MULE_VERSION"      .  iso-2022-jp))
  "Alist of file and coding-system pair.")

(defun rail-parse-file (basename &optional directory noerror)
  "Parse *_VERSION.  Return alist."
  (let* ((file (expand-file-name basename directory))
	 (parser (cdr (assoc basename rail-parse-file-parser-alist)))
	 (coding-system-for-read (cdr (assoc basename rail-parse-file-coding-system-alist)))
	 (temp-buffer (generate-new-buffer " *rail-parse*")))
    (if (file-readable-p file)
        (unwind-protect
            (save-excursion
              (set-buffer temp-buffer)
              (insert-file-contents file)
              (rail-parse-region (point-min) (point-max) parser))
          (kill-buffer temp-buffer))
      ;; can't read
      (unless noerror
        (error "Can't read %s" file)))))


(provide 'rail-make-table)

;;; rail-make-table.el ends here
