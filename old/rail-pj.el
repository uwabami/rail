;;; rail-pj.el --- Replace Agent-string Internal Library

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: SHIMADA Mitsunobu <simm@irc.fan.gr.jp>
;; Keywords: irchat-pj, Rail

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

;; compatible with irchat-pj 2.4.24.11 or later

;;; Code:

(defun rail-pj-bugfix-version ()
  "Bug fix for irchat-pj-version-string."
  (let (buf result)
    (save-excursion
      (setq buf (get-buffer-create rail-temporary-buffer-name))
      (if (set-buffer buf)
	  (progn
	    (erase-buffer)
	    (insert irchat-pj-version-string)
	    ;; Meadow
	    (goto-char (point-min))
	    (and (re-search-forward "Meadow\\s +" nil t)
		 (re-search-forward ":[0-9]+)" nil t)
		 (goto-char (match-beginning 0))
		 (progn
		   (delete-char 1)
		   (insert "/")))
	    ;; add "powered by rail-*"
	    (goto-char (point-min))
	    (and (re-search-forward " :" nil t)
		 (progn
		   (goto-char (match-beginning 0))
		   (insert " powered by rail-" rail-version)))
	    ;; result
	    (setq irchat-pj-version-string (buffer-substring (point-min) (point-max)))
	    (kill-buffer buf)))
      irchat-pj-version-string)))

(defun rail-pj-replace-version ()
  "Replace irchat-pj-version-string."
  (let (buf result)
    (save-excursion
      (setq buf (get-buffer-create rail-temporary-buffer-name))
      (if (set-buffer buf)
	  (progn
	    (erase-buffer)
	    (insert irchat-pj-version-string)
	    ;; MULE
	    (goto-char (point-min))
	    (and (re-search-forward "MULE\\s +" nil t)
		 (rail-replace-codename-primitive
		  rail-pj-mule-format
		  rail-additional-mule-codename-alist rail-mule-codename-alist))
	    ;; Meadow
	    (goto-char (point-min))
	    (and (re-search-forward "Meadow\\s +" nil t)
		 (rail-replace-codename-meadow ?: ?/)
		 (rail-replace-codename-meadow ?/))
	    ;; XEmacs
	    (goto-char (point-min))
	    (and (re-search-forward "XEmacs\\s +" nil t)
		 (rail-replace-codename-primitive
		  rail-pj-xmas-format
		  rail-additional-xmas-codename-alist rail-xmas-codename-alist))
	    ;; UTF-2000-Mule
	    (goto-char (point-min))
	    (and (re-search-forward "UTF-2000-MULE" nil t)
		 (looking-at rail-pj-utf-2000-format)
		 (let* ((beg (match-beginning 0))
			(end (match-end 0)))
		   (goto-char beg)
		   (delete-region beg end)
		   (insert " " utf-2000-version)
		   (goto-char beg)
		   (forward-char 1)
		   (rail-replace-codename-primitive
		    rail-pj-utf-2000-retry-format
		    rail-additional-utf2000-codename-alist rail-utf2000-codename-alist)))
	    ;; add "powered by rail-*"
	    (goto-char (point-min))
	    (and (re-search-forward " :" nil t)
		 (progn
		   (goto-char (match-beginning 0))
		   (insert " powered by rail-" rail-version)))
	    ;; result
	    (setq irchat-pj-version-string (buffer-substring (point-min) (point-max)))
	    (kill-buffer buf)))
      irchat-pj-version-string)))

(if rail-pj-convert
    (if (featurep 'irchat-pj-version-string)
	(rail-pj-replace-version)
      (add-hook 'irchat-pj-version-string-hook 'rail-pj-replace-version))
  (if (featurep 'irchat-pj-version-string)
      (rail-pj-bugfix-version)
    (add-hook 'irchat-pj-version-string-hook 'rail-pj-bugfix-version)))

(provide 'rail-pj)

;;; rail-pj.el ends here
