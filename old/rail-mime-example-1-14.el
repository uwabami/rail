;;; rail-mime-example-1-14.el --- Replace Agent-string Internal Library

;; Copyright (C) 2000 by Free Software Foundation, Inc.

;; Author: SHIMADA Mitsunobu <simm-emacs@fan.gr.jp>
;; Keywords: ~/.mime-example, SEMI-1_14, Rail

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

(require 'rail-mime-example)

(defun rail-mime-example-1-14-setup ()
  (cond ((and (fboundp 'mime-save-situation-examples)
	      (boundp  'mime-situation-examples-file-coding-system)
	      (boundp  'emacs-major-version)
	      (<= 20 emacs-major-version))
	 (defun rail-mime-save-situation-examples ()
	   (let ((mime-situation-examples-file-coding-system 'ctext))
	     (rail-mime-example-save-primitive 'rail-mime-save-situation-examples-original)))
	 (rail-mime-example-make-funcs 'mime-save-situation-examples))
	((and (fboundp 'mime-save-situation-examples)
	      (boundp  'mime-situation-examples-file-coding-system)
	      (boundp  'MULE))
	 (defun rail-mime-save-situation-examples ()
	   (let ((mime-situation-examples-file-coding-system *ctext*))
	     (rail-mime-example-save-primitive 'rail-mime-save-situation-examples-original)))
	 (rail-mime-example-make-funcs 'mime-save-situation-examples))
	((and (fboundp 'mime-save-situation-examples)
	      (boundp  'emacs-major-version)
	      (<= 20 emacs-major-version))
	 (defun rail-mime-save-situation-examples ()
	   (let ((coding-system-for-write 'ctext))
	     (rail-mime-example-save-primitive 'rail-mime-save-situation-examples-original)))
	 (rail-mime-example-make-funcs 'mime-save-situation-examples))
	((and (fboundp 'mime-save-situation-examples)
	      (boundp  'MULE))
	 (defun rail-mime-save-situation-examples ()
	   (let ((flie-coding-system *ctext*))
	     (rail-mime-example-save-primitive 'rail-mime-save-situation-examples-original)))
	 (rail-mime-example-make-funcs 'mime-save-situation-examples))))

(provide 'rail-mime-example-1-14)

;;; rail-mime-example-1-14.el ends here
