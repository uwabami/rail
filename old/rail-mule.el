;;; rail-mule.el --- Replace Agent-string Internal Library

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: SHIMADA Mitsunobu <simm-emacs@fan.gr.jp>
;; Keywords: MULE, Meadow, Genji, Rail

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
(require 'rail-common)

(and rail-emulate-genjis
     (featurep 'mule)
     (boundp 'mule-version)
     (setq mule-version (rail-replace-codename mule-version nil
					       rail-additional-mule-codename-alist
					       rail-mule-codename-alist)))

(and rail-mule-replace-meadow-version
     (featurep 'meadow)
     (not (fboundp 'rail-mule-original-Meadow-version))
     (fset 'rail-mule-original-Meadow-version (symbol-function 'Meadow-version))
     (defvar rail-mule-meadow-en-version
       (rail-mule-original-Meadow-version)
       "Meadow-version in ANK code")
     (defvar rail-mule-meadow-ja-version
       (rail-replace-codename rail-mule-meadow-en-version t
			      rail-additional-meadow-codename-alist
			      rail-meadow-codename-alist)
       "Meadow-verion in JISX0208 code")
     (defun rail-mule-meadow-version (&optional dummy)
       "return the Meadow's version in string. 
The optional argument DUMMY is not currently used."
       (if rail-convert-direction
	   rail-mule-meadow-ja-version
	 rail-mule-meadow-en-version))
     (fset 'Meadow-version 'rail-mule-meadow-version))

(provide 'rail-mule)

;;; rail-mule.el ends here
