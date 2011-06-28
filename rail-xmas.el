;;; rail-xmas.el --- Replace Agent-string Internal Library

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: SHIMADA Mitsunobu <simm-emacs@fan.gr.jp>
;; Keywords: XEmacs, UTF-2000-MULE, Rail

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

(let (codename)
  (and rail-xmas-replace-xemacs-codename
       (featurep 'xemacs)
       (boundp 'xemacs-codename)
       (setq codename (rail-assoc xemacs-codename
				  (append rail-additional-xmas-codename-alist
					  rail-xmas-codename-alist)
				  rail-convert-direction))
       (setq xemacs-codename codename)))

(and rail-xmas-replace-utf-2000-version
     (featurep 'utf-2000)
     (boundp 'utf-2000-version)
     (setq utf-2000-version (rail-replace-codename utf-2000-version nil
						   rail-additional-utf2000-codename-alist
						   rail-utf2000-codename-alist)))

(provide 'rail-xmas)

;;; rail-xmas.el ends here
