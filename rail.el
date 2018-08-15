;;; rail.el --- Replace Agent-string Internal Library

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: SHIMADA Mitsunobu <simm-emacs@fan.gr.jp>
;; Keywords: MULE, XEmacs, Meadow, UTF-2000, Genji, FLIM, SEMI, Rail

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

;; Current version supports only MULE, FLIM, and SEMI.

;;; Code:

(require 'rail-vars)

(cond ((featurep 'xemacs)
       (require 'rail-xmas))
      ((featurep 'mule)
       (require 'rail-mule)))

;; Replace ISO-8859-1 character into ISO-8859-4
(and rail-user-agent-replace-into-iso8859-4
     (fboundp 'eval-after-load)
     (eval-after-load "mime-def"
       '(aset mime-library-product 2
	      (rail-replace-into-iso8859-4 (aref mime-library-product 2)))))
(and rail-user-agent-replace-into-iso8859-4
     (fboundp 'eval-after-load)
     (eval-after-load "semi-def"
       '(aset mime-user-interface-product 2
	      (rail-replace-into-iso8859-4 (aref mime-user-interface-product 2)))))

;; load rail-user-agent.el
(if (featurep 'semi-def)
    (load "rail-user-agent")
  (add-hook 'mime-setup-load-hook
	    '(lambda () (load "rail-user-agent"))))

;; load rail-mime-example-1-13.el for SEMI-1.13.*
(and (fboundp 'eval-after-load)
     (eval-after-load "mime-play"
       (progn
	 (load "rail-mime-example-1-13")
	 '(rail-mime-example-1-13-setup))))

;; load rail-mime-example-1-14.el for SEMI-1.14.*
(and (fboundp 'eval-after-load)
     (eval-after-load "mime-view"
       (progn
	 (load "rail-mime-example-1-14")
	 '(rail-mime-example-1-14-setup))))

;; for irchat-pj
(require 'rail-pj)

;; That's all
(provide 'rail)

;;; rail.el ends here
