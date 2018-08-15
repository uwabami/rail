;;; rail.el --- Replace Agent-string Internal Library

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: SHIMADA Mitsunobu <simm-emacs@fan.gr.jp>
;;         Youhei SASAKI <uwabami@gfd-dennou.org>
;; Version: 1.2.12
;; Package-Requires: ((semi "1.14")(emacs "24.4"))
;; Keywords: MULE, UTF-2000, Genji, FLIM, SEMI, Rail
;; URL: https://github.com/uwabami/rail/

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
(require 'rail-common)
(require 'mime-def)

(if rail-emulate-genjis
    (setq mule-version (rail-replace-codename mule-version nil
                                              rail-additional-mule-codename-alist
                                              rail-mule-codename-alist)))

;; Replace ISO-8859-1 character into ISO-8859-4
(if rail-user-agent-replace-into-iso8859-4
    (with-eval-after-load 'mime-def
      '(aset mime-library-product 2
             (rail-replace-into-iso8859-4 (aref mime-library-product 2)))))
(if rail-user-agent-replace-into-iso8859-4
    (with-eval-after-load 'semi-def
      '(aset mime-user-interface-product 2
             (rail-replace-into-iso8859-4 (aref mime-user-interface-product 2)))))

;; load rail-user-agent.el
(add-hook 'mime-setup-load-hook
          '(lambda () (load "rail-user-agent")))

;; That's all
(provide 'rail)

;;; rail.el ends here
