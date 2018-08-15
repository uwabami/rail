;;; rail-user-agent.el --- Replace Agent-string Internal Library

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: SHIMADA Mitsunobu <simm-emacs@fan.gr.jp>
;; Keywords: User-Agent, MIME-Version, FLIM, SEMI, Rail

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

;; search header
(defun rail-user-agent-search-header (head)
  "Search header string and return region."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward (format "^%s:\\s +" head) nil t)
           (let ((pt (point)))
             (if (catch 'header-end
                   (while (not (eobp))
                     (forward-line 1)
                     (or (looking-at "\\s ")
                         (throw 'header-end t))))
                 (progn
                   (forward-char -1)
                   (cons pt (point)))))))))

;; replace MIME-Version: header dynamically
(defun rail-user-agent-replace-mime-version-region (region)
  "Replace codename in MIME-Version: region."
  (and region
       (car region)
       (cdr region)
       (save-restriction
         (narrow-to-region (car region) (cdr region))
         (goto-char (point-min))
         (and (looking-at rail-mime-version-header-format)
              (rail-replace-codename-primitive
               rail-mime-version-header-format
               rail-additional-semi-codename-alist rail-semi-codename-alist))
         (buffer-substring (point-min) (point-max)))))

;; replace User-Agent: header dynamically
(defun rail-user-agent-replace-user-agent-region (region)
  "Replace codename in User-Agent: region."
  (and region
       (car region)
       (cdr region)
       (save-restriction
         (narrow-to-region (car region) (cdr region))
         (goto-char (point-min))
         (while (not (eobp))
           (skip-chars-forward " \t\r\n")
           (if (not (looking-at "\\([^ \t\r\n/]+\\)/"))
               (skip-chars-forward "^ \t\r\n")
             (let ((kind (rail-assoc
                          (upcase (buffer-substring (match-beginning 1) (match-end 1)))
                          rail-product-name-alist t)))
               (if (not kind)
                   (skip-chars-forward "^ \t\r\n")
                 (goto-char (match-end 0))
                 (or (and (eq 'xmas kind)
                          (looking-at rail-user-agent-header-xmas-format)
                          (rail-replace-codename-primitive
                           rail-user-agent-header-xmas-format
                           rail-additional-xmas-codename-alist rail-xmas-codename-alist))
                     (let* ((skind (prin1-to-string kind))
                            (alist-a
                             (intern (format "rail-additional-%s-codename-alist" skind)))
                            (alist-b
                             (intern (format "rail-%s-codename-alist" skind))))
                       (and (looking-at rail-user-agent-header-format)
                            (boundp alist-a)
                            (boundp alist-b)
                            (rail-replace-codename-primitive
                             rail-user-agent-header-format
                             (symbol-value alist-a) (symbol-value alist-b)))))
                 (and (eq 'meadow kind)
                      (rail-replace-codename-meadow))))))
         (buffer-substring (point-min) (point-max)))))

;; replace header with rail-user-agent-replace-{mime-version|user-agent}-region
(defun rail-user-agent-translate-body (&optional buf)
  "Translate message body. It executes after mime-edit-translate-body."
  (and rail-user-agent-convert-dynamically
       (save-excursion
         (and buf (set-buffer buf))
         (mapc
          (lambda (item)
            (rail-user-agent-replace-mime-version-region
             (rail-user-agent-search-header item)))
          rail-mime-version-header-item-list)
         (mapc
          (lambda (item)
            (rail-user-agent-replace-user-agent-region
             (rail-user-agent-search-header item)))
          rail-user-agent-header-item-list))))

;; replace statically
(defun rail-user-agent-replace-string ()
  "Replace mime-edit-user-agent-value and mime-edit-mime-version-value."
  (and rail-user-agent-convert-statically
       (let (buf codename)
         (save-excursion
           (setq buf (get-buffer-create rail-temporary-buffer-name))
           (if (set-buffer buf)
               (progn
                 ;; mime-edit-mime-version-value
                 (erase-buffer)
                 (insert mime-edit-mime-version-value)
                 (setq mime-edit-mime-version-value
                       (rail-user-agent-replace-mime-version-region
                        (cons (point-min) (point-max))))
                 ;; mime-edit-user-agent-value
                 (erase-buffer)
                 (insert mime-edit-user-agent-value)
                 (setq mime-edit-user-agent-value
                       (rail-user-agent-replace-user-agent-region
                        (cons (point-min) (point-max))))
                 ;; end
                 (kill-buffer buf))))))
  ;; mime-library-product
  (and rail-user-agent-convert-statically
       rail-user-agent-replace-mime-library-product
       (boundp 'mime-library-product)
       (let ((codename (mime-product-code-name mime-library-product)))
         (aset mime-library-product 2
               (or (rail-assoc
                    codename
                    (append rail-additional-flim-codename-alist rail-flim-codename-alist)
                    rail-convert-direction)
                   codename))))
  ;; mime-user-interface-product
  (and rail-user-agent-convert-statically
       rail-user-agent-replace-mime-user-interface-product
       (boundp 'mime-user-interface-product)
       (let ((codename (mime-product-code-name mime-user-interface-product)))
         (aset mime-user-interface-product 2
               (or (rail-assoc
                    codename
                    (append rail-additional-semi-codename-alist rail-semi-codename-alist)
                    rail-convert-direction)
                   codename)))))

;; add mime-edit-translate-buffer-hook
(defun rail-user-agent-add-hook ()
  "Add hook for rail-user-agent"
  (let ((list (memq 'mime-edit-translate-body mime-edit-translate-buffer-hook)))
    (or (memq 'rail-user-agent-translate-body mime-edit-translate-buffer-hook)
        (setcdr list (cons 'rail-user-agent-translate-body (cdr list))))))

;; add mime-edit-load-hook
(if (not (featurep 'mime-edit))
    (add-hook 'mime-edit-load-hook
              (lambda ()
                (rail-user-agent-add-hook)
                (rail-user-agent-replace-string)))
  (rail-user-agent-add-hook)
  (rail-user-agent-replace-string))

(provide 'rail-user-agent)

;;; rail-user-agent.el ends here
