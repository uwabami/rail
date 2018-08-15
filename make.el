(defvar rail-install-list
  '("rail-common.el"
    "rail-user-agent.el"
    "rail-mime-example-1-13.el"
    "rail-mime-example-1-14.el"
    "rail-mime-example.el"
    ("rail-mule.el" . (and (not (featurep 'xemacs)) (featurep 'mule)))
    ("rail-xmas.el" . (featurep 'xemacs))
    "rail-table-mule.el"
    "rail-table-meadow.el"
    "rail-table-xmas.el"
    "rail-table-utf2000.el"
    "rail-table-flim.el"
    "rail-table-semi.el"
    "rail-pj.el"
    "rail-vars.el"
    "rail.el"))

;;################

(or (fboundp 'make-directory-internal)
    (defsubst make-directory-internal (dir)
      (call-process "mkdir" nil nil nil dir)))

(defun make-directory-one (dir)
  (or (and (file-exists-p dir)
	   (not (file-directory-p dir))
	   (error "Cannot make site-lisp directory: file \"%s\" exists." dir))
      (file-exists-p dir)
      (make-directory-internal dir)))

(defun get-bindir ()
  (message
   (expand-file-name
    (concat data-directory
            (cond ((featurep 'xemacs)
                   "../../../")
                  ((or (featurep 'meadow) (eq 'windows-nt system-type))
                   "../")
                  (t
                   "../../../../"))
            "bin/"))))

(defun get-site-lisp ()
  (message
   (expand-file-name
    (concat data-directory
            (cond ((featurep 'xemacs)
                   "../../xemacs/")
                  ((or (featurep 'meadow) (eq 'windows-nt system-type))
                   "../")
                  (t
                   "../../"))
            "site-lisp/"))))

(defun get-package-base ()
  (if (boundp 'early-packages)
      (let ((dirs (append (if early-package-load-path
                              early-packages)
                          (if late-package-load-path
                              late-packages)
                          (if last-package-load-path
                              last-packages)))
            dir)
        (while (not (file-exists-p
                     (setq dir (car dirs))))
          (setq dirs (cdr dirs)))
        (message dir))))

;;################

(defun get-filename (file)
  (or (and (stringp file)
	   (file-exists-p file)
	   file)
      (and (consp file)
	   (eval (cdr file))
	   (stringp (car file))
	   (car file))))

(defun compile-lisp (list)
  (mapcar
   (lambda (cell)
     (let (file compiled)
       (and (setq file (get-filename cell))
            (setq compiled (concat file "c"))
            (file-newer-than-file-p file compiled)
            (byte-compile-file file))))
   list))

(defun install-lisp (src dst)
  (message "Install *.el files...")
  (mapcar
   (lambda (list)
     (let ((file (get-filename list)))
       (and (stringp file)
            (message "%s -> %s" file dst)
            (copy-file file (concat dst (file-name-nondirectory file)) t t))))
   rail-install-list)
  (message "done.")
  (message "Install *.elc files...")
  (mapcar
   (lambda (file)
     (cond ((string-match ".*\\.elc$" file)
            (message "%s -> %s" file dst)
            (copy-file file (concat dst (file-name-nondirectory file)) t t)
            (delete-file file))))
   (directory-files src))
  (message "done."))

(defsubst install-current-lisp (dir)
  (install-lisp (expand-file-name "./") dir))

;;################

(defun compile-rail ()
  (and (catch 'found
         (mapcar
          (lambda (item)
            (if (eq 'mime-def item)
                (throw 'found t)))
          features))
       (require 'mime-def))
  (let ((load-path (cons (expand-file-name ".") load-path)))
    (compile-lisp rail-install-list)))

(defun install-rail ()
  (compile-rail)
  (let* ((dir (or (car command-line-args-left) "default"))
         (path (cond ((string= "default" dir)
                      (concat (get-site-lisp) "rail/"))
                     ((string= "site-lisp" dir)
                      (get-site-lisp))
                     ((string-match "/$" dir)
                      dir)
                     (t
                      (concat dir "/")))))
    (make-directory-one (substring path 0 (1- (length path))))
    (install-current-lisp path)))

(defun install-package ()
  (compile-rail)
  (let* ((dir (or (car command-line-args-left) "default"))
         (path (cond ((string= "default" dir)
                      (get-package-base))
                     ((string-match "/$" dir)
                      dir)
                     (t
                      (concat dir "/"))))
         (lispbase (concat path     "lisp/"))
         (lispdir  (concat lispbase "rail/"))
         (etcbase  (concat path     "etc/"))
         (etcdir   (concat etcbase  "rail/"))
         (infodir  (concat path     "pkginfo/")))
    (make-directory-one path)
    (make-directory-one lispbase)
    (make-directory-one lispdir)
    (make-directory-one etcbase)
    (make-directory-one etcdir)
    (make-directory-one infodir)
    (copy-file "MANIFEST.rail" (concat infodir "MANIFEST.rail") t t)
    (copy-file "00README" (concat etcdir "00README") t t)
    (install-current-lisp lispdir)))
