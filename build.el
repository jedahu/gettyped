(setq debug-on-error t)
(setq indent-tabs-mode nil)
(setq org-src-preserve-indentation t)

(defvar gettyped--verbose nil)

(defvar gettyped--initialized nil)

(defvar gettyped--packages
  '(org-plus-contrib))

(defvar gettyped--orig-message
  (symbol-function 'message))

(defun gettyped--info (format &rest args)
  (when gettyped--verbose
    (apply gettyped--orig-message (concat "\nGETTYPED: " format) args)))

(defun gettyped--noninteractive-init ()
  (unless gettyped--initialized
    (gettyped--info "init")
    (require 'package)
    (require 'cl)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
    (setq package-user-dir (concat default-directory "/.elisp"))
    (package-initialize)
    (let* ((package--builtins '())
           (pkgs (remove-if #'package-installed-p
                            gettyped--packages)))
      (when pkgs
        (gettyped--info "install packages")
        (package-refresh-contents)
        (dolist (pkg pkgs)
          (package-install pkg)))))
  (setq gettyped--initialized t))

(defun gettyped-build-html ()
  (gettyped--noninteractive-init)
  (gettyped--info "HTML")
  (require 'org)
  (require 'ox-org)
  (with-current-buffer (find-file "index.org")
    (let ((content (org-export-as 'org)))
      (make-directory "tmp" t)
      (with-current-buffer (find-file "tmp/index.expanded.org")
        (delete-region (point-min) (point-max))
        (insert content)
        (save-buffer)))))

(defun gettyped-tangle-src ()
  (gettyped--noninteractive-init)
  (gettyped--info "TANGLE")
  (require 'org)
  (require 'ob-tangle)
  (with-current-buffer (find-file "index.org")
    (org-babel-tangle)))

(defun gettyped-html-and-tangle ()
  (gettyped-build-html)
  (gettyped-tangle-src))
