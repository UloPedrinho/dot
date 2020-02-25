;; first time config install
;; . install 'use-package'
;; . create directory tree

(when (not (file-exists-p (concat user-emacs-directory ".install-done")))
  ;; install "use-package"
  (require 'package)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			   ;;("marmalade" . "http://marmalade-repo.org/packages/")
			   ("melpa" . "http://melpa.milkbox.net/packages/")
			   ("org" . "http://orgmode.org/elpa/")))
  (setq package-user-dir (concat user-emacs-directory "var/lisp"))
  (package-initialize t)
  (package-refresh-contents)
  (package-install 'use-package)
  ;; make directories
  (make-directory (concat user-emacs-directory "var/lisp") t)
  (make-directory (concat user-emacs-directory "var/eww"))
  (make-directory (concat user-emacs-directory "var/bookmarks"))
  (make-directory (concat user-emacs-directory "var/desktop"))
  (make-directory (concat user-emacs-directory "var/emms"))
  (make-directory (concat user-emacs-directory "var/projectile"))
  ;; wanderlust
  ;; (when (not (file-exists-p "~/.wl"))
  ;;   (make-symbolic-link (concat user-emacs-directory "includes/wanderlust/wl") "~/.wl"))
  ;; done, touch ".install-done" file
  (write-region "" "" (concat user-emacs-directory ".install-done")))
