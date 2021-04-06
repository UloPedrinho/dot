;; Error retrieving: PACKAGE_XXXXX "incomprehensible buffer"
;;
(load-library "url-handlers")

(setq user-emacs-directory "~/.emacs.d/")

;; first time install
(load (concat user-emacs-directory "includes/pedro-install.el"))

;; package.el
(require 'package)
(setq package-user-dir (concat user-emacs-directory "var/lisp"))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ;;("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))


;; FIXME https://debbugs.gnu.org/34341. It should be fixed in Emacs 26.3+
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; (when (< emacs-major-version 27)
;;   (package-initialize))
(package-initialize)

;; use-package.el
(add-to-list 'load-path (concat user-emacs-directory "var/lisp/use-package"))
(require 'use-package)

(setq use-package-verbose t)

;; benchmark statrup
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))


;; set frame title
(setq frame-title-format `("emacs@" ,(system-name)))

;; pedro code
(load (concat user-emacs-directory "includes/pedro.el"))

;; os specifics
(load (concat user-emacs-directory "includes/pedro-os-specific.el"))

;; builtin packages
(load (concat user-emacs-directory "includes/pedro-builtin.el"))
(load (concat user-emacs-directory "includes/pedro-eshell.el"))

;; external packages
(load (concat user-emacs-directory "includes/pedro-external.el"))

;; external optional packages
(load (concat user-emacs-directory "includes/pedro-external-optional.el"))

;; personal
(when (file-exists-p (concat user-emacs-directory "includes/pedro-personal.el"))
  (load (concat user-emacs-directory "includes/pedro-personal.el")))

(setq custom-file (concat user-emacs-directory "includes/pedro-custom.el"))
(load custom-file 'noerror)


;; start server
;;(server-start)
