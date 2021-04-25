;;; external packages

(use-package cl-lib
	     :ensure t)

(use-package undo-tree
	     :config
	     (setq undo-tree-visualizer-timestamps t)
	     (setq undo-tree-visualizer-diff t)
	     (global-undo-tree-mode)
	     ;; :diminish undo-tree-mode
	     :ensure t)

;; dired+
;; ;; install if not installed
;; ;; [[https://www.emacswiki.org/emacs/DiredPlus]]
(when (not (file-exists-p (concat user-emacs-directory "var/lisp/dired+")))
  (make-directory (concat user-emacs-directory "var/lisp/dired+"))
  (pedro/download-and-save-files-to-dir '("https://www.emacswiki.org/emacs/download/dired%2b.el")
                                        (concat user-emacs-directory "var/lisp/dired+")))
(add-to-list 'load-path (concat user-emacs-directory "var/lisp/dired+"))
(require 'dired+)


(use-package dired-quick-sort
             :ensure t
             :config
             (dired-quick-sort-setup))

(use-package dired-subtree
             :ensure t)

(use-package dired-rainbow
  :ensure t
  :config
  (progn
    (dired-rainbow-define image (:inherit 'bmkp-non-file) ("jpg" "jpeg" "png" "gif" "xpm" "svg" "xcf" "kra" "tiff" "tif" "psd" "eps"))
    (dired-rainbow-define doc (:inherit 'completions-annotations) ("pdf" "epub" "mobi" "doc" "docx"))
    (dired-rainbow-define audio (:inherit 'bmkp-no-jump) ("ogg" "wav" "mp3" "m4a" "flac" "api" "mid" "3gp"))
    (dired-rainbow-define video (:inherit 'bmkp-man) ("mkv" "avi" "mpeg" "mpg" "webm" "flv" "mp4"))
    (dired-rainbow-define compress (:foreground "#ff6c6b" :weight bold) ("zip" "rar" "bz2" "tar" "gz"))
    (dired-rainbow-define-chmod executable-unix "#00ff00" "-.*x.*")
    (dired-rainbow-define-chmod symlink-unix "#1f5582" "l...*")
    (dired-rainbow-define-chmod directory (:foreground "#61afef" :weight bold) "d...*"))
  (setq font-lock-maximum-decoration '((dired-mode . nil) (t . t))))

;; (use-package dired-filter
;;              :ensure t
;;              :config
;;              (setq dired-filter-group-saved-groups '(("default"
;;                                                       ("PDF"
;;                                                     (extension . "pdf"))
;;                                                       ("LaTeX"
;;                                                        (extension "tex" "bib"))
;;                                                       ("Org"
;;                                                        (extension . "org"))
;;                                                       ("Archives"
;;                                                        (extension "zip" "rar" "gz" "bz2" "tar"))))))

;; (use-package dired-k
;;              :config
;;              (setq dired-k-style 'git)
;;              (add-hook 'dired-initial-position-hook 'dired-k)
;;              (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
;;              :ensure t)

(use-package lilypond-mode
  :defer t
  :load-path "var/lisp/lilypond-mode"
  :config
  (setq auto-mode-alist
        (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

  (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
  (setq LilyPond-command-alist '(("LilyPond" "lilypond %s" "%s" "%l" "View")
                                 ("2PS" "lilypond -f ps %s" "%s" "%p" "ViewPS")
                                 ("Book" "lilypond-book %x" "%x" "%l" "LaTeX")
                                 ("LaTeX" "latex '\\nonstopmode\\input %l'" "%l" "%d" "ViewDVI")
                                 ("View" "open %f")
                                 ("ViewPDF" "open %f")
                                 ("ViewPS" "gv --watch %p")
                                 ("Midi" "")
                                 ("MidiAll" "")))
             ;; add info files
             (push "~/tmp/info/lilypond/" Info-directory-list)

             (defun pedro/lilypond-view-with-pdftools ()
               "view pdf sheet inside emacs with pdftools"
               (interactive)
               (let* ((name (file-name-sans-extension (buffer-name)))
                      (file (concat name ".pdf"))
                      (buffer (get-buffer (concat name ".ly")))
                      (revert-without-query (list file)))
                 (cond
                   (current-prefix-arg (LilyPond-command-view))
                   ((get-buffer file) (switch-to-buffer (get-buffer file)))
                   ((file-exists-p file) (find-file file))
                   ((message "File '%s' not exist" file)))))

             ;; redefine `LilyPond-info' to use lilypond-notation as index file
             (defun LilyPond-info ()
               "Launch Info for lilypond."
               (interactive)
               (info "lilypond-notation")))

(use-package  flycheck-lilypond
  :defer t
              :ensure t
              :config
              (add-hook 'LilyPond-mode-hook 'flycheck-mode)(and )
              (eval-after-load 'flycheck '(require 'flycheck-lilypond)))

;; `bookmak+'
;; install if not installed
;; [[https://www.emacswiki.org/emacs/BookmarkPlus]]
(when (not (file-exists-p (concat user-emacs-directory "var/lisp/bookmark+")))
  (make-directory (concat user-emacs-directory "var/lisp/bookmark+"))
  (pedro/download-and-save-files-to-dir
   '("https://www.emacswiki.org/emacs/download/bookmark%2b.el"
     "https://www.emacswiki.org/emacs/download/bookmark%2b-mac.el"
     "https://www.emacswiki.org/emacs/download/bookmark%2b-bmu.el"
     "https://www.emacswiki.org/emacs/download/bookmark%2b-1.el"
     "https://www.emacswiki.org/emacs/download/bookmark%2b-key.el"
     "https://www.emacswiki.org/emacs/download/bookmark%2b-lit.el"
     "https://www.emacswiki.org/emacs/download/bookmark%2b-doc.el"
     "https://www.emacswiki.org/emacs/download/bookmark%2b-chg.el")
   (concat user-emacs-directory "var/lisp/bookmark+")))
(add-to-list 'load-path (concat user-emacs-directory "var/lisp/bookmark+"))

;; init
(require 'bookmark+)
;; config
(setq bookmark-default-file (concat user-emacs-directory "var/bookmarks/main.bmk") ;; # TODO
      bmkp-bmenu-state-file (concat user-emacs-directory "var/bookmarks/emacs-bmk-state-file.el")
      bmkp-last-bookmark-file (concat user-emacs-directory "var/bookmarks/main.bmk")
      bmkp-current-bookmark-file (concat user-emacs-directory "var/bookmarks/main.bmk"))
(setq bookmark-save-flag 1)
(setq bookmark-version-control t)
(setq bmkp-bmenu-image-bookmark-icon-file nil)

(substitute-key-definition 'bmkp-bmenu-quit
                           'pedro-bmkp-bmenu-quit
                           bookmark-bmenu-mode-map)
(defvar pedro-bmkp-current-bookmark nil)

(defun pedro-bmkp-bmenu-quit ()
  (interactive)
  (bmkp-bmenu-quit)
  (when pedro-bmkp-current-bookmark
    (bmkp-switch-bookmark-file pedro-bmkp-current-bookmark))
  (setq pedro-bmkp-current-bookmark nil))

(defun pedro-bmkp-main ()
  (interactive)
  (setq pedro-bmkp-current-bookmark bmkp-current-bookmark-file)
  (bmkp-switch-bookmark-file
   (concat user-emacs-directory "var/bookmarks/main.bmk"))
  (bookmark-bmenu-list)
  (switch-to-buffer "*Bookmark List*"))

(use-package avy
  :defer t
	     :config
	     (global-set-key (kbd "C-c SPC") 'avy-goto-char-timer)
	     (add-hook 'org-mode-hook
		       (lambda ()
			 (local-set-key (kbd "\C-c SPC") 'avy-goto-char-timer)))
	     :ensure t)

(use-package yasnippet
	     :config
	     (setq yas-snippet-dirs '("~/.emacs.d/includes/pedro-snippets" yasnippet-snippets-dir))
	     (yas-global-mode 1)
	     :diminish yas-minor-mode
             :ensure yasnippet-snippets
	     :ensure t)

(use-package helm
	     :config
	     (require 'helm)
	     (require 'helm-config)
	     (helm-mode t)
	     (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
	     (global-set-key (kbd "M-x") 'helm-M-x)
	     (global-set-key (kbd "C-x C-f") 'helm-find-files)
	     ;; (global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
	     (global-set-key (kbd "C-x b") 'helm-buffers-list)

	     (setq helm-M-x-fuzzy-match nil
		   helm-M-x-always-save-history t
		   helm-quick-update t
		   helm-ff-skip-boring-files t)

	     (add-hook 'eshell-mode-hook
		       #'(lambda ()
			   (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))
	     :diminish helm-mode
	     :ensure t)

(use-package helm-flx
  :ensure t
  :config
  (helm-flx-mode +1))

;; (use-package helm-dash
;; 	     :config
;; 	     (setq helm-dash-browser-func 'eww
;; 		   helm-dash-common-docsets '("JavaScript"))
;; 	     :ensure t)

(use-package session
             :ensure t
	     :config
	     (add-hook 'after-init-hook 'session-initialize)
	     (setq session-save-file (concat user-emacs-directory "var/session")))

(use-package volatile-highlights
	     :config
	     (volatile-highlights-mode t)
	     :diminish volatile-highlights-mode
	     :ensure t)

;; (use-package diff-hl                    ;TODO,, to be removed. 'git-gutter' to be installed
;; 	     :config
;;              (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;; 	     (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
;; 	     (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;              (add-hook 'LilyPond-mode-hook 'turn-on-diff-hl-mode)
;; 	     :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(use-package rainbow-delimiters
	     :diminish rainbow-mode
	     :config
	     (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
	     :ensure t)


;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
;;   (setq evil-want-keybinding t) ;; nil)
;;   :config
;;   (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))


(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  ;; keybindings
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "M-(") 'sp-wrap-round)
  ;; (define-key smartparens-mode-map (kbd "C-(") 'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)
  :diminish smartparens-mode
  :ensure t)


(use-package magit
  ;; :defer t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  ;; set emacsclient executable path
  ;; (set-variable 'magit-emacsclient-executable "/usr/local/Cellar/emacs/HEAD-da18508/bin/emacsclient")
  :ensure t)

(use-package git-timemachine
  :defer t
	     :ensure t)

;; (use-package sr-speedbar
;; 	     :ensure t)

(use-package expand-region
  :defer t
	     :ensure t)

(use-package helm-swoop
	     :config
	     (setq helm-swoop-pre-input-function
		   (lambda () ""))
	     ;; (global-set-key "\C-s" 'helm-swoop)
	     :ensure t)

;; (use-package flycheck
;; 	     ;; css(html-tidy) :: apt-get install tidy
;;              :config
;;              (setq-default flycheck-emacs-lisp-load-path 'inherit)
;; 	     :ensure t)

(use-package window-numbering
	     :config
	     (setq window-numbering-assign-func
		   (lambda () (when (equal (buffer-name) "*Calculator*") 9)))
	     (window-numbering-mode 1)
	     :ensure t)

(use-package ace-window
             :bind (("C-M-o" . ace-window))
             :ensure t)

;; TODO: useful?
(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))


(use-package transpose-frame
	     :ensure t)

(use-package projectile
  :defer t
  :ensure t
  :config
  (require 'projectile)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; 	     (setq projectile-cache-file (concat user-emacs-directory "var/projectile/projectile.cache")
;; 		   projectile-known-projects-file (concat user-emacs-directory "var/projectile/projectile-bookmarks.eld"))

;; 	     (setq projectile-buffers-filter-function 'pedro/projectile-filter-buffers
;; 		   pedro/projectile-filter-modes '("ibuffer-mode" "magit-log-mode" "magit-status-mode"
;; 						   "magit-process-mode" "eshell-mode")
;; 		   pedro/projectile-filter-buffers '("Flycheck" "dir-locals.el" "tern-project"))

;; 	     (defun pedro/projectile-filter-buffers (buffers)
;; 	       (cl-remove-if (lambda (b)
;; 			       (or (member (symbol-name (with-current-buffer (buffer-name b) major-mode))
;; 					   pedro/projectile-filter-modes)
;; 				   (cl-find-if (lambda (r)
;; 						 (string-match r (buffer-name b)))
;; 					       pedro/projectile-filter-buffers)))
;; 			     buffers))

;; 	     :ensure t)

(use-package helm-projectile
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  :ensure t)


(use-package discover-my-major
  :config
  (global-unset-key (kbd "C-h h")) ; original "C-h h" displays "hello world" in different languages
  (define-key 'help-command (kbd "h m") 'discover-my-major)
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :bind (("M-/" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ;; ("<backtab>" . my-company-yasnippet)
         ;; ("C-c C-y" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :config
  (setq company-idle-delay 0.01  ;; 0.3
	company-show-numbers t
	company-tooltip-limit 10
	company-tooltip-align-annotations t
	company-minimum-prefix-length 2
	company-selection-wrap-around t
	company-selection-changed t
	company-tooltip-flip-when-above nil
	company-require-match nil
	company-quickhelp-max-lines 60
        company-dabbrev-downcase nil ;; case-replace
        company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        company-dabbrev-code-everywhere t
        company-dabbrev-other-buffers nil
	pos-tip-border-width 0)
  (setq company-backends
        (mapcar
         (lambda (backend)
           (if (and (listp backend) (member 'company-yasnippet backend))
               backend
             (append (if (consp backend) backend (list backend))
                     '(:with company-yasnippet))))
         company-backends))

  ;; :bind (:map company-active-map
  ;;             ("C-n" . company-select-next)
  ;;             ("C-p" . company-select-previous)
  ;;             ("C-d" . company-show-doc-buffer)
  ;;             ("M-." . company-show-location))

  ;; (define-key company-active-map (kbd "\C-n") 'company-select-next)
  ;; (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  ;; (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)  ;; (define-key company-active-map (kbd "M-.") 'company-show-location)

  (global-company-mode 1))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

(use-package org
  :ensure t
  :config
  (load-file (concat user-emacs-directory "includes/org-mode/pedro-org-mode.el"))
  ;; https://github.com/jwiegley/use-package/issues/319
  :ensure org-plus-contrib
  :pin org)

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package adaptive-wrap
  :ensure t
  :config
  (add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode))

(use-package org-pdfview
	     :config
	     (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
	     :ensure t)

(use-package eww-lnum
  :defer t
	     :ensure t)

(use-package lorem-ipsum
  :defer t
	     :ensure t)

(use-package which-key
	     :config
	     (setq which-key-sort-order 'which-key-key-order-alpha
                   which-key-side-window-max-height 10)
	     (which-key-mode)
	     (which-key-setup-side-window-right-bottom)
	     :diminish which-key-mode
	     :ensure t)

(use-package imenu-list
	     :ensure t)

(use-package hl-todo                    ;TODO: faces and new tags
  :ensure t
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#cc9393")
          ("NEXT" . "#dca3a3")
          ("NOTE"   . "#d0bf8f")
          ("TAG" ."#8a2be2")
          ("TEMP"   . "#d0bf8f")
          ("FIXME"  . "#cc9393")
          ("BENCHMARK" . "#aaefbf")))
  (add-hook 'prog-mode-hook #'hl-todo-mode))


;; folding
(use-package hideshow-org
             :ensure t)

(use-package origami
  :defer t
	     :config
	     (global-origami-mode)
	     :ensure t)

(use-package persistent-scratch
	     :config
	     (setq persistent-scratch-save-file (concat user-emacs-directory "var/persistent-scratch/persistent-scratch.new")
		   persistent-scratch-backup-directory (concat user-emacs-directory "var/persistent-scratch/")
		   persistent-scratch-backup-file-name-format "persistent-scratch.%Y%m%d%H%M%S.%N"
		   persistent-scratch-timer (run-with-timer 0 (* 8 60 60) #'(lambda ()
									      (persistent-scratch-new-backup)
									      (persistent-scratch-save))))
	     (persistent-scratch-setup-default)
	     :ensure t)


(use-package outshine
	     :config
	     (require 'outshine)
	     (add-hook 'outline-minor-mode-hook 'outshine-mode)
	     (add-hook 'prog-mode-hook 'outline-minor-mode)
	     (add-hook 'lisp-interaction-mode-hook 'outline-minor-mode)
	     :diminish outline-minor-mode
	     :ensure t)

(use-package navi-mode
	     :ensure t)

(use-package highlight-defined
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
  :ensure t)

(use-package elisp-slime-nav
  :defer t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  :ensure t)

(use-package eval-in-repl
	     :config
	     (require 'eval-in-repl-ielm)
	     ;; for .el files
	     (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
	     ;; for *scratch*
	     (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
	     ;; for M-x info
	     (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
	     :ensure t)

;; (use-package hlinum
;; 	     :ensure t)

(use-package zoom 			; golden-ratio sucesor
	     :config
	     (zoom-mode 1)
	     :diminish zoom-mode
	     :ensure t)

(use-package restart-emacs
	     :ensure t)

(use-package zoom-window
	     :config
	     (setq zoom-window-mode-line-color  "#2f7f4f")
	     :ensure t)

(use-package iedit
	     :ensure t)

(use-package shrink-whitespace
	     :ensure t)

(use-package visual-regexp
	     :ensure t)

(use-package comment-dwim-2
	     :ensure t)

(use-package poporg
             :ensure t)

(use-package keyfreq
	     :config
	     (keyfreq-mode 1)
	     (keyfreq-autosave-mode 1)
             (setq keyfreq-excluded-commands
                   '(self-insert-command
                     forward-char
                     backward-char
                     previous-line
                     next-line))
	     :ensure t)

(use-package expand-region
	     :ensure t)

(use-package exec-path-from-shell
	     :config
	     (exec-path-from-shell-initialize)
	     :ensure t)

(use-package eshell-z
  :ensure t
  :config
  (add-hook 'eshell-mode-hook
            (defun my-eshell-mode-hook ()
              (require 'eshell-z)))
  )

;;; web
(use-package rainbow-mode
	     :config
	     (add-hook 'prog-mode 'rainbow-mode)
	     :ensure t)


;; (use-package js2-mode
;; 	     :config
;; 	     (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; 	     (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
;; 	     ;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; 	     ;;(add-hook 'js2-mode-hook 'ac-js2-mode)
;; 	     (add-hook 'js2-mode-hook 'flycheck-mode)
;; 	     (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
;; 	     (setq js2-highlight-level 3
;; 		   js-indent-level 2
;;                    js2-basic-offset 2)
;; 	     :ensure t)

;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
;; (use-package js2-refactor
;; 	     :config
;; 	     (js2r-add-keybindings-with-prefix "C-c C-l")
;; 	     :ensure t)

;; (use-package xref-js2                   ; Navigate JS with ag & js2-mode's AST
;;              :ensure t
;;              :after js2-mode
;;              :init
;; 	     (defun add-xref-js2-backend ()
;; 	       (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
;; 	     (add-hook 'js2-mode-hook #'add-xref-js2-backend)
;; 	     (define-key js2-mode-map (kbd "M-.") nil))

;; (use-package js-doc
;;              :ensure t
;;              :config
;;              (setq js-doc-mail-address "your email address"
;;                    js-doc-author (format "your name <%s>" js-doc-mail-address)
;;                    js-doc-url "url of your website"
;;                    js-doc-license "license name")

;;              (add-hook 'js2-mode-hook
;;                        #'(lambda ()
;;                            (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
;;                            (define-key js2-mode-map "@" 'js-doc-insert-tag))))

;; (use-package helm-xref
;; 	     :config
;; 	     (setq xref-show-xrefs-function 'helm-xref-show-xrefs)
;; 	     :ensure t)

;; (use-package tern
;; 	     :config
;; 	     (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;; 	     ;; unbind keys to let xref-js2 use them
;; 	     (unbind-key "M-." tern-mode-keymap)
;; 	     (unbind-key "M-," tern-mode-keymap)
;; 	     :ensure t)

;; (use-package company-tern
;; 	     :config
;; 	     (add-to-list 'company-backends 'company-tern)
;; 	     :ensure t)

;; (use-package tide
;;              :ensure t
;;              :config
;;              ;; https://github.com/ananthakumaran/tide
;;              (defun setup-tide-mode ()
;;                (interactive)
;;                (tide-setup)
;;                (flycheck-mode +1)
;;                (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;                (eldoc-mode +1)
;;                (tide-hl-identifier-mode +1)
;;                ;; company is an optional dependency. You have to
;;                ;; install it separately via package-install
;;                (company-mode +1))
;;              (add-hook 'js2-mode-hook #'setup-tide-mode)
;;              ;; configure javascript-tide checker to run after your default javascript checker
;;              (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))


;; (use-package moz
;; 	     :ensure t)

;; TODO indium package
;; https://github.com/NicolasPetton/Indium

;; TODO https://github.com/yasuyk/web-beautify

;; (use-package impatient-mode
;; 	     :ensure t)

;; TODO https://github.com/pandeiro/livid-mode ;; test if is better than impatient-mode

;; (use-package skewer-mode
;; 	     :config
;; 	     ;; (add-hook 'js2-mode-hook 'skewer-mode)
;; 	     ;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; 	     ;; (add-hook 'html-mode-hook 'skewer-html-mode)
;; 	     ;; (add-hook 'web-mode-hook 'skewer-html-mode)
;; 	     :ensure t)


;; (use-package web-beautify
;;              :ensure t)

;; (use-package simple-httpd
;; 	     :config
;; 	     (setq httpd-root "~/tmp/tmp/deleteme/js/")
;; 	     :ensure t)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; (use-package helm-css-scss
;; 	     :config
;; 	     ;; Allow comment inserting depth at each end of a brace
;; 	     (setq helm-css-scss-insert-close-comment-depth 2)
;; 	     ;; If this value is t, split window appears inside the current window
;; 	     (setq helm-css-scss-split-with-multiple-windows nil)
;; 	     ;; Split direction. 'split-window-vertically or 'split-window-horizontally
;; 	     (setq helm-css-scss-split-direction 'split-window-vertically)

;; 	     ;; Set local keybind map for css-mode / scss-mode / less-css-mode
;; 	     (dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
;; 	       (add-hook
;; 		$hook (lambda ()
;; 			(local-set-key (kbd "s-i") 'helm-css-scss)
;; 			(local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))

;; 	     (define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
;; 	     (define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)
;; 	     :ensure t)

;; (use-package css-eldoc
;;              :config
;;              (add-hook 'css-mode-hook
;;                        #'turn-on-css-eldoc)
;;              :ensure t)

;; (use-package emmet-mode
;; 	     :config
;; 	     (setq emmet-move-cursor-between-quotes t)
;; 	     (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;; 	     (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;; 	     (add-hook 'web-mode-hook 'emmet-mode)
;; 	     :ensure t)

;; (use-package json-mode
;; 	     :ensure t)

;; (use-package nodejs-repl
;; 	     :config
;; 	     (defun pedro/nodejs-send-function ()
;; 	       "docstring"
;; 	       (interactive)
;; 	       ;; (js2-mark-defun)
;; 	       (mark-defun)
;; 	       (nodejs-repl-send-region (region-beginning) (region-end)))
;; 	     :ensure t)

(use-package ztree
	     :ensure t)

(use-package visual-fill-column
	     :ensure t)

;; (use-package dumb-jump
;; 	     :bind (("M-g o" . dumb-jump-go-other-window)
;; 		    ("M-g j" . dumb-jump-go)
;; 		    ("M-g i" . dumb-jump-go-prompt)
;; 		    ("M-g x" . dumb-jump-go-prefer-external)
;; 		    ("M-g z" . dumb-jump-go-prefer-external-other-window)
;;                     ("M-g b" . dumb-jump-back))
;; 	     :config
;;              (setq dumb-jump-selector 'helm) ;; (setq dumb-jump-selector 'ivy)
;;              (setq dumb-jump-prefer-searcher 'ag)
;; 	     :ensure t)


;; troubleshooting with bookmarks , "invalid bookmark file" error , end of file "^)"
(use-package aggressive-indent
	     :config
	     (add-hook 'prog-mode-hook 'aggressive-indent-mode)
	     :ensure t)

(use-package lisp-extra-font-lock
  :ensure t
  :config
  (lisp-extra-font-lock-global-mode 1))

(use-package markdown-mode
  :defer t
             :ensure t
             :commands (markdown-mode gfm-mode)
             :mode (("README\\.md\\'" . gfm-mode)
                    ("\\.md\\'" . markdown-mode)
                    ("\\.markdown\\'" . markdown-mode))
             :init (setq markdown-command "multimarkdown"))

;;; Themes
(use-package zerodark-theme
  :ensure t
  :config
  (load "zerodark-theme" nil t)
  (load-theme 'zerodark t)
  ;;(zerodark-setup-modeline-format t)
  )

;;; keys
(use-package key-chord
	     :config
	     (key-chord-mode 1)
	     :ensure t)

(use-package general
	     ;; it needs 'key-chord' package to key-chords.
	     :config
	     (general-define-key
	      (general-chord "kj") 'info
	      ;; ?
              ;;(general-chord "ff") 'evil-normal-state
	      ;; (general-chord "qw") '(lambda () (interactive) (load-theme 'solarized-dark t))
	      ;; (general-chord "wq") '(lambda () (interactive) (disable-theme 'solarized-dark))
	      ;;
	      )
	     :ensure t)

(use-package deft
  :ensure t
  :config
  (setq deft-directory "~/org/deft/default"
        ;;deft-default-extension "org"
        deft-default-extension "org"
        deft-extensions '("org")
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t)
  ;; FIXME: works fine (?)
  (add-hook 'after-change-major-mode-hook 'hack-local-variables))

(use-package hydra
             :ensure t
             :config
             (setq lv-use-separator t)
             (load (concat user-emacs-directory "includes/pedro-hydras.el")))

;; (use-package goto-chg
;;   :config
;;   (global-set-key [(control ?.)] 'goto-last-change)
;;   (global-set-key [(control ?,)] 'goto-last-change-reverse)
;; )

(use-package multi-term
  :defer t
  :ensure t
  :config
  )

(use-package helm-ag
	     :ensure t
             :config
             (setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
             (setq dumb-jump-prefer-searcher 'ag))

;; (use-package yankpad
;;   :ensure t
;;   :config
;;   (setq yankpad-file "~/.emacs.d/var/yankpad.org")
;;   (yas-global-mode t)
;;   ;; (add-to-list 'company-backends #'company-yankpad)
;;   )

;; (use-package eyebrowse
;; 	     :config
;;              (add-to-list 'window-persistent-parameters '(window-side . writable))
;;              (add-to-list 'window-persistent-parameters '(window-slot . writable))
;;              (setq eyebrowse-mode-line-separator " ")
;; 	     (eyebrowse-mode t)
;; 	     :ensure t)

(use-package perspective
    :ensure t
    :config
    (setq persp-initial-frame-name "1")
    (set-face-attribute 'persp-selected-face nil  :foreground "#4876ff" :underline t)
    (persp-mode)
    (global-set-key (kbd "C-x C-b") 'persp-ibuffer)
    ;;(global-set-key (kbd "C-x b") 'persp-ivy-switch-buffer)
    ;; helm is advised so no need to set key
    )


(use-package buffer-move
  :ensure t)

(use-package bind-key
	     :config
	     (global-unset-key [(control z)]) ; disable ^Z

             ;; (bind-key "C-M-x" 'pedro/nodejs-send-function js2-mode-map)
             (bind-key "<tab>" 'dired-subtree-toggle dired-mode-map)

             (bind-keys*
	      ("M-m m" . er/expand-region)
	      ("M-m =" . indent-region)
	      ("M-m t i" . iedit-mode)
	      ("M-m t s" . shrink-whitespace)
	      ("M-m t v" . vr/query-replace)
	      ("M-m w" . delete-trailing-whitespace)

	      ;; browser
	      ("M-m e e" . eww)
	      ("M-m e b" . helm-eww-buffers)
	      ("M-m e u" . (lambda () (interactive) (browse-url (ffap-url-at-point))))

	      ;; dict
              ("M-m D" . hydra-dictionary/body)

              ;; deft
              ("M-m d" . deft)

              ;; outline
              ("M-m i" . hydra-outline/body)
              ;; folding
              ("M-m f" . hydra-folding/body)
              ;; go
              ("M-m g" . hydra-go/body)
              ;; emms
              ("M-m p" . hydra-emms/body)
              ;; org
              ("M-m o" . hydra-org/body)
              ;; persp
              ("M-m n" . hydra-persp/body)

              ;; sandboxes
              ("M-m s" . hydra-sandboxes/body)

              ;; buffers
              ("M-m b" . hydra-buffers/body)

              ;; chronos
              ("M-m c" . hydra-chronos/body)

              ;; various testing
              ("M-m v" . hydra-testing/body)

	      ;; others
	      ("M-m h h" . describe-personal-keybindings)
	      ;; ("M-m h t" . highlight-thing-mode)
	      ("M-m h p" . highlight-phrase)
	      ("M-m h r" . highlight-regexp)
              ;; ("M-m o i" . org-insert-link)
	      ("C-x C-z" . zoom-window-zoom)
	      ("C-h C-m" . discover-my-major)
	      ("C-h M-m" . discover-my-mode)
	      ("C-x 5 5" . (lambda () (interactive) (make-frame '((name . "doc") ; 2560x1440 monitor doc frame
								  (top . 0)
								  (left . 1280)
								  (width . (text-pixels . 1270))
								  (height . (text-pixels . 1440))))))
	      ;; rebind
	      ("M-;" . comment-dwim-2)
	      ("C-M-f" . sp-forward-sexp)
	      ("C-M-b" . sp-backward-sexp)
	      ("C-M-q" . unfill-region)
              ("C-M-h" . winner-undo)
              ("C-M-l" . winner-redo)

             ;; symbol-overlay
             ("M-i i" . symbol-overlay-put)
             ("M-i m" . symbol-overlay-mode)
             ("M-i r" . symbol-overlay-remove-all)
             ("M-i n" . symbol-overlay-switch-forward)
             ("M-i p" . symbol-overlay-switch-backward)

	     ;; perspective
	     ("C-0" . (lambda () (interactive) (persp-switch "0")))
	     ("C-1" . (lambda () (interactive) (persp-switch "1")))
	     ("C-2" . (lambda () (interactive) (persp-switch "2")))
	     ("C-3" . (lambda () (interactive) (persp-switch "3")))
	     ("C-4" . (lambda () (interactive) (persp-switch "4")))
	     ("C-5" . (lambda () (interactive) (persp-switch "5")))
	     ("C-6" . (lambda () (interactive) (persp-switch "6")))
	     ("C-7" . (lambda () (interactive) (persp-switch "7")))
	     ("C-8" . (lambda () (interactive) (persp-switch "8")))
	     ("C-9" . (lambda () (interactive) (persp-switch "9")))

             ;; eyebrowse
             ;; ("C-1" . eyebrowse-switch-to-window-config-1)
             ;; ("C-2" . eyebrowse-switch-to-window-config-2)
             ;; ("C-3" . eyebrowse-switch-to-window-config-3)
             ;; ("C-4" . eyebrowse-switch-to-window-config-4)
             ;; ("C-5" . eyebrowse-switch-to-window-config-5)
             ;; ("C-6" . eyebrowse-switch-to-window-config-6)
             ;; ("C-7" . eyebrowse-switch-to-window-config-7)
             ;; ("C-8" . eyebrowse-switch-to-window-config-8)
             ;; ("C-9" . eyebrowse-switch-to-window-config-9)
             ;; ("C-0" . eyebrowse-switch-to-window-config-0)
             ;; ("C-c C-w C-w" . eyebrowse-last-window-config)
             ;; ("C-c C-w C-h" . eyebrowse-prev-window-config)
             ;; ("C-c C-w C-l" . eyebrowse-next-window-config)
	     )


	     :ensure t)

;; TODO: old config. to be deleted !!!

;; yasnippet
;;(add-to-list 'company-backends 'company-yasnippet)
;; (add-hook 'js2-mode-hook '(lambda () (setq-local company-backends '((company-tern company-yasnippet)))))

;; (add-hook 'html-mode-hook '(lambda () (setq-local company-backends '((company-web-html company-yasnippet))))	     (add-hook 'css-mode-hook '(lambda () (setq-local company-backends '((company-css company-yasnippet)))))
;; (add-hook 'lisp-interaction-mode-hook '(lambda () (setq-local company-backends '((company-elisp company-capf company-yasnippet)))))
;; (add-hook 'php-mode-hook '(lambda () (setq-local company-backends '((company-ac-php-backend company-php)))))
;; (add-hook 'eshell-mode-hook '(lambda () (setq-local company-backends '((company-capf company-files))))))

;; (use-package bind-key
;; 	     :config
;; 	     (global-unset-key [(control z)]) ; disable ^Z

;;              ;; (bind-key "C-M-x" 'pedro/nodejs-send-function js2-mode-map)
;;              (bind-key "<tab>" 'dired-subtree-toggle dired-mode-map)

;;              (bind-keys*
;; 	      ("M-m m" . er/expand-region)
;; 	      ("M-m =" . indent-region)
;; 	      ("M-m t i" . iedit-mode)
;; 	      ("M-m t s" . shrink-whitespace)
;; 	      ("M-m t v" . vr/query-replace)
;; 	      ("M-m w" . delete-trailing-whitespace)

;; 	      ;; browser
;; 	      ("M-m e e" . eww)
;; 	      ("M-m e b" . eww-list-bookmarks)
;; 	      ("M-m e u" . (lambda () (interactive) (browse-url (ffap-url-at-point))))

;; 	      ;; dict
;;               ("M-m D" . hydra-dictionary/body)

;;               ;; deft
;;               ("M-m d" . deft)

;;               ;; outline
;;               ("M-m \@" . hydra-outline/body)
;;               ;; folding
;;               ("M-m f" . hydra-folding/body)
;;               ;; go
;;               ("M-m g" . hydra-go/body)
;;               ;; emms
;;               ("M-m p" . hydra-emms/body)
;;               ;; org
;;               ("M-m o" . hydra-org/body)
;;               ;; persp
;;               ("M-m n" . hydra-persp/body)

;;               ;; sandboxes
;;               ("M-m s" . hydra-sandboxes/body)

;;               ;; various testing
;;               ("M-m v" . hydra-testing/body)

;; 	      ;; others
;; 	      ("M-m h h" . describe-personal-keybindings)
;; 	      ;; ("M-m h t" . highlight-thing-mode)
;; 	      ("M-m h p" . highlight-phrase)
;; 	      ("M-m h r" . highlight-regexp)
;;               ;; ("M-m o i" . org-insert-link)
;; 	      ("C-x C-z" . zoom-window-zoom)
;; 	      ("C-h C-m" . discover-my-major)
;; 	      ("C-h M-m" . discover-my-mode)
;; 	      ("C-x 5 5" . (lambda () (interactive) (make-frame '((name . "doc") ; 2560x1440 monitor doc frame
;; 								  (top . 0)
;; 								  (left . 1280)
;; 								  (width . (text-pixels . 1270))
;; 								  (height . (text-pixels . 1440))))))
;; 	      ;; rebind
;; 	      ("M-;" . comment-dwim-2)
;; 	      ("C-M-f" . sp-forward-sexp)
;; 	      ("C-M-b" . sp-backward-sexp)
;; 	      ("C-M-q" . unfill-region)
;;               ("C-M-h" . winner-undo)
;;               ("C-M-l" . winner-redo)

;;              ;; symbol-overlay
;;              ("M-i i" . symbol-overlay-put)
;;              ("M-i m" . symbol-overlay-mode)
;;              ("M-i r" . symbol-overlay-remove-all)
;;              ("M-i n" . symbol-overlay-switch-forward)
;;              ("M-i p" . symbol-overlay-switch-backward))

;; 	     :ensure t)

;; testing packages
(use-package symbol-overlay
  :ensure t)

(use-package fzf
  :ensure t)
