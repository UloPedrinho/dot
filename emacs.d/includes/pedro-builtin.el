;; no splash screen
(setq inhibit-splash-screen t)

;; after make frame
(setq initial-buffer-choice 'pedro/after-make-frame)

;; no bell
(setq ring-bell-function 'ignore)

;; C-n add new line when is at bottom
(setq next-line-add-newlines t)

;; Closing
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (save-buffers-kill-emacs)
    (message "Canceled exit")))

;; coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
;;(set-w32-system-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; keys
(global-set-key (kbd "C-x C-c") 'ask-before-closing)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x o") 'other-frame)

; Auto-save
(setq auto-save-interval 30)
(setq auto-save-list-file-name (concat user-emacs-directory "var/auto-save-list/.saves-"))
(setq auto-save-list-file-prefix (concat user-emacs-directory "var/auto-save-list/.saves-"))

;; save-place
(setq save-place-file "~/.emacs.d/var/places"
      save-place-limit nil)
(save-place-mode t)

;; better performance
;; http://emacs.stackexchange.com/questions/19705/how-to-better-debug-emacs-cpu-management
(setq gc-cons-threshold 10000000)
(setq bidi-paragraph-direction 'left-to-right)

;; let use 'C-l', 'C-v'.. while isearch
(setq isearch-allow-scroll t)

;; time-stamp
(add-hook 'before-save-hook 'time-stamp)

;; hide bars
(menu-bar-mode 0)
(tool-bar-mode 0)

;; hide scrollbars
(scroll-bar-mode 0)

;; fringes
;; (set-fringe-mode 0)

;; Show cursor position
(setq column-number-mode t)
(setq line-number-mode t)

;; cursor blink
(setq blink-cursor-blinks 2)

;; Display date&time modeline
(setq display-time-day-and-date t
      display-time-24hr-format nil
      display-time-default-load-average nil)

(setq display-time-string-forms
      '((propertize (format-time-string "%w") 'face 'org-block)
	(propertize (format-time-string "%d%m") 'face 'font-lock-comment-delimiter-face)
	(propertize (format-time-string "%H%M") 'face 'mode-line-buffer-id))) ; 'font-lock-builtin-face)))

(display-time-mode 1)

;; Turn off auto-fill-mode globally
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; fill column indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; whitespaces
(require 'whitespace)
(add-hook 'prog-mode-hook '(lambda () (setq show-trailing-whitespace t)))

;; no tabs, only spaces
(set-variable 'indent-tabs-mode nil)    ;TODO: to be deleted
(setq-default indent-tabs-mode nil)

;; * show file size on modeline
(size-indication-mode t)


;; * iBuffer
(defalias 'list-buffers 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ibuffer)
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "\\*.*helm.*\\*")
(setq ibuffer-show-empty-filter-groups nil)

;; show human-size readable
;; https://www.emacswiki.org/emacs/IbufferMode

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("programming"
		(or
		 (mode . c-mode)
		 (mode . c++-mode)
		 (mode . sql-mode)
		 (mode . nxml-mode)
		 (mode . sh-mode)
                 (mode . conf-space-mode)
                 (mode . conf-unix-mode)
		 (name . "Makefile")))
	       ("                          lua"
		(or
		 (mode . lua-mode)
		 ))

	       ("                       python"
		(or
		 (mode . python-mode)
		 ))
	       ("                         java"
		(or
		 (mode . java-mode)
		 (mode . jde-mode)
		 ))
	       ("                          web"
		(or
		 (mode . html-mode)
		 (mode . js2-mode)
		 (mode . javascript-mode)
		 (mode . js-mode)
		 (mode . css-mode)
		 (mode . web-mode)
		 (mode . php-mode)
		 (mode . nxhtml-mode)
		 ))
	       ("                         lisp"
		(or
		 (mode . lisp-mode)
		 (mode . emacs-lisp-mode)
		 ))
               ("                     lylipond"
                (or
                 (mode . lilypond-mode)
                 ))
	       ("org" ;; all org-related buffers
		(or
		 (mode . org-mode)
                 (mode . org-journal-mode)
		 ))
               ("pdf/epub"
                (or
                 (mode . nov-mode )
		 (mode . pdf-view-mode)))
	       ("music"
		(or
		 (filename . "música")
		 (mode . emms-playlist-mode)
		 (mode . emms-browser-mode)
		 ))
	       ("dired"
		(or
		 (mode . dired-mode)))
	       ("www"
		(or
		 (name . "^\\*w3m" )
		 (mode . eww-mode)
		 ))
	       ("shell"
		(or
		 (name . "^\\*eshell")
		 (name . "^\\*terminal")
		 (name . "^\\*zsh")
		 (name . "^\\*ansi-term")
                 (name . "^\\*Shell*")
                 (mode . vterm-mode)
		 ))
	       ("irc"
                (or
		 (mode . erc-mode)
                 (mode . rcirc-mode)
                 (mode . circe-channel-mode)
                 (mode . circe-server-mode)
                 (mode . circe-query-mode)))
               ("magit"
                (or
                 (name . "^magit*")))
               ("info"
	        (or
	         (name . "^\\*Messages\\*$")
                 (name . "^\\*Warnings\\*$")
                 (name . "^\\*Compile*")
	         (mode . Info-mode)
	         (mode . help-mode)
                 (mode . helpful-mode)
                 (mode . Man-mode)))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))


(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; name column to 30 witdh
(setq ibuffer-formats
	'((mark modified read-only " "
		(name 30 30 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" "
		filename-and-process)))

;; * dired
(setq-default dired-listing-switches "-alh")
(setq image-dired-dir (concat user-emacs-directory "var/image-dired"))
(setq dired-guess-shell-alist-user '(("\\.mp[34]\\|\\.mkv\\|\\.avi\\|\\.m4a\\|\\.wav\\|\\.ogg\\|\\.webm\\|\\.mpeg" "nohup mpv --force-window=yes"))
      dired-guess-shell-case-fold-search t)
(setq dired-recursive-copies 'always)

;; from: https://www.emacswiki.org/emacs/download/recentf-ext.el
(defun recentf-add-dired-directory ()
  (when (and (stringp dired-directory)
             (equal "" (file-name-nondirectory dired-directory)))
    (recentf-add-file dired-directory)))
(add-hook 'dired-mode-hook 'recentf-add-dired-directory)

;;; * Abbrevs
(setq abbrev-file-name (concat user-emacs-directory "var/abbrev_defs"))

;;; * tramp
(setq tramp-persistency-file-name (concat user-emacs-directory "var/tramp"))

;;; * recentf
(require 'recentf)
(setq recentf-max-menu-items 100000)
(setq recentf-max-saved-items nil)
(setq recentf-auto-cleanup 'never)
(setq recentf-save-file (concat user-emacs-directory "var/recentf"))
;; (defun recentf-ido-find-file ()
;;   "Find a recent file using ido."
;;   (interactive)
;;   (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
;;     (when file
;;       (find-file file))))
;; (global-set-key (kbd "C-c f") 'recentf-ido-find-file)
;; (setq recentf-exclude
;;       (append recentf-exclude
;; 	      '("\\.html$" "\\.bmk$" "~$"
;; 		"emacs.d/var" "\\.log$")))

(global-set-key (kbd "C-c f") 'helm-recentf)
(recentf-mode 1)

;;; * eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; * winner mode
(winner-mode 1)
(windmove-default-keybindings 'meta)

;;; * electric-pair-mode FIXME
;;  to be removed (?)
(electric-pair-mode 0)

;; * paren-mode FIXME
;; to be removed (?)
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match "#a9a9a9")

;;; * clean-mode-line
;; ;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
;; (defvar mode-line-cleaner-alist
;;   '(;;(auto-complete-mode . " あ") ;; FIXME: break modeline on console
;;     ;;(auto-complete-mode . " ac")
;;     (company-mode . " こ")
;;     (yas-global-mode . " や")
;;     (yas-minor-mode . " や")
;;     (eldoc-mode . "")
;;     (abbrev-mode . "")
;;     (hs-minor-mode-hook . "")
;;     (workgroups-mode . "")
;;     ;;(undo-tree-mode . "う")
;;     (undo-tree-mode . "")
;;     (lisp-interaction-mode . "LI")
;;     (hi-lock-mode . "")
;;     (volatile-highlights-mode . "")
;;     (python-mode . "Py")
;;     ;;(helm-mode . " へ")
;;     (helm-mode . "")
;;     (emacs-lisp-mode . "EL")
;;     (nxhtml-mode . "nx")
;;     (shell-script-mode . "sh")
;;     (which-key-mode . "" ))

;;   "Alist for `clean-mode-line'.

;; ;; When you add a new element to the alist, keep in mind that you
;; ;; must pass the correct minor/major mode symbol and a string you
;; ;; want to use in the modeline *in lieu of* the original.")

;; (defun clean-mode-line ()
;;  (interactive)
;;  (cl-loop for cleaner in mode-line-cleaner-alist
;;        do (let* ((mode (car cleaner))
;; 		  (mode-str (cdr cleaner))
;; 		  (old-mode-str (cdr (assq mode minor-mode-alist))))
;;             (when old-mode-str
;; 	       (setcar old-mode-str mode-str))
;; 	     ;; major mode
;;             (when (eq mode major-mode)
;;               (setq mode-name mode-str)))))

;; (add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; * fonts
;; default font
;; `set-default-font' obsolete function from Emacs 24
;; (set-frame-font "-*-Hack-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" nil t)
;; nihongo
;; (set-fontset-font "fontset-default" 'han "STSong-23") ; 94
;; (set-fontset-font "fontset-default" 'kana "STSong-32")

;; unicode
;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))


;;; * calc
(require 'calc-units)

;;; * eww
(eval-after-load "eww"
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)
	  (setq eww-bookmarks-directory (concat user-emacs-directory "var/eww"))))

; http://ergoemacs.org/emacs/emacs_eww_web_browser.html
(when (fboundp 'eww)
  (progn
    (defun pedro-rename-to-title-eww-hook ()
      "Rename eww browser's buffer so sites open in new page."
      (rename-buffer (concat "eww - " (plist-get eww-data :title)) t)) ;"eww" t)) 		; eww-data
    (add-hook 'eww-after-render-hook 'pedro-rename-to-title-eww-hook)))

;; (setq eww-search-prefix "https://www.google.com/search?hl=gl&site=&source=hp&q=")

;;; * url
(setq url-configuration-directory (concat user-emacs-directory "var/url"))
(setq url-cache-directory (concat user-emacs-directory "var/url/cache"))

;;; * custom themes
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/includes/pedro-themes")


;;; * auto-save & backup
(setq ;; backup-inhibited t
      kept-new-versions 20
      kept-old-versions 20
      version-control t
      backup-directory-alist '(("." . "~/.emacs.d/var/backups"))
      delete-old-versions t
      ;; vc-make-backup-files nil
      auto-save-file-name-transforms '((".*" "~/.emacs.d/var/auto-save-list/" t)))

;; *  history persistent
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      savehist-file (concat user-emacs-directory "var/history")
      kill-ring-max 1000)

(setq-default history-length 1000)
(savehist-mode 1)

;; *  narrow
;;https://www.emacswiki.org/emacs/BasicNarrowing
(put 'narrow-to-region 'disabled nil)

;;; * ERC
;; (setq erc-fill-column 97
;;       erc-header-line-format "[%l] [%m] %n @ %t"
;;       erc-nick-uniquifier "_"
;;       erc-server-reconnect-timeout 10
;;       erc-server-coding-system '(iso-8859-1 . iso-8859-1))

;; ;;http://www.emacswiki.org/emacs/ErcTruncation
;; (defun erc-cmd-FLUSH (&rest ignore)
;;   "Erase the current buffer."
;;   (let ((inhibit-read-only t))
;;     (erase-buffer)
;;     (message "Flushed contents of channel")
;;     t))

;; ;; http://soundbible.com/2154-Text-Message-Alert-1.html
;; (setq sound-default "~/tmp/Channel.mp3")

;; (defun sound (&optional path)
;;   (start-process-shell-command
;;    "sound"
;;    nil
;;    (concat "play -q " (or path sound-default)))) ;; play from sox

;; ;; private message sound
;; (defun erc-my-privmsg-sound (proc parsed)
;;   (let* ((tgt (car (erc-response.command-args parsed)))
;; 	 (privp (erc-current-nick-p tgt)))
;;     (and
;;      privp
;;      (sound)
;;      nil))) ;We must return nil. See help for `erc-server-PRIVMSG-functions'

;; (add-hook 'erc-server-PRIVMSG-functions
;; 	  'erc-my-privmsg-sound)

;; ;; nickuser sound
;; (setq erc-text-matched-hook nil)
;; (add-hook 'erc-text-matched-hook
;; 	  (lambda (match-type nickuserhost message)
;; 	    ;; (message "%s - %s - %s " (erc-current-nick) (string-match-p (regexp-quote (erc-current-nick)) message) message)
;; 	    (cond
;; 	      ((string-match-p (regexp-quote (erc-current-nick)) message)
;; 	       (sound "~/tmp/Nick.mp3")))))
;; (add-hook 'erc-text-matched-hook 'erc-log-matches)

;; ;; (setq erc-log-insert-log-on-open nil)
;; ;; (setq erc-log-channels t)
;; ;; (setq erc-log-channels-directory "~/.irclogs/")
;; ;; (setq erc-save-buffer-on-part t)
;; ;; (setq erc-hide-timestamps nil)
;; ;; (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs) ; [TO TEST]

;; (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))

;; (setq erc-track-use-faces t)
;; (setq erc-format-query-as-channel-p t
;;       erc-track-priority-faces-only 'all

;;       ;; erc-current-nick-highlight-type 'nick
;;       erc-track-faces-priority-list '(erc-nick-msg-face ;; vermello.. private mensage
;; 				       erc-default-face ;; branco
;;                                       erc-pal-face ;; rosa , for pals
;;                                       erc-error-face ; vermello , face for erros
;;                                       erc-keyword-face ;; fosforito, face for your keywords.
;;                                       erc-nick-default-face ;; ERC nickname default face.
;;                                       erc-direct-msg-face ;; vermello,  face used for messages you receive in the main erc buffer.
;;                                       erc-dangerous-host-face
;;                                       erc-notice-face ;;
;;                                       erc-prompt-face))

;; (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
;;   (if (erc-query-buffer-p)
;;       (setq ad-return-value (intern "erc-current-nick-face"))
;;     ad-do-it))

;; (defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
;;   (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'nil))
;;   ad-do-it
;;   (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'all)))


;;; * terminal-mode
(if (not (display-graphic-p))
    (setq mode-line-cleaner-alist '(
    (company-mode . "")
    (yas-global-mode . "")
    (yas-minor-mode . "")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (hs-minor-mode-hook . "")
    (workgroups-mode . "")
    (undo-tree-mode . "")
    (lisp-interaction-mode . "LI")
    (hi-lock-mode . "")
    (volatile-highlights-mode . "")
    (python-mode . "Py")
    (helm-mode . "")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx")
    (smartparens-mode . "")
    (shell-script-mode . "sh"))))

;; focus out
(add-hook 'focus-out-hook #'garbage-collect)

;; * browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")


;; ;;; * gnus
;; (when (file-exists-p (concat user-emacs-directory "includes/pedro-gnus.el"))
;;   (setq gnus-init-file (concat user-emacs-directory "includes/pedro-gnus.el")))

;;; * use-package :diminish builtin
(use-package abbrev
	     :diminish abbrev-mode)

(use-package autorevert
	     :diminish auto-revert-mode)

(use-package eldoc
	     :diminish eldoc-mode)

(use-package elisp-mode
	     :diminish emacs-lisp-mode)

;; archive-mpde
(add-to-list 'auto-mode-alist '("\\.\\(cbr\\)\\'" . archive-mode))
