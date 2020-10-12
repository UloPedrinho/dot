;;; optional packages

(use-package emms
	     :config
	     (setq emms-directory (concat user-emacs-directory "var/emms" ))
	     (require 'cl)
	     (require 'emms-setup)
	     (require 'emms-player-mpd)
             (require 'emms-volume)
	     (emms-all)
             (setq emms-seek-seconds 5
                   emms-player-list '(emms-player-mpd)
                   emms-info-functions '(emms-info-mpd)
                   emms-volume-change-function 'emms-volume-mpd-change
                   emms-player-mpd-music-directory "~/tmp/music") ;FIXME: use mpd.conf data

	     (setq emms-player-mpd-server-name "localhost")
	     (setq emms-player-mpd-server-port "6600")

             ;; disable emms in modeline
             (emms-playing-time-disable-display)
             (emms-mode-line nil)

	     ;; (emms-default-players)
	     ;; (add-to-list 'emms-player-list 'emms-player-mpd)
	     ;; (add-to-list 'emms-info-functions 'emms-info-mpd)

             (defun mpd/start-music-daemon ( )
               (interactive)
               (shell-command "mpd")
               ;; (mpd/update-database)
               (emms-player-mpd-connect)
               ;; (emms-cache-set-from-mpd-all)
               (message "MPD Started!"))

             (defun mpd/kill-music-daemon ()
               (interactive)
               (emms-stop)
               (call-process "killall" nil nil nil "-9" "mpd")
               (message "MPD Killed!"))

             (defun mpd/update-database ()
               (interactive)
               (call-process "mpc" nil nil nil "update")
               (message "MPD Database Updated!"))
	     :ensure t)

(use-package pdf-tools
	     :config
	     (pdf-tools-install)
             ;; chage 'pdf-view-bookmark-jump-handler' to 'pdf-view-bookmark-jump'
	     (defun pdf-view-bookmark-make-record  (&optional no-page no-slice no-size no-origin)
	       ;; TODO: add NO-PAGE, NO-SLICE, NO-SIZE, NO-ORIGIN to the docstring.
	       "Create a bookmark PDF record. The optional, boolean args exclude certain attributes."
	       (let ((displayed-p (eq (current-buffer)
	     			      (window-buffer))))
	     	 (cons (buffer-name)
	     	       (append (bookmark-make-record-default nil t 1)
	     		       `(,(unless no-page
	     			    (cons 'page (pdf-view-current-page)))
	     			 ,(unless no-slice
	     			    (cons 'slice (and displayed-p
	     					      (pdf-view-current-slice))))
	     			 ,(unless no-size
	     			    (cons 'size pdf-view-display-size))
	     			 ,(unless no-origin
	     			    (cons 'origin
	     				  (and displayed-p
	     				       (let ((edges (pdf-util-image-displayed-edges nil t)))
	     					 (pdf-util-scale-pixel-to-relative
	     					  (cons (car edges) (cadr edges)) nil t)))))
	     			 (handler . pdf-view-bookmark-jump))))))

             ;; http://pragmaticemacs.com/emacs/more-pdf-tools-tweaks/
             ;; (setq pdf-view-resize-factor 1.1)

             ;; http://babbagefiles.blogspot.com.es/2017/11/more-pdf-tools-tricks.html
             ;; midnite mode hook
             (add-hook 'pdf-view-mode-hook (lambda ()
                                             (pdf-view-midnight-minor-mode))) ; automatically turns on midnight-mode for pdfs

             (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; set the amber profile as default (see below)

             (defun bms/pdf-no-filter ()
               "View pdf without colour filter."
               (interactive)
               (pdf-view-midnight-minor-mode -1)
               )

             ;; change midnite mode colours functions
             (defun bms/pdf-midnite-original ()
               "Set pdf-view-midnight-colors to original colours."
               (interactive)
               (setq pdf-view-midnight-colors '("#839496" . "#002b36" )) ; original values
               (pdf-view-midnight-minor-mode)
               )

             (defun bms/pdf-midnite-amber ()
               "Set pdf-view-midnight-colors to amber on dark slate blue."
               (interactive)
               (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; amber
               (pdf-view-midnight-minor-mode)
               )

             (defun bms/pdf-midnite-green ()
               "Set pdf-view-midnight-colors to green on black."
               (interactive)
               (setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; green
               (pdf-view-midnight-minor-mode))

             (defun bms/pdf-midnite-colour-schemes ()
               "Midnight mode colour schemes bound to keys"
               (local-set-key (kbd "!") (quote bms/pdf-no-filter))
               (local-set-key (kbd "@") (quote bms/pdf-midnite-amber))
               (local-set-key (kbd "#") (quote bms/pdf-midnite-green))
               (local-set-key (kbd "$") (quote bms/pdf-midnite-original)))

             (add-hook 'pdf-view-mode-hook 'bms/pdf-midnite-colour-schemes)

	     :ensure t)

;; epub
;; (use-package ereader
;; 	     :ensure t)

(use-package nov
             :config
             (setq nov-text-width 80)
             ;; (require 'justify-kp)
             ;; (setq nov-text-width most-positive-fixnum)

             ;; (defun my-nov-window-configuration-change-hook ()
             ;;   (my-nov-post-html-render-hook)
             ;;   (remove-hook 'window-configuration-change-hook
             ;;                'my-nov-window-configuration-change-hook
             ;;                t))

             ;; (defun my-nov-post-html-render-hook ()
             ;;   (if (get-buffer-window)
             ;;       (let ((max-width (pj-line-width))
             ;;             buffer-read-only)
             ;;         (save-excursion
             ;;          (goto-char (point-min))
             ;;          (while (not (eobp))
             ;;                 (when (not (looking-at "^[[:space:]]*$"))
             ;;                   (goto-char (line-end-position))
             ;;                   (when (> (shr-pixel-column) max-width)
             ;;                     (goto-char (line-beginning-position))
             ;;                     (pj-justify)))
             ;;                 (forward-line 1))))
             ;;       (add-hook 'window-configuration-change-hook
             ;;                 'my-nov-window-configuration-change-hook
             ;;                 nil t)))

             ;; (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)
             (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
	     :ensure t)

;; dictionary
(use-package dictionary
	     :config
	     ;; https://github.com/myrkr/dictionary-el
	     (autoload 'dictionary-search "dictionary"
		       "Ask for a word and search it in all dictionaries" t)
	     (autoload 'dictionary-match-words "dictionary"
		       "Ask for a word and search all matching words in the dictionaries" t)
	     (autoload 'dictionary-lookup-definition "dictionary"
		       "Unconditionally lookup the word at point." t)
	     (autoload 'dictionary "dictionary"
		       "Create a new dictionary buffer" t)
	     ;; (autoload 'dictionary-mouse-popup-matching-words "dictionary"
	     ;;   "Display entries matching the word at the cursor" t)
	     (autoload 'dictionary-popup-matching-words "dictionary"
		       "Display entries matching the word at the point" t)
	     (autoload 'dictionary-tooltip-mode "dictionary"
		       "Display tooltips for the current word" t)
	     (autoload 'global-dictionary-tooltip-mode "dictionary"
		       "Enable/disable dictionary-tooltip-mode for all buffers" t)
	     :ensure t)

(use-package google-translate
	     :ensure t)

;; (use-package kanji-mode
;; 	     ;; this package needs to install "kakasi" application (deb-package: kakasi)
;; 	     ;; to convert from-to
;; 	     :ensure t)

;; (use-package erc-hl-nicks
;; 	     :ensure t)

(use-package elfeed
	     :config
	     (setq elfeed-search-title-max-width 100)

	     ;; elfeed tags + helm
	     (setq pedro/elfeed-helm-tags-source
		   '((name . "elfeed tags")
		     (candidates . (lambda ()
				     (cons 'nil (elfeed-db-get-all-tags))))
		     (action . (("show tag(s) entries" . (lambda (candidate)
							   (if (equal candidate "nil")
							       (setq elfeed-search-filter "+unread")
							       (setq elfeed-search-filter (concat "+unread"
												  " +"
												  (mapconcat 'identity (helm-marked-candidates) " +"))))
							   (elfeed-search-update :force)))
				("remove 'unread' tag from entries" . (lambda (candidate)
									(mark-whole-buffer)
									(elfeed-search-untag-all-unread)))))))

	     (defun pedro/elfeed-helm-tags-source ()
	       (interactive)
	       (helm :sources '(pedro/elfeed-helm-tags-source)))

	     (define-key elfeed-search-mode-map (kbd "C-SPC") 'pedro/elfeed-helm-tags-source)

	     ;; load feeds file
	     (load-file (concat user-emacs-directory "includes/pedro-elfeed-data.el"))

	     ;; elfeed-goodies (TODO)
	     (setq elfeed-goodies/entry-pane-position 'bottom)

	     ;; mail style
	     (setq elfeed-show-refresh-function #'elfeed-show-refresh--mail-style)
	     :ensure t)

;; (use-package elfeed-org
;;   :ensure t
;;   :config
;;   (elfeed-org)
;;   (setq rmh-elfeed-org-files (list "~/.emacs.d/var/elfeed/elfeed.org")))

(use-package elfeed-goodies
	     :config
	     (elfeed-goodies/setup)
	     :ensure t)

;; FIXME: To be deleted. not updated since 2016..
;; (use-package helm-firefox
;; 	     :config
;; 	     (if (string-equal system-type "darwin") ; Mac OS
;; 		 (setq helm-firefox-default-directory "~/Library/Application Support/Firefox/"))
;; 	     :ensure t)

(use-package counsel-ffdata
  :ensure t)

(use-package org-pomodoro
	     :ensure t)

;; TODO: not working.. 
;; (use-package howdoi
;; 	     :ensure t)

(use-package ledger-mode
	     :config
	     (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
	     :ensure t)

(use-package clipmon
	     :ensure t)

(use-package link-hint
	     :ensure t)

(use-package org-brain
             :ensure t
             :init
             (setq org-brain-path "~/org/learn/brain/")
             ;; For Evil users
             ;; (eval-after-load 'evil
             ;;   (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
             :config
             (setq org-id-track-globally t)
             (setq org-id-locations-file "~/.emacs.d/var/org-id-locations")
             (setq org-brain-visualize-default-choices 'all)

             (defun org-brain-insert-resource-icon (link)
               "Insert an icon, based on content of org-mode LINK."
               (insert (format "%s "
                               (cond ((string-prefix-p "http" link)
                                      (cond ((string-match "wikipedia\\.org" link)
                                             (all-the-icons-faicon "wikipedia-w"))
                                            ((string-match "github\\.com" link)
                                             (all-the-icons-octicon "mark-github"))
                                            ((string-match "vimeo\\.com" link)
                                             (all-the-icons-faicon "vimeo"))
                                            ((string-match "youtube\\.com" link)
                                             (all-the-icons-faicon "youtube"))
                                            (t
                                             (all-the-icons-faicon "globe"))))
                                     ((string-prefix-p "brain:" link)
                                      (all-the-icons-fileicon "brain"))
                                     ((string-suffix-p "epub" link)
                                      (all-the-icons-octicon "book"))
                                     (t
                                      (all-the-icons-icon-for-file link))))))

             (add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)
             (add-hook 'org-brain-visualize-mode-hook #'visual-line-mode)
             )

(use-package ascii-art-to-unicode
             :ensure t
             :config
             (defun aa2u-buffer ()
               (aa2u (point-min) (point-max)))

             (add-hook 'org-brain-after-visualize-hook #'aa2u-buffer))

(use-package helm-org-rifle
             :ensure t
             :config
             (defun helm-org-rifle-brain ()
               "Rifle files in `org-brain-path'."
               (interactive)
               (helm-org-rifle-directories (list org-brain-path))))

(use-package all-the-icons
             :ensure t)

(use-package helm-ag
	     :ensure t
             :config
             (setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
             (setq dumb-jump-prefer-searcher 'ag))

(use-package pandoc
	     :ensure t)

(use-package speed-type
	     :ensure t)

;; (use-package treemacs
;; 	     :ensure t)

(use-package helpful
  :ensure t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

(use-package info-colors
             :config
             (add-hook 'Info-selection-hook 'info-colors-fontify-node)
             :ensure t)

(use-package beacon
             :ensure t
             :diminish beacon-mode
             :config
             (beacon-mode +1))

(use-package vi-tilde-fringe
             :ensure t
             :diminish vi-tilde-fringe-mode
             :config
             (global-vi-tilde-fringe-mode nil))

;; (use-package company-childframe
;;              :ensure t
;;              :diminish company-childframe-mode
;;              :config
;;              (require 'company-childframe)
;;              (company-childframe-mode 1))

(use-package diminish
  :ensure t
  :config
  (diminish 'company-childframe-mode)
  ;; (diminish 'company-mode)
  (diminish 'smartparens-mode)
  (diminish 'helm-mode)
  (diminish 'which-key-mode)
  (diminish 'undo-tree-mode)
  (diminish 'eldoc-mode)
  (diminish 'highlight-changes-mode)
  (diminish 'volatile-highlights-mode)
  (diminish 'zoom-mode)
  (diminish 'highlight-thing-mode)
  (diminish 'vi-tilde-fringe-mode)
  (diminish 'beacon-mode)
  (diminish 'yas-minor-mode)
  (diminish 'outshine-mode)
  (diminish 'outline-minor-mode)
  (diminish 'buffer-face-mode)
  (diminish 'elisp-slime-nav-mode)
  (diminish 'git-gutter-mode))

;; (use-package persp-mode
;;   :ensure t
;;   :config
;;   (setq wg-morph-on nil) ;; switch off animation
;;   (setq persp-autokill-buffer-on-remove 'kill-weak)
;;   ;; (persp-mode 1)
;;   (setq persp-save-dir (concat user-emacs-directory "var/persp/"))
;;   ;; (global-set-key (kbd "C-x b") #'persp-switch-to-buffer)
;;   ;; (global-set-key (kbd "C-x k") #'persp-kill-buffer))
;;   )

(use-package circe
  :ensure t
  :config
  (setq circe-network-options
        '(("hispano"
           ;; :tls t
           :host "irc.chathispano.com"
           :port 6667
           :nick "mauzinho-ng"
           :user "31415"
           :realname "mauzinho")
          ("lispano"
           ;; :tls t
           :host "irc.chathispano.com"
           :port 6667
           :nick "galdor"
           :user "galdor"
           :realname "galdor"))
        ;; strings format
        circe-format-self-say "<{nick}> {body}"
        circe-format-server-join (concat ;; "*** "
                                         (propertize "->" 'face '(:foreground "#008b45" :weight bold))
                                         " {nick} $ {userhost}")
        circe-format-server-quit-channel (concat ;; "*** "
                                                 (propertize "<=" 'face '(:foreground "#27408b" :weight bold))
                                                 " {nick} $ {reason}")
        circe-format-server-part (concat ;;"*** "
                                         (propertize "<-" 'face '(:foreground "#27408b" :weight bold))
                                         " {nick} $ {reason}")
        circe-format-server-nick-change (concat (propertize "==" 'face '(:foreground "#8b6508" :weight bold))
                                                " {old-nick} => {new-nick}")
        circe-format-server-mode-change (concat (propertize "--" 'face '(:foreground "#473c8b" :weight bold))
                                                " {change} on {target} by {setter}"))
  ;; kick string format
  (setq circe--irc-format-server-numeric "%s")
  (puthash "KICK" `(0 ,(concat (propertize "<-" 'face '(:foreground "red" :weight bold))
                                  " {1} kicked by {origin}: {2}"))
           circe-display-table)
  ;; mode string format

  ;; time
  (setq lui-time-stamp-position 'left
        lui-time-stamp-format "%d%m%y%H%M "
        lui-time-stamp-only-when-changed-p nil)

  ;; logging
  ;; (load "lui-logging" nil t)
  ;; (enable-lui-logging-globally)
  ;; (setq lui-logging-directory "~/user/mnt/to-sync/var/logs/irc"
  ;;       lui-logging-file-format "{buffer}.%m%Y.log"
  ;;       lui-logging-format "%d%m%y%H%M {text}")


  ;; nicks colors
  (require 'circe-color-nicks)
  (enable-circe-color-nicks))

(use-package circe-notifications
  :ensure t
  :config
  (autoload 'enable-circe-notifications "circe-notifications" nil t)
  (eval-after-load "circe-notifications"
    '(setq circe-notifications-watch-strings
           '("mauzinho" "mauzinho-ng" "dana83" "martinha83")))
  (add-hook 'circe-server-connected-hook 'enable-circe-notifications))


;; FIXME: or org-drill?
;; (use-package pamparam
;;   :ensure t
;;   :config
;;   (setq pamparam-alist
;;         '(("~/org/learn/brain/music.org"
;;            . "~/org/learn/brain/music.pam"))
;;         pamparam-path "~/org/learn/brain/music.pam"))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package chronos
  :ensure t
  :config
  (setq chronos-shell-notify-program "mpv"
        chronos-shell-notify-parameters '("--really-quiet"
                                          "--af=scaletempo=speed=pitch"
                                          "--speed=0.65"
                                          "~/tmp/music/bell.mp3")
        ;; chronos-text-to-speech-program "espeak"
        ;; chronos-text-to-speech-program-parameters "-s 100"
        chronos-expiry-functions '(chronos-buffer-notify
                                   chronos-desktop-notifications-notify
                                   chronos-shell-notify))

  (defun pedro-my-chronos-repeat (time message repetitions)
    (let  ((time-unit (substring time -1 nil))
           (time-integer (string-to-number (substring time 0 -1))))
      (dotimes (n repetitions)
        (chronos-add-timer (concat (number-to-string (* (if (string-equal time-unit "h")
                                                            (* time-integer 60)
                                                          time-integer)
                                                        (1+ n)))
                                   "m")
                           message nil)))))

(use-package focus
  :ensure t
  :config
  (setq focus-mode-to-thing '((prog-mode . defun) (text-mode . sentence) (org-mode . sentence))))

(use-package load-theme-buffer-local
    :ensure t
    :config
    (add-hook 'eww-mode-hook (lambda () (load-theme-buffer-local 'whiteboard (current-buffer)))))


;; TEST
(use-package org-sidebar
  :ensure t)

(use-package disk-usage
  :ensure t)

(use-package auto-highlight-symbol
  :ensure t)

(use-package org-board
  :ensure t)

(use-package i3wm-config-mode
  :ensure t)

