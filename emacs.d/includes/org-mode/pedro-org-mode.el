;; * org-mode-hook
(add-hook 'org-mode-hook 'visual-line-mode 'org-indent-mode)

;; * various
;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))

(setq org-src-window-setup 'current-window)

(setq org-indent-mode t)

(setq org-hide-emphasis-markers t)

(setq org-catch-invisible-edits 'error)

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

;; https://orgmode.org/manual/Breaking-Down-Tasks.html
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; show hide white-space between headings when toggled
;; https://explog.in/notes/writingsetup.html
(customize-set-variable 'org-blank-before-new-entry
                        '((heading . nil)
                          (plain-list-item . nil)))
(setq org-cycle-separator-lines 1)


(setq org-agenda-tags-column -110)
(setq org-habit-graph-column 50)

(setq org-log-into-drawer t)

(setq org-id-method (quote uuidgen))
(setq org-cycle-include-plain-lists 'integrate ) ; t
(setq org-src-fontify-natively t)
(setq org-agenda-window-setup 'only-window) ; 'current-window

(add-to-list 'org-file-apps '(directory . emacs))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; * org-modules
(setq org-modules '(org-habit org-info org-drill))  ;org-gnus

;; * org-protocol
(require 'org-protocol)
;; https://github.com/sprig/org-capture-extension
;;; Kill the frame if one was created for the capture
(defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

(defun kk/delete-frame-if-neccessary (&rest r)
  (cond
    ((= kk/delete-frame-after-capture 0) nil)
    ((> kk/delete-frame-after-capture 1)
     (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
    (t
     (setq kk/delete-frame-after-capture 0)
     (delete-frame))))

(advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
(advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
(advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)

;; * org-mode Agenda
;; add contrib dir (have to be udpated ofen)
;; (add-to-list 'load-path (concat user-emacs-directory "plugins/org-contrib-7.9.2/lisp")

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cm" 'org-agenda-month-view)
(add-hook 'org-mode-hook 'turn-on-font-lock)

(setq org-extend-today-until 0)
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-span 15)
(setq org-agenda-start-day "-3d")

(when (file-exists-p "~/org/axenda/axenda.org")
  (setq org-agenda-files (quote ("~/org/axenda/axenda.org"
			         "~/org/notes.org"))))

(setq org-log-done 'time)
(setq org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "ABORTED" )))
(setq org-todo-keyword-faces '(("ANIVERSARIO" . (:background "gold" :foreground "sienna":weight bold))
                               ("TODO" . (:foreground "#4169e1" :weight bold))
                               ("STARTED" . (:foreground "dark orange" :weight bold))
                               ("CURRENT" . (:foreground "#00bfff" :weight bold))
                               ("CONTINUED" . (:foreground "#00bfff" :weight bold))
                               ("WAITING" . (:foreground "#cd2626" :weight bold))
                               ("DONE" . (:foreground "green4" :weight bold))
                               ("ENDED" . (:foreground "green4" :weight bold))
                               ("ABORTED" . (:foreground "gray" :weight bold))
			       ("STOPPED" . (:foreground "#d3d3d3" :weight bold))
                               ("EVENTO" . (:background "red" :foreground "white" :weight bold))
			       ("IMPROVE" . (:foreground "blue" :weight bold))
			       ("CONCERTO" . (:background "#6495ed" :foreground "white" :weight bold))
                               ("DÍA" . (:background "#b23aee" :foreground "white" :weight bold))
                               ("ESCURO" . (:background "black" :foreground "#ffd700" :weight bold ))
                               ("LER" . (:background "#c1ffc1" :foreground "#20b2aa" :weight bold))
                               ("REMATADO" . (:background "#8b8b00" :foreground "white" :weight bold))
                               ("PAIXÓN" . (:background "#ffc0cb" :foreground "white" :weight bold))
                               ("DOWNLOAD" . (:background "#87ceff" :foreground "#104e8b" :weight bold))
			       ("BACKUP" . (:background "red" :foreground "white" :weight bold))


                               ;; org-relato  [night-notebook theme]
                               ("TEMA" . (:foreground "#1e90ff"))
			       ("PERSOAXES" . (:foreground "#1e90ff"))
                               ("POSIBLE" . (:foreground "#eec900"))
                               ("IMPROBABLE" . (:foreground "#ee4000"))
                               ("DESENROLAR" . (:foreground "#00ff00"))

                               ))

;; * org-agenda-custom-commands

(setq org-agenda-custom-commands
      '(("a" "agenda"
	 ((agenda ""
           ((org-agenda-this-buffer-name "*Org Agenda Week")))))
        ("x" "todos"
         ((tags "+LEVEL=3+current"
	   ((org-agenda-sorting-strategy '(priority-down todo-state-down))
	    (org-agenda-skip-function
	     '(org-agenda-skip-entry-if 'todo '("DONE" "ABORTED")))))
	  (tags-todo "STYLE=\"habit\"+daily"  ; FIXME +SCHEDULED<=\"<today>\"
           ((org-agenda-overriding-header "Daily\n")
            (org-agenda-sorting-strategy
             '(priority-down time-down todo-state-up
	       effort-up category-keep))))
	  (tags "weekly"  ; FIXME +SCHEDULED<=\"<today>\"
           ((org-agenda-overriding-header "Weekly\n")
            (org-agenda-sorting-strategy
             '(priority-down time-down todo-state-up
               effort-up category-keep))))))
        ("y" "agenda today"
         ((agenda ""
           ((org-agenda-span 4)
            (org-agenda-start-day "0d")))))
	;; ("z" "agenda-current-daily"
	;;  ((tags-todo "STYLE=\"habit\"+SCHEDULED<=\"<today>\""
        ;;    ((org-agenda-overriding-header "Daily")
        ;;     (org-agenda-sorting-strategy
        ;;      '(priority-down time-down todo-state-up
        ;;        effort-up category-keep))))))
))


;; habits
(setq org-habit-show-habits nil)
;; To show habits graph in todo list agenda view
;; https://emacs.stackexchange.com/questions/13360/org-habit-graph-on-todo-list-agenda-view?rq=1
(defvar my/org-habit-show-graphs-everywhere t
  "If non-nil, show habit graphs in all types of agenda buffers.

Normally, habits display consistency graphs only in
\"agenda\"-type agenda buffers, not in other types of agenda
buffers.  Set this variable to any non-nil variable to show
consistency graphs in all Org mode agendas.")

(defun my/org-agenda-mark-habits ()
  "Mark all habits in current agenda for graph display.

This function enforces `my/org-habit-show-graphs-everywhere' by
marking all habits in the current agenda as such.  When run just
before `org-agenda-finalize' (such as by advice; unfortunately,
`org-agenda-finalize-hook' is run too late), this has the effect
of displaying consistency graphs for these habits.

When `my/org-habit-show-graphs-everywhere' is nil, this function
has no effect."
  (when (and my/org-habit-show-graphs-everywhere
             (not (get-text-property (point) 'org-series)))
    (let ((cursor (point))
          item data)
      (while (setq cursor (next-single-property-change cursor 'org-marker))
             (setq item (get-text-property cursor 'org-marker))
             (when (and item (org-is-habit-p item))
               (with-current-buffer (marker-buffer item)
                 (setq data (org-habit-parse-todo item)))
               (put-text-property cursor
                                  (next-single-property-change cursor 'org-marker)
                                  'org-habit-p data))))))

(advice-add #'org-agenda-finalize :before #'my/org-agenda-mark-habits)



;; * org-publishing
(if (file-exists-p (concat user-emacs-directory "includes/org-mode/pedro-org-publishing-projects.el"))
    (load (concat user-emacs-directory "includes/org-mode/pedro-org-publishing-projects.el")))

;; * org-capture
(load (concat user-emacs-directory "includes/org-mode/pedro-org-capture.el"))

;; * org-refile
(setq org-outline-path-complete-in-steps nil)

;; * org-babel
(setq org-babel-clojure-backend 'cider)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (shell . t)
   (java . t)
   (org . t)
   (latex . t)
   (lilypond . t)
   (lisp . t)
   (js . t)
   (R . t)
   (clojure . t)))

(add-to-list 'org-src-lang-modes '("js" . js2))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "ditaa")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; * org-crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

;; * org-clock
(setq org-clock-persist-file (concat user-emacs-directory "var/org-clock-save.el")
      org-clock-into-drawer t)
;; (setq org-clock-persist 'history  ;; FIXME!!!
;;       org-clock-into-drawer t)
;; (org-clock-persistence-insinuate)
(defun pedro/org-report-to-temp-buffer ()
  " "                                   ;;TODO:
  (interactive)
  (let ((full-buffer (buffer-substring (point-min) (point-max)))) ;FIXME bad solution use `buffer-substring'
    (with-output-to-temp-buffer (concat "Report from: " (buffer-name))
      (with-temp-buffer
          (org-mode)
        (insert full-buffer)
        (goto-char (point-max))         ;FIXME necessary after `insert' ?
        (insert (concat "\n" "#+BEGIN: clocktable :maxlevel 2 :scope file "
                        ":emphasize nil  :formula % :compact nil" "\n" "#+END:"))
        (org-clock-report t)
        (print (buffer-substring-no-properties  (search-forward "#+CAPTION") (search-forward "#+END")))))))

;; * org-eww
(require 'org-eww)

;; * org latex
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
;;; highlight inline latex code
(font-lock-add-keywords
 'org-mode
 '(("\\(@@latex:\\)\\(.*?\\)\\(@@\\)"
        (1 font-lock-comment-face)
        (2 '(org-latex-and-related))
        (3 font-lock-comment-face))
     ))

;; * org expiry
(require 'org-expiry)
;; (org-expiry-insinuate)
(setq org-expiry-inactive-timestamps t) ;; Insert inactive timestamps for created/expired properties

;; * org-drill
;; http://www.xiangji.me/2015/07/13/a-few-of-my-org-mode-customizations/
;; http://francismurillo.github.io/2017-03-30-Exploring-Emacs-rx-Macro/
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; (defvar pedro/rx-org-drill-code (rx (category japanese)))

;; (add-hook 'org-mode-hook 'pedro/org-drill-font-lock)

;; (defun pedro/org-drill-font-lock ()
;;   (interactive)
;;   (require 'org)
;;   (font-lock-add-keywords
;;    nil
;;    `((,pedro/rx-org-drill-code
;;       .
;;       ;; let's just use org-block
;;       (quote pedro/org-drill-japanese-face)))))

;; (defface pedro/org-drill-japanese-face
;;     '((t :width ultraexpanded
;;        :height 2.0
;;        ))
;;   "Face for function parameters."
;;   :group 'org-mode)

;; * org-appt FIXME
;;; https://lists.gnu.org/archive/html/emacs-orgmode/2013-02/msg00644.html
(require 'appt)
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '10) ;; warn every 10 minutes from t - appt-message-warning-time
(setq
 appt-message-warning-time '10  ;; send first warning 10 minutes before appointment
 appt-display-mode-line nil     ;; don't show in the modeline
 appt-display-format 'window)   ;; pass warnings to the designated window function
(appt-activate 1)                ;; activate appointment notification
(display-time)                   ;; activate time display

(org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

;; set up the call to terminal-notifier
;; (defvar my-notifier-path
;;  "~/terminal-notifier_1.4.2/terminal-notifier.app/Contents/MacOS/terminal-notifier")
(defun my-appt-send-notification (title msg)
  (let ((async-shell-command-buffer 'new-buffer))
    (async-shell-command "play -V0 -q --volume 0.5 ~/tmp/music/bell.mp3" nil nil)
    (async-shell-command (concat  "notify-send " title " "  msg) nil nil)))

;; designate the window function for my-appt-send-notification
(defun my-appt-display (min-to-app new-time msg)
  (my-appt-send-notification
   (format "'Appointment in %s minutes'" min-to-app)    ;; passed to -title in terminal-notifier call
   (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call
(setq appt-disp-window-function (function my-appt-display))

;; * org-journal
(defun org-journal-date-format-func (time)
  "Custom function to insert journal date header,
  and some custom text on a newly created journal file."
  (when (= (buffer-size) 0)
    (insert
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal")
       (`weekly "#+TITLE: Weekly Journal")
       (`monthly (concat "#+TITLE:  " (format-time-string "%B %Y") "\n"
                         "#+AUTHOR: " user-full-name "\n\n"))
       (`yearly "#+TITLE: Yearly Journal"))))

  (let ((date-string (format-time-string "%F %A")))
    (concat org-journal-date-prefix
            date-string
            (make-string (- 23 (length date-string)) ?\s)
            "[Galiza. Terra]")))

(use-package org-journal
  :ensure t
  :defer t
  :init
  (require 'org-datetree)
  (setq org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "ABORTED" )))

  :custom
  (org-journal-dir (expand-file-name "~/org/journal"))
  (org-journal-file-type 'monthly)
  (org-journal-date-format 'org-journal-date-format-func)
  ;; (org-journal-date-format (let ((date-string (format-time-string "%F %A")))
  ;;                                       (concat date-string
  ;;                                               (make-string (- 23 (length date-string)) ?\s)
  ;;                                               "[Galiza. Terra]"))) ;FIX where!
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STARTED\""))
