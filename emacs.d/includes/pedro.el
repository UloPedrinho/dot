(setq calendar-time-zone 60)
(setq calendar-week-start-day 1)
(setq calendar-standard-time-zone-name "CET")
(setq calendar-daylight-time-zone-name "CEST")

(setq display-time-world-list '(("Europe/London" "London")
				("Europe/Paris" "Paris")
				("Asia/Seoul" "Seoul")
				("Asia/Tokyo" "Tokyo")))

;; after-make-frame
(defun pedro/after-make-frame (&optional frame)
  ;;TODO: fortune cookie
  (if (framep frame)
      (select-frame frame))
  (let ((org-agenda-buffer-name "*Org Agenda Todo*"))
    (org-agenda nil "x"))
  (let ((org-agenda-buffer-name "*Org Agenda Today*"))
    (org-agenda nil "y"))
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*Org Agenda Todo*"))

(add-hook 'after-make-frame-functions 'pedro/after-make-frame)


;; unfill-region
;; https://www.emacswiki.org/emacs/UnfillRegion
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
logical line.  This is useful, e.g., for use with
`visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; disable mouse yank
(global-set-key (kbd "<mouse-2>") nil)


;; eww !!! ;FIXME
(defvar *pedro/eww-search-prefix* '((academia "http://academia.gal/dicionario_rag/searchNoun.do?nounTitle=")
				    (wikipedia "http://en.wikipedia.org/wiki/")
				    (wr-en "http://www.wordreference.com/definition/")
				    (wr-es "http://www.wordreference.com/es/translation.asp?tranword=")))

(defun pedro/eww ()
  (interactive)
  (let* ((site (intern (helm :sources (helm-build-sync-source "test"
				        :candidates '(academia wikipedia rae wr-en wr-es)
				        :fuzzy-match t)
			     :buffer "*helm test*")))
	 (url-string (read-from-minibuffer "Entry Word: ")))


    (eww (concat (cadr (assoc site *pedro/eww-search-prefix*)) url-string))))

;;;; load theme
(defun pif/load-theme (new-theme)
  ;; disable all themes
  (mapc (lambda (theme)
          (disable-theme theme))
        custom-enabled-themes)
  ;; load new theme
  (load-theme new-theme))

;; emms
(defun pedro-emms-get-current-song-info ()
  "Returns current song info plist"
  (let* ((get-string-func '(lambda (regexp)
                             (substring mpc-output
                                        (string-match regexp mpc-output)
                                        (match-end 0))))
         (mpc-output (shell-command-to-string "mpc"))
         (playing (if (eq (string-match "volume:" mpc-output) 0) nil t))
         (title (if playing (substring mpc-output 0 (string-match "\n" mpc-output)) "-------------"))
         (state    (if playing (funcall get-string-func "\\[.*\\]") "Paused/Stopped"))
         (position (if playing (funcall get-string-func "#[0-9]+\\/[0-9]+") "--"))
         (time     (if playing (funcall get-string-func "[0-9]+:[0-9]+/[0-9]+:[0-9]+") "--/--"))
         (volume (funcall get-string-func "volume: +[0-9%]+"))
         (repeat (funcall get-string-func "repeat: [a-z]+"))
         (random (funcall get-string-func "random: [a-z]+"))
         (single (funcall get-string-func "single: [a-z]+")))

    `((title . ,title)
      (state . ,state)
      (position . ,position)
      (time . ,time)
      (volume . ,(cadr (split-string volume)))
      (repeat . ,(cadr (split-string repeat)))
      (random . ,(cadr (split-string random)))
      (single . ,(cadr (split-string single))))))

;; (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))

;;; install
(defun pedro/download-and-save-files-to-dir (urls dir)
      "Download and save files to DIR. URLS is a list of urls strings."
      (let ((old-buffer (current-buffer))
            (buffer-name "*download-and-save-temp-buffer*")
            filename)
        (get-buffer-create buffer-name)

        (save-excursion
          (set-buffer buffer-name)
          (dolist (url urls)
            (setq filename (concat dir "/" (file-name-nondirectory (url-unhex-string
                                                                    (url-filename
                                                                     (url-generic-parse-url url))))))
            (delete-region (point-min) (point-max))
            (url-insert-file-contents url)
            (write-region (point-min) (point-max) filename))
          (set-buffer old-buffer))))

;;; org-gtd
(add-to-list 'load-path "~/dev/computer/emacs/org-gtd/stable")
(require 'org-gtd)

;;;  diccionario sinónimos de galego
(add-to-list 'load-path "~/dev/computer/emacs/dictionaries/stable")
(require 'galego-sinonimos)

;;; sunrise-sunset
;; %%(pedro-sunrise-sunset 'car "☼")
;; %%(pedro-sunrise-sunset 'cadr "🌙")
(defun pedro-get-sunrise-sunset (f txt)
  (concat (car (split-string (apply #'solar-time-string (funcall f (solar-sunrise-sunset (calendar-current-date)))))) " " txt))

(defun pedro-sunrise-sunset (f txt)
  (with-no-warnings (defvar date))
  (if (calendar-date-equal date (calendar-current-date))
      (pedro-get-sunrise-sunset f txt)))


(defun pedro-diary-daily (time txt)
  (with-no-warnings (defvar date))
  (if (calendar-date-equal date (calendar-current-date))
      (concat time " " txt)))

(defun pedro-diary-week (day-time txt)
  (with-no-warnings (defvar date))
  (let* ((dt (split-string day-time))
         (day (downcase (car dt)))
         (time (cadr dt)))
    (and (string= day (downcase (format-time-string "%a" (current-time))))
         (calendar-date-equal date (calendar-current-date))
         (concat time " " txt))))

;; holidays
(setq holiday-other-holidays
      '((holiday-fixed 1 6 "reis cristiáns")))
