;; eshell
(setq eshell-history-size 3000000)
(setq eshell-directory-name (concat user-emacs-directory "var/eshell/"))
(setq eshell-aliases-file (concat user-emacs-directory "var/eshell/eshell-aliases"))
(setq eshell-history-file-name (concat user-emacs-directory "var/eshell/eshell-history"))
(setq eshell-last-dir-ring-file-name (concat user-emacs-directory "var/eshell/eshell-lastdir"))
(setq history-delete-duplicates t)
(defun eshell/clear ()
  (open-line (window-height))
  (eshell-send-input))

;; eshell ls file type colours
(require 'em-ls)
(set-face-attribute 'eshell-ls-directory nil  :foreground "#61afef")
(set-face-attribute 'eshell-ls-symlink nil  :foreground "#1f5582" :weight 'bold)
(set-face-attribute 'eshell-ls-archive nil  :foreground "#ff6c6b")
(set-face-attribute 'eshell-ls-executable nil :foreground "#00ff00" :weight 'normal)

(setq pedro-eshell--ls-video-regexp "\\.\\(mkv\\|avi\\|mpeg\\|mpg\\|webm\\|flv\\|mp4\\)")
(setq pedro-eshell--ls-audio-regexp "\\.\\(ogg\\|wav\\|mp3\\|m4a\\|flac\\|ape\\|mid\\)")
(setq pedro-eshell--ls-image-regexp "\\.\\(jpg\\|jpeg\\|png\\|gif\\|xpm\\|svg\\|xcf\\|kra\\)")
(setq pedro-eshell--ls-doc-regexp "\\.\\(pdf\\|epub\\|mobi\\)")

(setq eshell-ls-highlight-alist nil)
(add-to-list 'eshell-ls-highlight-alist
             (cons `(lambda (file attr)
                      (string-match ,pedro-eshell--ls-video-regexp file))
                   'bmkp-man))
(add-to-list 'eshell-ls-highlight-alist
             (cons `(lambda (file attr)
                      (string-match ,pedro-eshell--ls-audio-regexp file))
                   'bmkp-no-jump))
(add-to-list 'eshell-ls-highlight-alist
             (cons `(lambda (file attr)
                      (string-match ,pedro-eshell--ls-image-regexp file))
                   'bmkp-non-file))
(add-to-list 'eshell-ls-highlight-alist
             (cons `(lambda (file attr)
                      (string-match ,pedro-eshell--ls-doc-regexp file))
                   'completions-annotations))

;; eshell prompt
(defun pedro-eprompt--is-a-git-dir ()
  "RETURN A STRING IF TRUE AND A NUMBER IF FALSE.
     If is a git dir returns a string with the branch name, in other way a number with the status output of git command"
  (let* ((output-buffer "*git-eshell-prompt")
         (output-status (call-process "git" nil output-buffer nil "rev-parse" "--abbrev-ref" "HEAD"))
         (current-branch (car (split-string (with-current-buffer output-buffer
                                              (buffer-substring (point-min) (point-max)))
                                            "\n"))))
    (kill-buffer output-buffer)
    (if (= output-status 0)
        current-branch
      output-status)))

(defun pedro-eprompt--git-status-verbose()
  "RETURN 0 IF WORKING TREE CLEAN, OTHERWISE RETURN A STRING LIST WITH STAGGED/UNTRACKED..."
  (let* ((output-buffer "*git-eshell-prompt")
         (output-status (call-process "git" nil output-buffer nil "status" "--porcelain"))
         (git-status (with-current-buffer output-buffer
                       (buffer-substring (point-min) (point-max))))
         (status-list (mapcar #'(lambda(s)
                                  (if (not (string-empty-p s))
                                      (substring s 0 2)))
                              (split-string git-status "\n")))
         (status-keys (delq nil (seq-uniq status-list)))
         (verbose-list (mapcar #'(lambda(k)
                                   (cons (seq-count #'(lambda(e)
                                                        (string= e k))
                                                    status-list)
                                         k))
                               status-keys)))

    (kill-buffer output-buffer)

    (mapconcat #'(lambda (e)
                   (concat
                    (propertize (number-to-string (car e)) 'face `(:foreground "#ffffff"))
                    (propertize (cdr e) 'face `(:foreground "#8b3e2f" :weight bold :underline t))))
               verbose-list
               "")))

(defun pedro-eprompt--shorten-path ()
  "SHORT THE PATH WHEN PATH LENGTH GREATER THAN `WINDOW-MAX-CHARS-PER-LINE' / 2"
  (let ((path (abbreviate-file-name (eshell/pwd))))
    (if (> (length path) (/ (window-max-chars-per-line) 2 )) ;; TODO max-size-path (?)
        (let* ((max-size-path (/ (window-max-chars-per-line) 2))
               (path-split (split-string (abbreviate-file-name (eshell/pwd)) "/"))
               (shorten-list (mapcar (lambda (dir)
                                       (if (string-empty-p dir )
                                           "/"
                                         (substring dir 0 1)))
                                     (butlast path-split))))
          (mapconcat (lambda (p)
                       (if (string= p "/") "" p))
                     (append shorten-list (last path-split))
                     "/"))
      path)))

(defun pedro-eprompt--git-status()
  "RETURN 0 IF WORKING TREE CLEAN, OTHERWISE 1."
  (let* ((output-buffer "*git-eshell-prompt")
         (output-status (call-process "git" nil output-buffer nil "status" "-s"))
         (git-status (with-current-buffer output-buffer
                       (buffer-substring (point-min) (point-max)))))
    (kill-buffer output-buffer)
    (if (string-empty-p git-status)
        0
      1)))

(defun pedro-eprompt-prompt-function ()
  (setq eshell-prompt-regexp "^$ ")
  (concat
   "\n"
   "# "                             ;TODO
   (propertize user-login-name 'face `(:foreground "#1f5582"))
   "@"
   (propertize system-name 'face `(:foreground "#9acd32" ))
   ": "
   (propertize (pedro-eprompt--shorten-path) 'face `(:foreground "#cd8500" :weight bold))
   (let ((output (pedro-eprompt--is-a-git-dir))
         (git-status (pedro-eprompt--git-status)))
     (if (numberp output)
         ""
       (concat (propertize " on git:" 'face `(:foreground "#ffffff"))
               (propertize output 'face `(:foreground "#1f5582")) " "
               (if (= git-status 0)
                   (propertize "o" 'face `(:foreground "#9acd32"))
                 ;;(propertize "x" 'face `(:foreground "#ff6347"))))))
                 (pedro-eprompt--git-status-verbose)))))
   " "
   (let ((output eshell-last-command-status)
         (current-time (propertize (format-time-string "[%-H:%M:%S]") 'face `(:foreground "#ffffff" ))))
     (if  (= output 0)
         current-time
       (concat current-time  " C:" (propertize (number-to-string output) 'face `(:foreground "#ff6347")))))
   "\n"
   (propertize "$" 'face `(:foreground "#ff6347"))
   " "))

(setq eshell-prompt-function #'pedro-eprompt-prompt-function)

;; eshell history backup
(defvar pedro-eshell-hist-bkp-size 200000 "Size in KBytes")
(defvar pedro-eshell-hist-bkp-compress-command "bzip2")
(defvar pedro-eshell-hist-bkp-compress-arguments "-c")
(defvar pedro-eshell-hist-bkp-file-extension "bz2")


(defun pedro-eshell-hist-bkp-make-it? ()
  (when (> (file-attribute-size (file-attributes eshell-history-file-name))
           pedro-eshell-hist-bkp-size)
    (pedro-eshell-hist-bkp)))


(defun pedro-eshell-hist-bkp ()
  "Create an eshell history backup with date and backup number and
compress it."
  (interactive)
  (let* ((dir-name (file-name-directory eshell-history-file-name))
         (file-name (file-name-nondirectory eshell-history-file-name))
         (current-date (format-time-string "%Y%m%d%H%M%S"))
         (bkp-numbers (mapcar (lambda (f)
                                (and-let* ((n (string-to-number (file-name-extension (file-name-sans-extension f))))
                                           (numberp n))))
                              (directory-files dir-name nil (concat file-name "\\..*\\.[0-9]+\\.bz2"))))
         (new-bkp-number (if (null bkp-numbers)
                             0
                           (1+ (apply 'max bkp-numbers))))
         (compressed-file-name (concat dir-name file-name "."
                                       current-date "."
                                       (int-to-string new-bkp-number))))
    (shell-command
     (concat pedro-eshell-hist-bkp-compress-command " "
             pedro-eshell-hist-bkp-compress-arguments " "
             eshell-history-file-name
             ">"
             compressed-file-name "."
             pedro-eshell-hist-bkp-file-extension))))

(add-hook 'eshell-load-hook 'pedro-eshell-hist-bkp-make-it?)
