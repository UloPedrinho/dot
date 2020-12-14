;;;;; dictionaries
(defhydra hydra-dictionary ()
  "dict"
  ("d" dictionary-search "dict")
  ("a" google-translate-at-point "google at point")
  ("q" google-translate-query-translate "google query ")
  ("S" helm-wordnet-suggest "wordnet suggest")
  ("e" (lambda () (interactive)
         (eww
          (concat "http://www.wordreference.com/es/translation.asp?tranword=" (read-string "eng-esp: "))))
   "wr eng-esp")
  ("s" (lambda () (interactive)
         (eww
          (concat "http://www.wordreference.com/es/translation.asp?tranword=" (read-string "esp-eng: "))))
   "wr esp-eng")
  ("g" pedro-gal-sin "sinónimos"))
;;;;; folding
(defhydra hydra-folding ()
  "folding"
  ("C" origami-close-all-nodes "close all")
  ("O" origami-open-all-nodes "open all")
  ("c" origami-close-node "close")
  ("o" origami-open-node "open")
  )
;;;;; emms
(defhydra hydra-emms (:hint nil) ; (:exit t)
  "
%s(cdr (assoc 'state (pedro-emms-get-current-song-info))): %s(cdr (assoc 'title (pedro-emms-get-current-song-info))) | %s(cdr (assoc 'time (pedro-emms-get-current-song-info)))
-----------------------------------------------------------
^Control^              ^Seek^       ^Volume[%s(car (split-string (cdr (assoc 'volume (pedro-emms-get-current-song-info))) \"%\"))]^    ^Modes^
^^^^^^^^^-----------------------------------------------------------
_<SPC>_: Play/Pause    _h_: -10s    _-_: -5         _R_: Repeat  [%s(cdr (assoc 'repeat (pedro-emms-get-current-song-info)))]
    _p_: Previous      _l_: +10s    _+_: +5         _S_: Shuffle [%s(cdr (assoc 'random (pedro-emms-get-current-song-info)))]
    _n_: Next          _j_:  -1m                  _I_: Single  [%s(cdr (assoc 'single (pedro-emms-get-current-song-info)))]
    _s_: Stop          _k_:  +1m
-----------------------------------------------------------
_b_: Browser  _q_: Quit
"
  ("b"  emms-smart-browse  :exit t)
  ("n"  emms-next)
  ("p"  emms-previous)
  ("<SPC>"  emms-pause)
  ("s" emms-player-mpd-stop)
  ("-" (lambda () (interactive)
         (call-process "mpc" nil nil nil "volume" "-5")))
  ("+" (lambda () (interactive)
         (call-process "mpc" nil nil nil "volume" "+5")))
  ("m" (lambda () (interactive)
         (let ((current-volume (string-to-number (car (split-string (cdr (assoc 'volume (pedro-emms-get-current-song-info))) "%")))))

           (if (= current-volume 0)

               (call-process "mpc" nil nil nil "volume" (or (number-to-string pedro-emms-mute-old-volume) 30))
             (progn
               ;; (shell-command "mpc volume 0" nil nil)
               (call-process "mpc" nil nil nil "volume" "0")
               (setq pedro-emms-mute-old-volume current-volume))))))
  ("h" (lambda () (interactive)
         (shell-command "mpc seek -10 ")
         (message "%s" (cdr (assoc 'time (pedro-emms-get-current-song-info))))))
  ("l" (lambda () (interactive)
         (shell-command "mpc seek +10 ")
         (message "%s" (cdr (assoc 'time (pedro-emms-get-current-song-info))))))
  ("j" (lambda () (interactive)
         (shell-command "mpc seek -60 ")
         (message "%s" (cdr (assoc 'time (pedro-emms-get-current-song-info))))))
  ("k" (lambda () (interactive)
         (shell-command "mpc seek +60 ")
         (message "%s" (cdr (assoc 'time (pedro-emms-get-current-song-info))))))
  ("S" (lambda () (interactive)
         (if (string= "on" (cdr (assoc 'random (pedro-emms-get-current-song-info))))
             (shell-command "mpc random off")
           (shell-command "mpc random on"))
         (message "Shuffle: %s" (cdr (assoc 'random (pedro-emms-get-current-song-info))))))
  ("R" (lambda () (interactive)
         (if (string= "on" (cdr (assoc 'repeat (pedro-emms-get-current-song-info))))
             (shell-command "mpc repeat off")
           (shell-command "mpc repeat on"))
         (message "Repeat: %s" (cdr (assoc 'repeat (pedro-emms-get-current-song-info))))))
  ("I" (lambda () (interactive)
         (if (string= "on" (cdr (assoc 'single (pedro-emms-get-current-song-info))))
             (shell-command "mpc single off")
           (shell-command "mpc single on"))
         (message "Single: %s" (cdr (assoc 'single (pedro-emms-get-current-song-info))))))
  ("q" nil  :color blue)
  ("P" (hydra-emms-playlists/body) :exit t))

(defhydra hydra-emms-playlists (:columns 5)
  ("g" emms-metaplaylist-mode-go "go")
  ("n" emms-playlist-new "new")
  ("s" emms-playlist-save "save")
  ("l" emms-playlist-mode-open-buffer "load")
  ("p" emms-play-playlist "play")
  ("q" nil "quit" :color blue))



;;;;; go
(defhydra hydra-go (:exit t)
  "go"
  ("e" (lambda () (interactive)
	 (eshell current-prefix-arg)) "eshell")
  ("r" (lambda () (interactive)
	 (let ((buffer (get-buffer "*elfeed-search*")))
	   (if buffer
	       (switch-to-buffer buffer)
	       (elfeed)))) "rss")
  ("s" (lambda () "switch to *scratch* buffer" (interactive) (switch-to-buffer "*scratch*" )) "*scratch*")
  ("p" (lambda () (interactive)
         (let ((buffer (get-buffer "pomodoro.org")))
           (if buffer
               (switch-to-buffer buffer)
               (find-file (concat "~/org/pomodoro.org"))))) "pomo")
  ("c" (lambda () (interactive) (find-file "~/org/computer/emacs/emacs-cheatsheet.org")) "cheat-sheet")
  ("b" (lambda () (interactive)
         (if (get-buffer "*org-brain*")
             (switch-to-buffer "*org-brain*")
             (progn
               (setq current-prefix-arg '(4))
               (call-interactively 'org-brain-visualize)))) "brain")
  ("t" (lambda () (interactive)
         (let ((buffer (get-buffer "today.org")))
           (if buffer
               (switch-to-buffer buffer)
             (find-file (concat "~/org/axenda/gtd/today.org"))))) "today")
  ("m" pedro-bmkp-main "main bmk")
  ("j" (org-journal-new-entry t) "journal"))
;;;;; outline
;; https://github.com/abo-abo/hydra/wiki/Emacs
(defhydra hydra-outline (:color pink :hint nil)
  "
^Hide^             ^Show^           ^Move^                    ^Others
^^^^^^------------------------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up                   _y_: edit as org
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
  ;; ** Hide
  ("q" hide-sublevels)    ; Hide everything but the top-level headings
  ("t" hide-body)         ; Hide everything but headings (all body lines)
  ("o" hide-other)        ; Hide other branches
  ("c" hide-entry)        ; Hide this entry's body
  ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; ** Show
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)     ; Show this heading's immediate child sub-headings
  ("k" show-branches)     ; Show all sub-headings under this heading
  ("s" show-subtree)      ; Show (expand) everything in this heading & below
  ;; *** Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("y" outorg-edit-as-org)

  ("z" nil "leave"))

;;;;; org-mode
(defhydra hydra-org (:hint nil :color pink)
  "
^Cycle & Show^              Motion                         Mark
-------------------------------------------------------------------------
_UUU<tab>_: show all        _n_: next visible heading      _Mh_: element
_U<tab>_:   global cycle    _p_: prev.. visible heading    _@_:  subtree
_r_:   reveal               _f_: next same level heading
_k_:   show branches        _b_: prev.. same level heading
_i_:   indirect buf..       _u_: up to higher level
                          _j_: go to
"
  ("UUU<tab>" outline-show-all)
  ("U<tab>" org-global-cycle)
  ("r" org-reveal )
  ("k" outline-show-branches )
  ("i" org-tree-to-indirect-buffer )
  ("n" org-next-visible-heading)
  ("p" org-previous-visible-heading)
  ("f" org-forward-heading-same-level)
  ("b" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("j" org-goto)
  ("Mh" org-mark-element)
  ("@" org-mark-subtree)
  ("q" nil "quit" :color blue))

;;;;; persp-mode
(defhydra hydra-persp (:columns 4 :color blue)
    "Perspective"
    ("a" persp-add-buffer "Add Buffer")
    ("i" persp-import "Import")
    ("c" persp-kill "Close")
    ("n" persp-next "Next")
    ("p" persp-prev "Prev")
    ("k" persp-remove-buffer "Kill Buffer")
    ("r" persp-rename "Rename")
    ("A" persp-set-buffer "Set Buffer")
    ("s" persp-switch "Switch")
    ("C-x" persp-switch-last "Switch Last")
    ("b" persp-switch-to-buffer "Switch to Buffer")
    ("P" projectile-persp-switch-project "Switch Project")
    ("q" nil "Quit"))

;;;;; themes
(defhydra hydra-themes ()
  ("e" (pif/load-theme 'eink)    "e-ink")
  ("sd" (pif/load-theme 'solarized-dark) "solar.. dark")
  ("sl" (pif/load-theme 'solarized-light) "solar.. light")
  ;; ("ds" (pif/load-theme 'doom-spacegrey) "doom spacegrey")
  ("z" (pif/load-theme 'zerodark) "zerodark"))

;;;;; windows
(defhydra hydra-windows (:hint nil)
  "
^Resize^
--------------
  _k_
_h_   _l_
  _j_
"
  ("h" (shrink-window-horizontally 1))
  ("l" (shrink-window-horizontally -1))
  ("k" (enlarge-window 1))
  ("j" (enlarge-window -1)))

;;;;; frames
(defhydra hydra-frames (:hint nil :exit t)
  "frames"
  ("i" iconify-or-deiconify-frame "iconify")
  ;; FIXME works "only" with one iconify frame
  ("r" (lambda () (interactive)
         (raise-frame (car (seq-difference (frame-list) (visible-frame-list))))) "raise"))


;;;;; sandboxes
(defhydra hydra-sandboxes (:hint nil :exit t)
  "Sandboxes"
  ("g" (lambda () (interactive)
         (let* ((sandbox-org-path "/Users/pedro/dev/computer/emacs/init-sandbox/boxes/gnus/gnus-sandbox.org")
                (sandbox-org-buffer (find-file-noselect sandbox-org-path))
                (org-confirm-babel-evaluate nil))
           (save-excursion
             (set-buffer sandbox-org-buffer)
             (org-babel-goto-named-src-block "launch")
             (org-babel-execute-src-block)
             (kill-buffer sandbox-org-buffer)))) "gnus")
  ("c" (lambda () (interactive)
         (let* ((sandbox-org-path "/Volumes/dev/dev/computer/emacs/init-sandbox/boxes/clojure/clojure/clojure-sandbox.org")

                (sandbox-org-buffer (find-file-noselect sandbox-org-path))
                (org-confirm-babel-evaluate nil))
           (save-excursion
             (set-buffer sandbox-org-buffer)
             (org-babel-goto-named-src-block "launch")
             (org-babel-execute-src-block)
             (kill-buffer sandbox-org-buffer)))) "clojure")
("r" (lambda () (interactive)
         (let* ((sandbox-org-path "/Volumes/dev/dev/computer/emacs/init-sandbox/boxes/R/R-sandbox.org")
                (sandbox-org-buffer (find-file-noselect sandbox-org-path))
                (org-confirm-babel-evaluate nil))
           (save-excursion
             (set-buffer sandbox-org-buffer)
             (org-babel-goto-named-src-block "launch")
             (org-babel-execute-src-block)
             (kill-buffer sandbox-org-buffer)))) "R")
("s" (lambda () (interactive)
         (let* ((sandbox-org-path "/Volumes/dev/dev/computer/emacs/init-sandbox/boxes/scheme/scheme-sandbox.org")
                (sandbox-org-buffer (find-file-noselect sandbox-org-path))
                (org-confirm-babel-evaluate nil))
           (save-excursion
             (set-buffer sandbox-org-buffer)
             (org-babel-goto-named-src-block "launch")
             (org-babel-execute-src-block)
             (kill-buffer sandbox-org-buffer)))) "scheme")
  ("m" (lambda () (interactive)
         (let* ((sandbox-org-path "/Volumes/dev/dev/computer/emacs/init-sandbox/boxes/mandarin/mandarin-sandbox.org")

                (sandbox-org-buffer (find-file-noselect sandbox-org-path))
                (org-confirm-babel-evaluate nil))
           (save-excursion
             (set-buffer sandbox-org-buffer)
             (org-babel-goto-named-src-block "launch")
             (org-babel-execute-src-block)
             (kill-buffer sandbox-org-buffer)))) "mandarin"))


(defhydra hydra-buffers (:hint nil :exit nil)
  "
^move^     ^resize^    ^transpose^
----     ------    ---------
_h_ _k_ _j_ _l_  _H_ _K_ _J_ _L_   _a_: horizontal  _f_: vertical _s_: rotate clockwise  _d_: rotate anti-clockwise"
              ("q" nil)
              ("h" buf-move-left  :color red)
              ("k" buf-move-up    :color red)
              ("j" buf-move-down  :color red)
              ("l" buf-move-right :color red)
              ("L" (shrink-window-horizontally 1))
              ("H" (shrink-window-horizontally -1))
              ("K" (enlarge-window 1))
              ("J" (enlarge-window -1))
              ("" transpose-frame)
              ("f" flip-frame :color blue)
              ("a" flop-frame :color blue)
              ("" rotate-frame)
              ("s" rotate-frame-clockwise)
              ("d" rotate-frame-anticlockwise))


;; chronos
(defhydra hydra-chronos (:hint nil :exit t)
  "
_C_: custom timer  _a_: 15m
_q_: quit          _b_: 30m
                 _c_: 1h
"
  ("a" (lambda (u) (interactive "p") (pedro-my-chronos-repeat "15m" "15m!" (* 1 u))))
  ("b" (lambda (u) (interactive "p") (pedro-my-chronos-repeat "30m" "30m!" (* 1 u))))
  ("c" (lambda (u) (interactive "p") (pedro-my-chronos-repeat "1h" "1h!" (* 1 u))))
  ("C" chronos-add-timer)
  ("q" nil))

;;;;; various to test
(defhydra hydra-testing (:columns 5 :exit t :hint nil)
  "testing"
  ;; ("F" feebleline-mode "feebleline")
  ("W" writeroom-mode "writeroom")
  ("t" ztree-dir "ztree")
  ;; ("s" treemacs "treemacs")
  ("a" helm-do-ag "helm-do-ag")
  ("g" git-timemachine "timemachine")
  ("z" zoom-mode "zoom")
  ("c" clipmon-autoinsert-toggle "clipmon")
  ("T" (hydra-themes/body) "themes")
  ("m" menu-bar-open "menu")
  ("w" (hydra-windows/body) "windows")
  ("f" (hydra-frames/body) "frames")
  ("h" hl-line-mode "hl-line")
  ("o" dired-omit-mode "dired-omit-mode")
  ("b" counsel-ffdata-firefox-bookmarks "firefox bookmarks"))
