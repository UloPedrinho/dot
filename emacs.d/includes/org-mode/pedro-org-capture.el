;; org-capture
(setq org-default-notes-file "~/org/notes.org")
(setq org-capture-bookmark nil)
(define-key global-map "\C-cc" 'org-capture)

;; user functions
(defun pedro/org-capture-drill-word (word)
  "" ;TODO:
  (interactive "MWord: ")
  (let* ((url-wr-translation (concat "http://www.wordreference.com/es/translation.asp?tranword=" word))
         (url-wr-definition (concat "http://www.wordreference.com/definition/" word))
         (url-wr-synonymous (concat "http://www.wordreference.com/synonyms/" word))
         (buffer-wr-translation-html (url-retrieve-synchronously url-wr-translation))
         (buffer-wr-definition-html (url-retrieve-synchronously url-wr-definition))
         (buffer-wr-synonymous-html (url-retrieve-synchronously url-wr-synonymous))
         definition
         pronunciation
         translations
         examples
         synonymous)
    (save-excursion
     (set-buffer buffer-wr-translation-html)
     (setq pronunciation (dom-text (dom-by-id
                                    (libxml-parse-html-region
                                     (point-min) (point-max))
                                    "pronWR")))
     (goto-char (point-min))
     (setq translations  (let* ((start (goto-char (- (search-forward "<table class='WRD'") (length "<table class='WRD'"))))
                                (end (progn (sgml-skip-tag-forward 1) (point)))
                                (data (dom-by-class (libxml-parse-html-region start end) "ToWrd" )))
                           (cdr (seq-uniq (mapcar (lambda (n)
                                                    (dom-text n))
                                                  (car (dom-by-tag data 'td)))))))
     (goto-char (point-min))
     (setq examples (let* ((start (goto-char (- (search-forward "<table class='WRD'") (length "<table class='WRD'"))))
                           (end (progn (sgml-skip-tag-forward 1) (point)))
                           (data (dom-by-class (libxml-parse-html-region start end) "FrEx")))
                      (mapcar (lambda (n)
                                (dom-text n))
                              data)))
     (save-excursion
     ;; Collins Concise English Dictionarity
     (set-buffer buffer-wr-definition-html)
     (setq definition  (mapcar (lambda (n)
                                 (dom-text n))
                               (dom-by-class (libxml-parse-html-region (point-min) (point-max)) "definition"))))

    (save-excursion
     (set-buffer buffer-wr-synonymous-html)
     (setq synonymous (delq "" (mapcar (lambda (n)
                                         (dom-text n))
                                       (dom-by-class (libxml-parse-html-region (point-min) (point-max)) "syno" )))))

    ;;
    (with-temp-buffer
        (insert "*** word   :drill:" "\n" word)
      (insert "\n")
      (insert "**** pronunciation" "\n" pronunciation)
      (insert "\n")
      (insert "**** meaning" "\n")
      (dotimes (m (if (< (length definition) 10)
                      (length definition)
                      10))
        (insert (int-to-string (+ 1 m)) ". " (nth m definition ) "\n"))
      (insert "**** synonymous" "\n")
      (dolist (m synonymous)
        (insert m ", "))
      (delete-char -2)
      (insert "\n")
      (insert "**** translations" "\n")
      (dolist (m translations)
        (insert m ", "))
      (delete-char -2)
      (insert "\n")
      (insert "**** examples" "\n")
      (dolist (m examples)
        (insert "- " m "\n"))
      (buffer-string)))))

;; capture templates
(setq org-capture-templates
      '(("a" "Axenda" entry (file+headline "~/org/axenda/axenda.org" "diario") "* %?\n")

        ("t" "TODO")
        ("tq" "Quick" entry (file+headline "~/org/notes.org" "Quick") "* TODO %?\n%U")
	("tc" "Computer")
        ("tce" "Emacs" entry (file+olp "~/org/notes.org" "Computer" "Emacs") "* TODO %?\n%U")
        ("tcl" "Learn" entry (file+olp "~/org/notes.org" "Computer" "Learn") "* TODO %?\n%U")
        ("tcr" "Read" entry (file+olp "~/org/notes.org" "Computer" "Read") "* TODO %?\n%U")
        ("tco" "Others" entry (file+olp "~/org/notes.org" "Computer" "Others") "* TODO %?\n%U")
	("tp" "Personal")
        ("tpt" "Tasks" entry (file+olp "~/org/notes.org" "Personal" "Blog") "* TODO %?\n%U")
        ("tpb" "Blog" entry (file+olp "~/org/notes.org" "Personal" "Blog") "* TODO %?\n%U")
        ("tpl" "Learn" entry (file+olp "~/org/notes.org" "Personal" "Learn") "* TODO %?\n%U")
	("tpm" "Music" entry (file+olp "~/org/notes.org" "Personal" "Music") "* TODO %?\n%U")
        ("tpo" "Ocio" entry (file+olp "~/org/notes.org" "Personal" "Ocio") "* TODO %?\n%U")
        ("tpr" "Read" entry (file+olp "~/org/notes.org" "Personal" "Read")"* TODO %?\n%U")
        ("tph" "Others" entry (file+olp "~/org/notes.org" "Personal" "Others") "* TODO %?\n%U")


	("b" "Bookmarks")
	("bt" "TODO" item (file+olp "~/org/bookmarks.org" "TODO"))
	("bA" "anime" item (file+olp "~/org/bookmarks.org" "anime"))
	("ba" "Astronomy" item (file+olp "~/org/bookmarks.org" "astronomy"))
	("be" "Emacs" item (file+olp "~/org/bookmarks.org" "computador" "emacs"))
	("bJ" "java" item (file+olp "~/org/bookmarks.org" "computador" "java"))
	("bj" "javascript" item (file+olp "~/org/bookmarks.org" "computador" "javascript"))
	("bl" "lisp" item (file+olp "~/org/bookmarks.org" "computador" "lisp"))
	("bn" "nihongo" item (file+olp "~/org/bookmarks.org" "linguas" "nihongo"))
	("bm" "music" item (file+olp "~/org/bookmarks.org" "music"))
	("bs" "Science" item (file+olp "~/org/bookmarks.org" "science"))

	;; Text
	("e" "Text")
	("ei" "Idea" entry (file+olp "~/dev/text/text.org" "ideas") "** IDEA \n%U\n%?")
	("ep" "Phrases" item (file+headline "~/dev/text/text.org" "frases") "- %U\n  #+BEGIN_EXAMPLE\n  %?\n  #+END_EXAMPLE")
	;;("es" "Spacial Nhape" checkitem (file+olp "~/dev/text/correo/spacial_nhape/spacial_nhape.org" "Ideas"))

	;; Lists
	("l" "Lists")
	("la" "Anime" table-line (file+headline "~/org/leisure/anime.org" "[Rematados]") "|%?||||%u||||")
	("ld" "Doramas" table-line (file+headline "~/org/leisure/teleseries.txt" "ended") "|%?||||||%u|")
	("lm" "Music" item (file+olp "~/org/leisure/lists.org" "music" "to sort"))

	;; Various
	("v" "Various")
	;; ("vc" "contas" table-line (file+headline "~/org/varios/contas.org" "contas") "|%u|%?||||||")

	;; org-drill
	("d" "Drill")
	("dw" "word" plain (file+olp "~/org/learn/drill.org" "vocabulary" "english") "%(call-interactively #'pedro/org-capture-drill-word)")
	("dp" "palabra" plain (file+olp "~/org/learn/drill.org" "vocabulary" "español") "palabra%?")
	("dd" "definition" plain (file+olp "~/org/learn/drill.org" "definitions" "astronomy") "item%?")
	("dt" "TODO" plain (file+headline "~/org/learn/drill.org" "TODO") "- [ ] %? %U")

	;; ???? FIXME
	("c" "Random log" item (file "~/org/axenda/bitacora.org") "%T %?")
	;;("P" "Pensamentos" item (file+olp "~/dev/text/text.org" "pensamentos") "%T\n%?")

        ;; org-capture & org-protocol
        ("p" "Protocol" entry (file+headline "~/org/bookmarks.org" "Firefox")
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry (file+headline "~/org/bookmarks.org"  "Firefox")
         "* %? [[%:link][%:description]] %(progn (setq kk/delete-frame-after-capture 2) \"\")\nCaptured On: %U"
         :empty-lines 1)

))
