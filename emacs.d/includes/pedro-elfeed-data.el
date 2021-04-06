(setq elfeed-feeds
      '(
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" emacs)
        ("https://hnrss.org/newest?q=emacs" emacs)
        ("https://hnrss.org/newest?q=org-mode" emacs)
        ("https://hnrss.org/newest?q=clojure" clojure)
        ;;("https://hnrss.org/newest" hacker)
        ("https://todayinclojure.com/rss" clojure)
        ("http://novafantasia.com/feed" sci-fi)
        ("http://culturagalega.gal/fluxos2.php" galiza)
;; 	"http://www.seriecanal.com/rss.php"
;; 	"http://www.eldiario.es/rss/"
;; 	"http://esmateria.com/feed/"
 	("http://apod.nasa.gov/apod.rss" space)
	("https://www.masteringemacs.org/feed" emacs)
;;	("http://planet.emacsen.org/atom.xml" emacs)
;; 	"http://clavicembalo.com/rss/"
;; 	"http://www.lugo.es/cs/ContentServer?pagename=Spark/Spark_Novas/PlantNoticiasRSS&c=Spark_Novas"
;; 	"http://www.lugo.es/cs/ContentServer?pagename=Spark/Spark_Eventos/PlantEventosRSS&c=Spark_Eventos"
;; 	"http://praza.gal/rss/novas/"
	("http://emacsredux.com/atom.xml" emacs)
;; 	;; "http://www.animenewsnetwork.com/newsroom/rss.xml"
;; 	"https://diccionarioeconomico.wordpress.com/feed/"
 	("https://www.space.com/feeds/all" space)
;; 	"http://feeds.feedburner.com/universetoday/pYdq"
;; 	"https://www.sciencenews.org/feeds/headlines.rss"
;; 	"http://www.esa.int/rssfeed/TopNews"
	("http://emacs.stackexchange.com/feeds" emacs)
	"https://www.reddit.com/login.rss?dest=http%3A%2F%2Fwww.reddit.com%2Fmessage%2Finbox%2F.rss%3Ffeed%3De2143400c82f187a4846f258260ba7ddc0dfd67e"
	"https://www.reddit.com/login.rss?dest=http%3A%2F%2Fwww.reddit.com%2Fmessage%2Finbox%2F.rss%3Ffeed%3De2143400c82f187a4846f258260ba7ddc0dfd67e%26user%3Dogalego"
;; 	"http://feeds2.feedburner.com/NewHorizonsHeadlines"
;; 	"http://www.jpl.nasa.gov/multimedia/rss/news.xml"
	"https://www.reddit.com/.rss?feed=cf103585ceaba3eabc6850ae08259eec2d64c374&user=ogalego"
	"https://www.reddit.com/user/ogalego/saved.rss?feed=cf103585ceaba3eabc6850ae08259eec2d64c374&user=ogalego"
	"https://www.reddit.com/user/ogalego/upvoted.rss?feed=cf103585ceaba3eabc6850ae08259eec2d64c374&user=ogalego"
	"https://www.reddit.com/message/inbox/.rss?feed=cf103585ceaba3eabc6850ae08259eec2d64c374&user=ogalego"
	"http://emacsformacosx.com/atom/release"
;; 	"http://hopwag.podbean.com/feed"
;; 	;; "http://www.dramabeans.com/feed/"
;; 	"http://www.gciencia.com/feed/"
;; 	;; "http://stackoverflow.com/feeds/tag/html5"
;; 	"https://avistaz.to/rss/feed?fid=2918&pid=c738af0266f810b49c7c2cffb6d3edca"
;;         "https://avistaz.to/rss/feed?fid=5114&pid=c738af0266f810b49c7c2cffb6d3edca"))
))

;; ;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :before "2 weeks ago" :remove 'unread))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "ycombinator\\.com" :add '(emacs)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "todayinclojure\\.com" :add '(clojure)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/Guitar" :add '(guitar music r)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "clavicembalo\\.com"  :add '(clavi lecer music)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "seriecanal\\.com"  :add '(video series lecer) :remove 'unread))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "apod\\.nasa\\.gov"  :add '(space apod)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "jpl\\.nasa\\.gov"  :add '(space apod)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/emacs"  :add '(emacs r computer)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/orgmode"  :add '(emacs r computer)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/lisp"  :add '(lisp computer r)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/Common_Lisp"  :add '(lisp computer r)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/musictheory"  :add '(music theory learn))) ;
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/guitarlessons"  :add '(music guitar theory learn)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/Guitar_Theory"  :add '(music guitar theory learn)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/science"  :add '(science r)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/todayilearned"  :add '(til r)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com/r/worldnews"  :add '(news world r)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com\\/r/philosophy"  :add '(philosophy r)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com\\/r/space"  :add '(space r)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com\\/r/askscience"  :add '(science r)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com\\/r/gadgets"  :add '(gadgets r)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com\\/r/asm"  :add '(asm r computer)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com\\/r/scifi"  :add '(r  sci-fi)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com\\/r/Clojure"  :add '(r clojure)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "reddit\\.com\\/r/Gintama"  :add '(r gintama)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "universetoday\\.com"  :add '(astronomy space)))
;; ;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "dramabeans\\.com"  :add '(lecer doramas video)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "avistaz\\.to"  :add '(lecer doramas video)))
;; ;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "animenewsnetwork\\.com"  :add '(anime video)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "sciencenews\\.org" :add '(science)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "feedproxy\\.google\\.com/~r/materia"  :add '(science)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "feedproxy\\.google\\.com/~r/Newhorizonsheadlines"  :add '(news space pluto)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "novafantasia\\.com" :add '(fantasy sci-fi)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "space\\.com"  :add '(space)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "esa\\.int"  :add '(astronomy space esa)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "praza\\.gal"  :add '(galiza news)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "emacs\\.stackexchange\\.com" :add '(emacs computer stackexchange)))
;; ;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "stackoverflow\\.com/feeds/tag/html5" :add '(html5 computer stackoverflow)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "emacsninja\\.com" :add '(emacs computer)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "irreal\\.org" :add '(emacs computer)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "eldiario\\.es" :add '(news)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "planet\\.emacsen\\.org" :add '(emacs computer)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "hopwag\\.podbean\\.com" :add '(philosophy learn)))
;; (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "gciencia\\.com" :add '(science galiza)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "emacsformacosx\\.com" :add '(emacs computer mac)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "youtube\\.com/c/SystemCrafters" :add '(emacs computer)))
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :entry-link "culturagalega\\.gal/fluxos2\\.php" :add '(galizaq)))
