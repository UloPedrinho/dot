{:user  {:jvm-opts ^:replace ["-Xmx3G"]
         :plugins [[refactor-nrepl "2.4.0"]
                   [cider/cider-nrepl "0.18.0"]
                   [lein-ancient "0.6.15"]
                   ;;[jonase/eastwood "0.3.3"]
                   [lein-pprint "1.2.0"]
                   ;;[lein-try "0.4.3"]
                   [lein-cloverage "1.0.13"]]
         ;; :middleware [cider-nrepl.plugin/middleware
         ;;              refactor-nrepl.plugin/middleware]
         :dependencies [[nrepl "0.5.3"]
                        [org.clojure/tools.namespace "0.2.11"]]}}

