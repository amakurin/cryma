(defproject net.cryma/cryma "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :jvm-opts ["-Xms512m" "-Xmx1024m"]
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"my.datomic.com" {:url "https://my.datomic.com/repo"
                                   :creds :gpg}}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.36"]
                 [org.clojure/core.async "0.2.382"]

                 [weasel "0.7.0" :exclusions [org.clojure/clojurescript]]

                 [org.immutant/web "2.1.4"
                  :exclusions [;org.jboss.logging/jboss-logging
                               org.jboss.logging/jboss-logging-spi]]

                 [com.stuartsierra/component "0.3.1"]

                 [compojure "1.3.3"]
                 [com.cemerick/url "0.1.1"]
                 [bidi "2.0.9"]
                 [ring/ring-core "1.3.2"]
                 [ring/ring-anti-forgery "1.0.0"]
                 [ring/ring-devel "1.3.2"]
                 [ring-mock "0.1.5"]
                 [com.taoensso/sente "1.9.0-beta2"]

                 [environ "1.0.0"]
                 [hiccup "1.0.5"]

                 [clj-http "3.1.0"]
                 [cheshire "5.6.1"]

                 [clj.qrgen "0.4.0"]
                 ;[bitcoinj/bitcoinj "0.14.3"]
                 [org.bitcoinj/bitcoinj-core "0.14.3"]

                 [rum "0.9.0" :exclusions [[cljsjs/react]]]
                 [cljsjs/react-with-addons "15.1.0-0"]
                 [hickory "0.6.0"]
                 [org.mindrot/jbcrypt "0.3m"]
                 [crypto-random "1.2.0"]
                 ;[com.datomic/datomic-free "0.9.5344" :exclusions [[com.google.guava/guava]]]
                 [com.datomic/datomic-pro "0.9.5385" :exclusions [[com.google.guava/guava]]]
                 [org.clojure/java.jdbc "0.6.1"]
                 ]
  :plugins [[lein-localrepo "0.5.3"]
            [lein-cljsbuild "1.1.3"]
            [lein-environ "1.0.0"]]
  :cljsbuild {:builds [{:id           "dev"
                        :source-paths ["srccljs"]
                        :compiler     {:optimizations    :none
                                       ;:main "cryma.cryma"
                                       :pretty-print     true
                                       :output-to        "resources/public/js/dev.js"
                                       :output-dir       "resources/public/js/cljs-dev-out"
                                       :closure-warnings {:non-standard-jsdoc :off}
                                       ;:source-map       "resources/public/js/dev.map"
                                       }}
                       {:id           "dev1"
                        :source-paths ["srccljs"]
                        :compiler     {:optimizations    :whitespace
                                       :main             "cryma.cryma"
                                       :pretty-print     true
                                       :output-to        "resources/public/js/dev.js"
                                       :output-dir       "resources/public/js/cljs-dev1-out"
                                       :closure-warnings {:non-standard-jsdoc :off}
                                       ;:source-map "resources/public/js/dev.map"
                                       }}
                       {:id "prod"
                        :source-paths ["srccljs"]
                        :compiler {
                                   :output-to "resources/public/js/prod.js"
                                   :optimizations :advanced
                                   :pretty-print false
                                   :closure-warnings {:non-standard-jsdoc :off}}}
                       ]}
  :source-paths ["src" "srccljs"]
  :main cryma.cryma)
