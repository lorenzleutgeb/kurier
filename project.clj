(defproject kurier "0.1.0-SNAPSHOT"
  :description "Facebook Messenger Bot in Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.1"]
                 [compojure "1.5.1"]
                 [http-kit "2.2.0"]
                 [ring/ring-defaults "0.2.1"]
                 [ring/ring-json "0.4.0"]
                 [ring/ring-jetty-adapter "1.5.0"]
                 [environ "1.1.0"]
                 [clj-time "0.13.0"]
                 [clojure-term-colors "0.1.0-SNAPSHOT"]]
  :min-lein-version "2.0.0"
  :plugins [[lein-ring "0.9.7"]
            [lein-environ "1.1.0"]]
  ;:hooks [environ.leiningen.hooks]
  :ring {:handler kurier.web/app}
  :main kurier.repl
  :aot [kurier.web kurier.repl]
  :uberjar-name "kurier-standalone.jar")
  ; :profiles {:default [:base :dev :user]
  ;            #_:production #_{:env {:production false}}})
