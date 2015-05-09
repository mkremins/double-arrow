(defproject mkremins/double-arrow "0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3211"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.omcljs/om "0.8.8"]
                 [prismatic/om-tools "0.3.11"]]
  :plugins [[lein-cljsbuild "1.0.5"]]
  :cljsbuild {:builds [{:id "app"
                        :source-paths ["src"]
                        :compiler {:main double-arrow.app
                                   :optimizations :none
                                   :output-dir "target/app"
                                   :output-to "target/app.js"
                                   :source-map true}}]})
