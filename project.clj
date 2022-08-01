(defproject net.cgrand/xforms "0.19.3"
  :description "Extra transducers for Clojure"
  :url "https://github.com/cgrand/xforms"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:project]}

  :profiles
  {:provided
   {:dependencies [[org.clojure/clojure "1.8.0"]
                   [org.clojure/clojurescript "1.9.293"]]}})
