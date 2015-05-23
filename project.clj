(defproject cats "0.4.0"
  :description "Category Theory abstractions for Clojure"
  :url "https://github.com/funcool/cats"
  :license {:name "BSD (2 Clause)"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies []
  :deploy-repositories {"releases" :clojars
                        "snapshots" :clojars}

  :source-paths ["src"]
  :test-paths ["test"]

  :cljsbuild {:test-commands {"test" ["node" "output/tests.js"]}
              :builds [{:id "test"
                        :source-paths ["src" "test"]
                        :notify-command ["node" "output/tests.js"]
                        :compiler {:output-to "output/tests.js"
                                   :output-dir "output"
                                   :source-map true
                                   :static-fns true
                                   :cache-analysis false
                                   :main cats.testrunner
                                   :optimizations :none
                                   :target :nodejs
                                   :pretty-print true}}]}

  :jar-exclusions [#"\.swp|\.swo"]

  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.10"]
                                  [org.clojure/clojure "1.7.0-RC1"]
                                  [org.clojure/clojurescript "0.0-3269"]
                                  [funcool/cljs-testrunners "0.1.0-SNAPSHOT"]]
                   :codeina {:sources ["src"]
                             :output-dir "doc/codeina"}
                   :plugins [[funcool/codeina "0.1.0-SNAPSHOT"
                              :exclusions [org.clojure/clojure]]
                             [lein-cljsbuild "1.0.4"]]}})
