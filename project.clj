(defproject cats "0.4.0-SNAPSHOT"
  :description "Category Theory abstractions for Clojure"
  :url "https://github.com/funcool/cats"
  :license {:name "BSD (2 Clause)"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2913"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]]
  :source-paths ["output/src" "src/clj"]

  :deploy-repositories {"releases" :clojars
                        "snapshots" :clojars}

  :release-tasks [["cljx" "once"]
                  ["deploy" "clojars"]]

  :plugins [[codox "0.8.10"]]
  :codox {:sources ["output/src"]
          :output-dir "doc/codox"}

  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store|user.clj"]

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "output/src"
                   :rules :clj}
                  {:source-paths ["src/cljx"]
                   :output-path "output/src"
                   :rules :cljs}
                  {:source-paths ["test"]
                   :output-path "output/test/clj"
                   :rules :clj}
                  {:source-paths ["test"]
                   :output-path "output/test/cljs"
                   :rules :cljs}]}

  :cljsbuild {:test-commands {"test" ["node" "output/tests.js"]}
              :builds [{:id "dev"
                        :source-paths ["output/test/cljs" "output/src"]
                        :notify-command ["node" "output/tests.js"]
                        :compiler {:output-to "output/tests.js"
                                   :output-dir "output/out"
                                   :source-map true
                                   :static-fns true
                                   :cache-analysis false
                                   :main cats.testrunner
                                   :optimizations :none
                                   :target :nodejs
                                   :pretty-print true}}]}

  :profiles
  {:dev {:dependencies [[org.clojure/tools.namespace "0.2.7"]
                        [funcool/cljs-testrunners "0.1.0-SNAPSHOT"]]
         :test-paths ["output/test/clj"]
         :plugins [[org.clojars.cemerick/cljx "0.6.0-SNAPSHOT" :exclusions [org.clojure/clojure]]
                   [lein-cljsbuild "1.0.4"]]}})

