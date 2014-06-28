(defproject cats "0.1.0-SNAPSHOT"
  :description "Category Theory abstractions for Clojure"
  :url "https://github.com/niwibe/cats"
  :license {:name "BSD (2 Clause)"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :source-paths ["target/classes"]
  :test-paths ["target/testclasses"]
  :deploy-repositories {"releases" :clojars
                        "snapshots" :clojars}
  :release-tasks [["cljx" "once"]
                  ["deploy" "clojars"]]
  :cljsbuild {:test-commands {"unit-tests" ["node" :node-runner
                                            "target/tests.js"]}
              :builds {:test {:source-paths ["target/testclasses"
                                             "target/classes"]
                              :compiler {:output-to "target/tests.js"
                                         :optimizations :simple
                                         :pretty-print true}}}}
  :profiles
  {:common {:hooks [cljx.hooks]
            :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
            :dependencies [[org.clojure/clojurescript "0.0-2227"]
                           [org.clojure/tools.namespace "0.2.4"]]
            :plugins [[com.cemerick/clojurescript.test "0.3.1"]
                      [com.keminglabs/cljx "0.4.0" :exclusions [org.clojure/clojure]]
                      [lein-cljsbuild "1.0.3"]
                      [codox "0.8.9"]]
            :codox {:sources ["target/classes"]
                    :output-dir "doc/codox"}
            :cljx {:builds [{:source-paths ["src/cljx"]
                             :output-path "target/classes"
                             :rules :clj}
                            {:source-paths ["src/cljx"]
                             :output-path "target/classes"
                             :rules :cljs}
                            {:source-paths ["tests"]
                             :output-path "target/testclasses"
                             :rules :clj}
                            {:source-paths ["tests"]
                             :output-path "target/testclasses"
                             :rules :cljs}]}}
   :dev [:common {:injections [(use '[clojure.tools.namespace.repl :only (refresh)])]}]
   :test [:common {:hooks [cljx.hooks leiningen.cljsbuild]}]})


