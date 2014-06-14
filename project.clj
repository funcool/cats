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
  :profiles
  {:dev {:hooks [cljx.hooks
                 leiningen.cljsbuild]
         :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
         :dependencies [[org.clojure/clojurescript "0.0-2227"]
                        [com.cemerick/clojurescript.test "0.3.1"]]
         :plugins [[com.keminglabs/cljx "0.4.0"]
                   [com.cemerick/clojurescript.test "0.3.1"]
                   [lein-cljsbuild "1.0.3"]]
         :prep-tasks ["cljx" "javac" "compile"]
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
                          :rules :cljs}]}
         :cljsbuild {:test-commands {"unit-tests" ["node" :node-runner
                                                   "target/tests.js"]}
                     :builds {:test {:source-paths ["target/testclasses"
                                                    "target/classes"]
                                     :compiler {:output-to "target/tests.js"
                                                :optimizations :simple
                                                :pretty-print true}}}}}})


