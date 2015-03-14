(defproject cats "0.3.3"
  :description "Category Theory abstractions for Clojure"
  :url "https://github.com/funcool/cats"

  :license {:name "BSD (2 Clause)"
            :url "http://opensource.org/licenses/BSD-2-Clause"}

  :dependencies []
  :source-paths ["target/src" "src/clj"]

  :deploy-repositories {"releases" :clojars
                        "snapshots" :clojars}

  :release-tasks [["cljx" "once"]
                  ["deploy" "clojars"]]

  :plugins [[codox "0.8.10"]]
  :codox {:sources ["target/classes"]
          :output-dir "doc/codox"}

  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store|user.clj"]

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/src"
                   :rules :clj}
                  {:source-paths ["src/cljx"]
                   :output-path "target/src"
                   :rules :cljs}
                  {:source-paths ["spec"]
                   :output-path "target/spec/clj"
                   :rules :clj}
                  {:source-paths ["spec"]
                   :output-path "target/spec/cljs"
                   :rules :cljs}]}

  :cljsbuild {:test-commands {"spec" ["phantomjs"  "bin/speclj" "target/tests.js"]}
              :builds [{:id "dev"
                        :source-paths ["target/spec/cljs"
                                       "target/src"]
                        :notify-command ["phantomjs" "bin/speclj" "target/tests.js"]
                        :compiler {:output-to "target/tests.js"
                                   :optimizations :simple
                                   :pretty-print true}}
                       {:id "tests"
                        :source-paths ["target/spec/cljs"
                                       "target/src"]
                        :compiler {:output-to "target/tests.js"
                                   :optimizations :simple
                                   :pretty-print true}}]}

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.6.0"]
                                  [org.clojure/clojurescript "0.0-3058"]
                                  [speclj "3.1.0"]
                                  [com.keminglabs/cljx "0.5.0" :exclusions [org.clojure/clojure]]
                                  [org.clojure/tools.namespace "0.2.7"]]
                   :test-paths ["target/spec/clj"]
                   :plugins [[speclj "3.1.0"]
                             [com.keminglabs/cljx "0.5.0" :exclusions [org.clojure/clojure]]
                             [lein-cljsbuild "1.0.4"]]}})
