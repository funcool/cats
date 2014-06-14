(defproject cats "0.1.0-SNAPSHOT"
  :description "Category Theory abstractions for Clojure"
  :url "https://github.com/niwibe/cats"
  :license {:name "BSD (2 Clause)"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :source-paths ["target/classes"]
  :resource-paths ["resources"]
  :test-paths ["target/classes" "test"]
  :deploy-repositories {"releases" :clojars
                        "snapshots" :clojars}
  :profiles
  {:dev {:hooks [cljx.hooks
                 leiningen.cljsbuild]
         :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
         :dependencies [[org.clojure/clojurescript "0.0-2227"]]
         :plugins [[com.keminglabs/cljx "0.4.0"]
                   [lein-cljsbuild "1.0.3"]]
         :prep-tasks ["cljx" "javac" "compile"]
         :cljx {:builds [{:source-paths ["src/cljx"]
                          :output-path "target/classes"
                          :rules :clj}
                         {:source-paths ["src/cljx"]
                          :output-path "target/classes"
                          :rules :cljs}]}
         :cljsbuild {:builds [{:source-paths ["target/classes"]
                               :jar true}]}}})


