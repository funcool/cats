(defproject funcool/cats "0.5.0"
  :description "Category Theory abstractions for Clojure"
  :url "https://github.com/funcool/cats"
  :license {:name "BSD (2 Clause)"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.7.0" :scope "provided"]
                 [org.clojure/clojurescript "0.0-3308" :scope "provided"]]
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
                                   :main cats.runner
                                   :optimizations :none
                                   :target :nodejs
                                   :pretty-print true}}]}

  :jar-exclusions [#"\.swp|\.swo"]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]]
                   :codeina {:sources ["src"]
                             :reader :clojure
                             :target "doc/dist/latest/api"
                             :src-uri "http://github.com/funcool/cats/blob/master/"
                             :src-uri-prefix "#L"}
                   :plugins [[funcool/codeina "0.2.0"]
                             [lein-cljsbuild "1.0.6"]]}
             :bench [:dev {:dependencies [[criterium "0.4.3"]]
                           :main ^:skip-aot benchmarks
                           :jvm-opts ^:replace []
                           :source-paths ["dev"]}]})
