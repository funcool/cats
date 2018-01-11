{:dev
 {:aliases {"test-all" ["with-profile" "dev,1.8:dev" "test"]}
  :dependencies [[org.clojure/tools.namespace "0.2.11"]
                 [criterium "0.4.3"]]
  :source-paths ["dev"]
  :codeina {:sources ["src"]
            :reader :clojure
            :target "doc/dist/latest/api"
            :src-uri "http://github.com/funcool/cats/blob/master/"
            :src-uri-prefix "#L"}
  :plugins [[funcool/codeina "0.5.0"]
            [lein-ancient "0.6.15" :exclusions [org.clojure/tools.reader]]]}
 :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
 :bench
 [:dev
  {:main ^:skip-aot benchmarks
   :jvm-opts ^:replace []}]}
